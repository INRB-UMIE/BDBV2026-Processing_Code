#!/usr/bin/env Rscript
# Step 1: Delay intervals and delay-distribution fitting for the epi linelist
# pipeline.
#   - Computes raw delay intervals (days) between key linelist dates (onset,
#     sample collection, notification, reporting, investigation, hospitalisation)
#   - Flags delays outside the plausible [min_days, max_days] window from
#     pipeline_configs.json (nowcasting.delay) -- informational only for now
#   - Fits lognormal/weibull/gamma delay distributions (epidist/primarycensored/
#     cmdstanr) for every onset-anchored delay, separately per case group
#     (pipeline_configs.json delay_distribution_fit.case_groups -- e.g. cases =
#     confirmed + probable vs not_a_case), and records which family fits best
#     per group x delay (delay_distribution_fit_summary.csv, case_group column;
#     fits under delay_distributions/<group>/<delay>/)
#   - Writes dhis2_linelist_with_delays.csv, which 2-date_imputation.R,
#     7-nowcasting_prep.R (and any later step) reads instead of the raw
#     ctsafe linelist
#
# Date imputation (using these fitted distributions to impute missing onset
# dates) is a separate step -- see 2-date_imputation.R -- so this script's
# job stops at fitting the distributions and writing the delays-augmented
# linelist; it doesn't impute anything itself.
#
# The delay-interval logic was originally moved out of 7-nowcasting_prep.R
# (then 3-nowcasting_prep.R) verbatim (same columns, same bounds-check
# messages) so that script no longer computes delays itself.

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

sandbox_mode <- F

# --------------- Determine which processed dir to handle ----------------------
# resolve_epi_latest_dir() returns the single latest synced date folder.
target_epi_dirs <- resolve_epi_latest_dir()

#------------------- Load Config for Epi Pipeline ------------------------------
cfg <- pipeline_configs$epi_linelist
cfg$version <- pipeline_configs$version

DATE_COLUMNS <- cfg$qc$date_columns
DELAY_MIN_DAYS <- as.integer(cfg$nowcasting$delay$min_days)
DELAY_MAX_DAYS <- as.integer(cfg$nowcasting$delay$max_days)

# ----------------------------- Load Linelist ----------------------------------
message("======== Delays & Imputation: ", basename(target_epi_dirs), " =========")
message("")
message("A. LOADING LINELIST FILE")
input_csv <- file.path(target_epi_dirs, cfg$data$files$processed_linelist)
df <- load_linelist_properly(input_csv, date_columns = DATE_COLUMNS)
message("  Loaded linelist file,  ", nrow(df), " rows")

missing_date_cols <- setdiff(DATE_COLUMNS, names(df))
if (length(missing_date_cols)) {
  message(
    "  NOTE: expected date columns not present in this sync (any delay ",
    "involving them will be NA): ", paste(missing_date_cols, collapse = ", ")
  )
}

# --------------------- Calculate Delay Intervals ------------------------------
message("")
message("B. CALCULATING DELAYS AND FLAGGING SUSPICIOUS DATES")

# Guard each pair so a schema change upstream (a source date column dropped
# from this sync, as happened to the hospitalisation fields) makes that delay
# column NA instead of erroring out the whole script.
has_cols <- function(...) all(c(...) %in% names(df))

df <- df |>
  dplyr::mutate(
    onset_to_sample_days = if (has_cols("date_of_sample_collection", "date_of_symptom_onset"))
      as.integer(date_of_sample_collection - date_of_symptom_onset) else NA_integer_,
    onset_to_notification_days = if (has_cols("date_of_notification", "date_of_symptom_onset"))
      as.integer(date_of_notification - date_of_symptom_onset) else NA_integer_,
    onset_to_investigation_days = if (has_cols("investigation_datetime", "date_of_symptom_onset"))
      as.integer(investigation_datetime - date_of_symptom_onset) else NA_integer_,
    onset_to_reporting_days = if (has_cols("reporting_date", "date_of_symptom_onset"))
      as.integer(reporting_date - date_of_symptom_onset) else NA_integer_,
    onset_to_hospitalisation_start_days = if (has_cols("date_of_hospitalisation_start", "date_of_symptom_onset"))
      as.integer(date_of_hospitalisation_start - date_of_symptom_onset) else NA_integer_,
    onset_to_hospitalisation_end_days = if (has_cols("date_of_hospitalisation_end", "date_of_symptom_onset"))
      as.integer(date_of_hospitalisation_end - date_of_symptom_onset) else NA_integer_,
    notification_to_sample_days = if (has_cols("date_of_sample_collection", "date_of_notification"))
      as.integer(date_of_sample_collection - date_of_notification) else NA_integer_,
    hospitalisation_start_to_end_days = if (has_cols("date_of_hospitalisation_end", "date_of_hospitalisation_start"))
      as.integer(date_of_hospitalisation_end - date_of_hospitalisation_start) else NA_integer_
  )

delay_cols <- c(
  "onset_to_sample_days",
  "onset_to_notification_days",
  "onset_to_investigation_days",
  "onset_to_reporting_days",
  "onset_to_hospitalisation_start_days",
  "onset_to_hospitalisation_end_days",
  "notification_to_sample_days",
  "hospitalisation_start_to_end_days"
)
delay_cols <- intersect(delay_cols, names(df))

# Per-row binary flag for each delay column, kept in the output for future
# reference (e.g. filtering/QC downstream without redoing the bounds check).
# NA where the delay itself is NA (nothing to assess), "in_range"/
# "out_of_range" otherwise.
df <- df |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(delay_cols),
      \(x) dplyr::case_when(
        is.na(x) ~ NA_character_,
        x < DELAY_MIN_DAYS | x > DELAY_MAX_DAYS ~ "out_of_range",
        TRUE ~ "in_range"
      ),
      .names = "{.col}_range_flag"
    )
  )

# One row per delay column: n observed, how many/what share fall outside the
# plausible window, median/mean over everything observed, and a "backup"
# median/mean computed excluding the out-of-range values (NA/NaN if none are
# in range).
delay_summary <- df |>
  dplyr::select(dplyr::all_of(delay_cols)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "delay", values_to = "days") |>
  dplyr::filter(!is.na(days)) |>
  dplyr::group_by(delay) |>
  dplyr::summarise(
    n_observed = dplyr::n(),
    n_out_of_range = sum(days < DELAY_MIN_DAYS | days > DELAY_MAX_DAYS),
    prop_out_of_range = round(sum(days < DELAY_MIN_DAYS | days > DELAY_MAX_DAYS) / dplyr::n(), 3),
    median_days = median(days),
    mean_days = round(mean(days), 1),
    median_days_in_range = median(days[days >= DELAY_MIN_DAYS & days <= DELAY_MAX_DAYS]),
    mean_days_in_range = round(mean(days[days >= DELAY_MIN_DAYS & days <= DELAY_MAX_DAYS]), 1),
    .groups = "drop"
  )

message("  Summary:")
purrr::pwalk(delay_summary, function(delay, n_observed, n_out_of_range, prop_out_of_range,
                                      median_days, mean_days,
                                      median_days_in_range, mean_days_in_range) {
  message(
    "  ", delay, ": n=", n_observed, " median=", median_days, " mean=", mean_days,
    if (n_out_of_range > 0) {
      paste0(
        " (", n_out_of_range, " outside [", DELAY_MIN_DAYS, ", ", DELAY_MAX_DAYS, "] days, ",
        prop_out_of_range, "; excluding those: median=", median_days_in_range,
        " mean=", mean_days_in_range, ")"
      )
    } else {
      ""
    }
  )
})

delay_summary_exclude_pre_june14 <- df |>
  dplyr::filter(reporting_date > as.Date(cfg$delay_distribution_fit$onset_cutoff_start)) |>
  dplyr::select(dplyr::all_of(delay_cols)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "delay", values_to = "days") |>
  dplyr::filter(!is.na(days)) |>
  dplyr::group_by(delay) |>
  dplyr::summarise(
    n_observed = dplyr::n(),
    n_out_of_range = sum(days < DELAY_MIN_DAYS | days > DELAY_MAX_DAYS),
    prop_out_of_range = round(sum(days < DELAY_MIN_DAYS | days > DELAY_MAX_DAYS) / dplyr::n(), 3),
    median_days = median(days),
    mean_days = round(mean(days), 1),
    median_days_in_range = median(days[days >= DELAY_MIN_DAYS & days <= DELAY_MAX_DAYS]),
    mean_days_in_range = round(mean(days[days >= DELAY_MIN_DAYS & days <= DELAY_MAX_DAYS]), 1),
    .groups = "drop"
  )

message("  Summary (Excluding pre June 14th:")
purrr::pwalk(delay_summary_exclude_pre_june14, function(delay, n_observed, n_out_of_range, prop_out_of_range,
                                     median_days, mean_days,
                                     median_days_in_range, mean_days_in_range) {
  message(
    "  ", delay, ": n=", n_observed, " median=", median_days, " mean=", mean_days,
    if (n_out_of_range > 0) {
      paste0(
        " (", n_out_of_range, " outside [", DELAY_MIN_DAYS, ", ", DELAY_MAX_DAYS, "] days, ",
        prop_out_of_range, "; excluding those: median=", median_days_in_range,
        " mean=", mean_days_in_range, ")"
      )
    } else {
      ""
    }
  )
})

# --------------------- Fit Delay Distributions (all onset-anchored delays) ----
# Ported from an earlier interactive exploration (found in
# BDBV2026-Secure_Raw_Data/.Rhistory, never previously saved as a script), then
# generalised: for every delay column anchored on date_of_symptom_onset, fit
# three candidate families (lognormal, weibull, gamma) via epidist/
# primarycensored (properly accounting for interval censoring), using brms +
# cmdstanr as the Stan backend, then compare the three with LOO
# (loo::loo_compare) and record which family fits best.
#
# Scope: onset-anchored delays only (not notification_to_sample_days or
# hospitalisation_start_to_end_days, which don't start from onset) -- a
# deliberate choice to bound compute cost (each delay x family is a full Stan
# fit: up to 6 delays x 3 families = 18 fits/run, fewer while the
# hospitalisation columns stay absent from the sync).
#
# NOTE: this is a genuinely heavy step (Stan compilation + 4-chain MCMC per
# fit, cmdstanr/CmdStan not installed anywhere here yet) -- FIT_DELAY_DISTRIBUTIONS
# is an escape hatch so it can be disabled per-environment (e.g. locally, or
# until CI has the Stan toolchain wired up) without touching this script.
message("")
message("C. FITTING DELAY DISTRIBUTIONS (lognormal / weibull / gamma, per onset-anchored delay)")

FIT_DELAY_DISTRIBUTIONS <- toupper(Sys.getenv("FIT_DELAY_DISTRIBUTIONS", "TRUE")) != "FALSE"

if (!FIT_DELAY_DISTRIBUTIONS) {

  message("  Skipping (FIT_DELAY_DISTRIBUTIONS=FALSE)")

} else {

  # Null-coalescing operator: defined locally so this script doesn't depend on
  # whichever loaded package (purrr/rlang/base R >= 4.4) happens to export it.
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # Excludes a known onset-date data-quality issue prior to this date (see
  # pipeline_configs.json's epi_linelist.delay_distribution_fit._comment) --
  # confirm/update onset_cutoff_start there periodically.
  ONSET_FIT_CUTOFF <- as.Date(cfg$delay_distribution_fit$onset_cutoff_start)
  MIN_ROWS_FOR_FIT <- as.integer(cfg$delay_distribution_fit$min_rows_for_fit)

  # delay column -> its non-onset endpoint (pdate_lwr is always onset here).
  # Only delays actually present this run (delay_cols, computed above) are
  # attempted -- e.g. the hospitalisation ones are skipped while those source
  # columns stay absent from the sync.
  onset_delay_targets <- list(
    onset_to_sample_days = "date_of_sample_collection",
    onset_to_notification_days = "date_of_notification",
    onset_to_investigation_days = "investigation_datetime",
    onset_to_reporting_days = "reporting_date",
    onset_to_hospitalisation_start_days = "date_of_hospitalisation_start",
    onset_to_hospitalisation_end_days = "date_of_hospitalisation_end"
  )
  onset_delay_targets <- onset_delay_targets[intersect(names(onset_delay_targets), delay_cols)]
  # Drop targets whose source date column is absent from this sync: the delay
  # column itself still exists (as all-NA, see section B), but the fit and the
  # first_seen check below reference the source date column directly.
  onset_delay_targets <- Filter(function(col) col %in% names(df), onset_delay_targets)

  # family = family object to fit under the same mu ~ 1 marginal model.
  # lognormal/weibull are brms-specific families; plain Gamma isn't
  # (brms accepts the base R stats::Gamma() family object directly rather
  # than exporting its own -- brms::Gamma() doesn't exist, which is what
  # threw "'Gamma' is not an exported object from 'namespace:brms'").
  # Gamma needs an explicit log link so its location parameter is comparable
  # to lognormal/weibull's (both naturally log-scale).
  delay_fit_families <- list(
    lognormal = brms::lognormal(),
    weibull = brms::weibull(),
    gamma = stats::Gamma(link = "log")
  )

  # chains stays fixed at 4 regardless of hardware -- that's a statistical
  # choice (enough independent chains for Rhat/ESS diagnostics to mean
  # anything), not a hardware one. cores adapts to N_CORES_AVAILABLE (set in
  # 0-setup.R -- 2 on a standard GitHub Actions runner, however many a local
  # machine actually has) so it doesn't oversubscribe small environments;
  # capped at FIT_CHAINS since requesting more cores than chains doesn't
  # parallelize the fit any further.
  FIT_CHAINS <- 4L
  FIT_CORES <- min(N_CORES_AVAILABLE, FIT_CHAINS)

  fit_one_family <- function(data, sdate_col, family, seed) {
    data |>
      epidist::as_epidist_linelist_data(
        pdate_lwr = "date_of_symptom_onset",
        sdate_lwr = sdate_col
      ) |>
      epidist::as_epidist_marginal_model() |>
      epidist::epidist(
        formula = mu ~ 1,
        family = family,
        chains = FIT_CHAINS,
        cores = FIT_CORES,
        iter = 2000,
        backend = "cmdstanr",
        seed = seed
      )
  }

  # Checks for (and tries to actually load, not just find) a previously-saved
  # .rds before redoing the work that produced it -- used for both the fit
  # itself and its loo() result, so a rerun with nothing new to compute (same
  # date/delay/family) skips straight past both the expensive Stan fit and
  # the also-expensive loo() call. If the saved .rds can't be read (e.g. an
  # interrupted/corrupt write -- see the "lazy-load database is corrupt"
  # issue from primarycensored, a similar failure mode could in principle hit
  # a saved .rds too), it's treated as absent and recomputed rather than
  # erroring out.
  load_cached_rds <- function(path, label) {
    if (!file.exists(path)) {
      return(NULL)
    }
    tryCatch(readRDS(path), error = function(e) {
      message("    cached ", label, " at ", path, " could not be read (", conditionMessage(e), ") -- recomputing.")
      NULL
    })
  }

  # ------------------- Per-delay distribution fit summary ---------------------
  # Converts each family's fitted (native) parameters into derived, days-scale
  # quantities -- mean/median/sd/95% interval of the delay itself -- rather
  # than reporting the raw model parameters directly. The three families don't
  # share a parameterization (lognormal's Intercept is meanlog, not the mean;
  # weibull/gamma's Intercept, after their log link, already IS the mean), so
  # raw parameters aren't comparable side by side. Derived quantities are, and
  # each is recoverable back to the family's native parameters if ever needed
  # (closed-form for lognormal/gamma; a 1-line uniroot() for weibull's shape).
  #
  # Parameter extraction itself (get_param_estimate/epidist_extract_family_params/
  # epidist_family_rq_params) lives in 0-setup.R, shared with
  # 2-date_imputation.R, which needs the same mu/shape values to sample
  # imputed onset dates from the same fitted distributions.
  extract_delay_distribution_summary <- function(fit, loo_result, family_name) {
    params <- epidist_extract_family_params(fit, family_name)
    if (is.null(params)) {
      message(
        "    Could not locate expected posterior parameters for family ", family_name,
        " -- skipping its row in the distribution summary."
      )
      return(NULL)
    }
    mu <- params$mu
    disp <- params$disp
    rq <- epidist_family_rq_params(family_name, mu, disp)

    if (family_name == "lognormal") {
      sdlog <- disp
      mean_days   <- exp(mu + sdlog^2 / 2)
      median_days <- exp(mu)
      sd_days     <- sqrt((exp(sdlog^2) - 1) * exp(2 * mu + sdlog^2))
      q2.5_days   <- stats::qlnorm(0.025, meanlog = rq$meanlog, sdlog = rq$sdlog)
      q97.5_days  <- stats::qlnorm(0.975, meanlog = rq$meanlog, sdlog = rq$sdlog)
    } else if (family_name == "weibull") {
      shape <- disp
      # Computed via lgamma() (log-space) throughout, not raw gamma(). Delay
      # data with many near-zero delays and a long tail fits a small shape
      # (<1) -- a real, plausible result, not a data error -- and 1+1/shape
      # or 1+2/shape can then be large enough that gamma() individually
      # overflows toward double precision's ~1.8e308 ceiling, or both terms
      # are merely astronomical and nearly equal, so subtracting them (the
      # variance formula) is almost pure floating-point noise rather than the
      # real value -- this is what produced the wildly inconsistent, garbage
      # SDs (e.g. one delay showing sd_days in the millions). lgamma() grows
      # only logarithmically, so working in log-space avoids that overflow/
      # cancellation entirely and gives the true value even when shape is small.
      log_gamma_1 <- lgamma(1 + 1 / shape)
      log_gamma_2 <- lgamma(1 + 2 / shape)
      mean_days   <- mu
      median_days <- exp(log(rq$scale) + log(log(2)) / shape)
      # Var/mean^2 = Gamma(1+2/k)/Gamma(1+1/k)^2 - 1; expm1() keeps precision
      # when that ratio is close to 1 (large shape, low variance) and still
      # returns a clean Inf (rather than NaN from Inf - Inf) if the true
      # variance genuinely exceeds double precision's representable range.
      cv_squared <- expm1(log_gamma_2 - 2 * log_gamma_1)
      sd_days <- if (is.finite(cv_squared) && cv_squared >= 0) mu * sqrt(cv_squared) else NA_real_
      q2.5_days   <- stats::qweibull(0.025, shape = rq$shape, scale = rq$scale)
      q97.5_days  <- stats::qweibull(0.975, shape = rq$shape, scale = rq$scale)
    } else if (family_name == "gamma") {
      shape <- disp
      mean_days   <- mu
      median_days <- stats::qgamma(0.5, shape = rq$shape, rate = rq$rate)
      sd_days     <- mu / sqrt(shape)
      q2.5_days   <- stats::qgamma(0.025, shape = rq$shape, rate = rq$rate)
      q97.5_days  <- stats::qgamma(0.975, shape = rq$shape, rate = rq$rate)
    } else {
      stop("Unknown family: ", family_name, call. = FALSE)
    }

    # Rhat / ESS pulled from the same summary() object already written to
    # <family>_summary.txt, rather than a separate diagnostics call, so this
    # can't disagree with what's on disk. Divergences come from nuts_params()
    # (works across both rstan and cmdstanr backends); best-effort, since it's
    # a secondary diagnostic -- failure here shouldn't drop the whole row.
    conv <- tryCatch({
      # posterior::summarise_draws (not brms::summary) keeps this fork/
      # serialization-safe: brms::summary()'s rstan S4 dispatch fails on fits
      # created in multisession workers ("class_model_base ... attempt to apply
      # non-function"), whereas posterior works straight off the draws.
      sd <- posterior::summarise_draws(posterior::as_draws(fit), "rhat", "ess_bulk")
      list(
        max_rhat = suppressWarnings(max(sd$rhat, na.rm = TRUE)),
        min_ess_bulk = suppressWarnings(min(sd$ess_bulk, na.rm = TRUE))
      )
    }, error = function(e) list(max_rhat = NA_real_, min_ess_bulk = NA_real_))

    n_divergent <- tryCatch({
      np <- brms::nuts_params(fit)
      sum(np$Parameter == "divergent__" & np$Value > 0)
    }, error = function(e) NA_integer_)

    loo_est <- if (!is.null(loo_result)) tryCatch(loo_result$estimates, error = function(e) NULL) else NULL
    pareto_k <- if (!is.null(loo_result)) tryCatch(loo_result$diagnostics$pareto_k, error = function(e) NULL) else NULL

    tibble::tibble(
      family = family_name,
      mean_days = round(mean_days, 2),
      median_days = round(median_days, 2),
      sd_days = round(sd_days, 2),
      q2.5_days = round(q2.5_days, 2),
      q97.5_days = round(q97.5_days, 2),
      elpd_loo = if (!is.null(loo_est)) unname(loo_est["elpd_loo", "Estimate"]) else NA_real_,
      se_elpd_loo = if (!is.null(loo_est)) unname(loo_est["elpd_loo", "SE"]) else NA_real_,
      looic = if (!is.null(loo_est)) unname(loo_est["looic", "Estimate"]) else NA_real_,
      n_pareto_k_high = if (!is.null(pareto_k)) sum(pareto_k > 0.7, na.rm = TRUE) else NA_integer_,
      max_rhat = round(conv$max_rhat, 3),
      min_ess_bulk = round(conv$min_ess_bulk, 0),
      n_divergent = n_divergent
    )
  }

  FIT_WORKERS <- as.integer(cfg$delay_distribution_fit$fit_workers %||% 12)
  if (is.na(FIT_WORKERS) || FIT_WORKERS < 1L) FIT_WORKERS <- 1L  # guard bad config before future::plan()
  LOO_CORES   <- max(1L, N_CORES_AVAILABLE %/% FIT_WORKERS)

  # Case groups (pipeline_configs.json delay_distribution_fit.case_groups):
  # group name -> the final_mve_case_classification values it covers. Each
  # group gets its own full set of fits, so e.g. cases (confirmed + probable)
  # and not-a-case records get separate delay distributions rather than one
  # pooled fit. Outputs go to delay_distributions/<group>/<delay>/.
  CASE_GROUPS <- lapply(cfg$delay_distribution_fit$case_groups, as.character)
  if (!length(CASE_GROUPS)) {
    stop("pipeline_configs.json delay_distribution_fit.case_groups is empty.", call. = FALSE)
  }

  # --- Build the flat job list: one job per (case group) x (present delay) x (family) ---
  # Per-group/delay eligible sets are computed once here, keyed "<group>|<delay>".
  eligible_by_delay <- list()
  jobs <- list()
  for (case_group in names(CASE_GROUPS)) {
  for (delay_name in names(onset_delay_targets)) {
    sdate_col <- onset_delay_targets[[delay_name]]
    group_key <- paste(case_group, delay_name, sep = "|")
    delay_dist_dir <- file.path(
      data_root, "outputs", basename(target_epi_dirs), "delay_distributions",
      case_group, delay_name
    )
    dir.create(delay_dist_dir, recursive = TRUE, showWarnings = FALSE)

    eligible <- df |>
      dplyr::filter(final_mve_case_classification %in% CASE_GROUPS[[case_group]]) |>
      dplyr::filter(date_of_symptom_onset > ONSET_FIT_CUTOFF, date_of_symptom_onset < Sys.Date()) |>
      dplyr::filter(!is.na(.data[[delay_name]]), .data[[delay_name]] >= 0)

    if ("first_seen_date" %in% names(eligible)) {
      suspicious <- eligible |>
        dplyr::summarise(
          any_first_seen_before_secondary = any(first_seen_date < .data[[sdate_col]], na.rm = TRUE),
          any_first_seen_before_onset = any(first_seen_date < date_of_symptom_onset, na.rm = TRUE)
        )
      if (any(suspicious)) {
        message(
          "    WARNING (", case_group, "): at least one row has a ", sdate_col,
          "/onset date later than it was first seen in the linelist!"
        )
      }
    }

    if (nrow(eligible) < MIN_ROWS_FOR_FIT) {
      message("    Skipping ", case_group, " / ", delay_name, ": only ", nrow(eligible),
              " eligible rows (need ", MIN_ROWS_FOR_FIT, ")")
      next
    }
    message("    ", case_group, " / ", delay_name, ": ", nrow(eligible), " eligible rows")
    eligible_by_delay[[group_key]] <- eligible
    for (family_name in names(delay_fit_families)) {
      jobs[[length(jobs) + 1L]] <- list(
        case_group = case_group, group_key = group_key,
        delay_name = delay_name, sdate_col = sdate_col,
        family_name = family_name, dir = delay_dist_dir
      )
    }
  }
  }

  # --- Phase 1+2 (parallel): fit (or load cache) + parallel_loo per job ---
  future::plan(future::multisession, workers = FIT_WORKERS)

  compute_job <- function(job) {
    cmdstan_dir <- Sys.getenv("CMDSTAN")
    if (nzchar(cmdstan_dir)) try(cmdstanr::set_cmdstan_path(cmdstan_dir), silent = TRUE)

    fit_path <- file.path(job$dir, paste0(job$family_name, "_fit.rds"))
    fit <- load_cached_rds(fit_path, "fit")
    fit_error <- NA_character_
    if (is.null(fit)) {
      fit <- tryCatch(
        fit_one_family(
          eligible_by_delay[[job$group_key]], job$sdate_col,
          delay_fit_families[[job$family_name]],
          seed = epidist_fit_seed(job$group_key, job$family_name)
        ),
        error = function(e) { fit_error <<- conditionMessage(e); NULL })
    }
    if (is.null(fit))
      return(c(job, list(fit = NULL, loo = NULL, fit_error = fit_error, loo_error = NA_character_)))

    loo_path <- file.path(job$dir, paste0(job$family_name, "_loo.rds"))
    loo_res <- load_cached_rds(loo_path, "loo")
    loo_error <- NA_character_
    if (is.null(loo_res)) {
      loo_res <- tryCatch(parallel_loo(fit, cores = LOO_CORES),
        error = function(e) { loo_error <<- conditionMessage(e); NULL })
    }
    c(job, list(fit = fit, loo = loo_res, fit_error = fit_error, loo_error = loo_error))
  }

  results <- furrr::future_map(jobs, compute_job,
    .options = furrr::furrr_options(seed = TRUE,
      globals = c("eligible_by_delay", "delay_fit_families", "LOO_CORES",
                  "fit_one_family", "parallel_loo", "epidist_fit_seed",
                  "load_cached_rds", "FIT_CHAINS", "FIT_CORES")))

  message("  Fit/loo outcomes:")
  for (r in results) {
    status <- if (is.null(r$fit)) paste0("FIT FAILED (", r$fit_error, ")")
              else if (is.null(r$loo)) paste0("loo failed (", r$loo_error, ")")
              else "ok"
    message("    ", r$case_group, " / ", r$delay_name, " / ", r$family_name, ": ", status)
  }

  # --- Phase 3 (sequential): write outputs + compare families per group x delay ---
  delay_distribution_summary_rows <- list()
  by_delay <- split(results, vapply(results, function(r) r$group_key, character(1)))

  for (group_key in names(by_delay)) {
    delay_results <- Filter(function(r) !is.null(r$fit), by_delay[[group_key]])
    if (!length(delay_results)) {
      message("    All family fits failed for ", group_key, " -- nothing to save.")
      next
    }
    case_group <- delay_results[[1]]$case_group
    delay_name <- delay_results[[1]]$delay_name
    delay_dist_dir <- delay_results[[1]]$dir

    fits <- setNames(lapply(delay_results, `[[`, "fit"),
                     vapply(delay_results, `[[`, character(1), "family_name"))
    loos <- setNames(lapply(delay_results, `[[`, "loo"),
                     vapply(delay_results, `[[`, character(1), "family_name"))

    for (family_name in names(fits)) {
      saveRDS(fits[[family_name]], file.path(delay_dist_dir, paste0(family_name, "_fit.rds")))
      # posterior::summarise_draws instead of print(summary(fit)): brms::summary
      # reloads rstan via S4 dispatch and fails on worker-created fits. Wrapped so
      # a diagnostic-write failure never aborts the batch.
      tryCatch(
        writeLines(
          utils::capture.output(print(posterior::summarise_draws(posterior::as_draws(fits[[family_name]])))),
          file.path(delay_dist_dir, paste0(family_name, "_summary.txt"))),
        error = function(e) message("    ", family_name, " summary write failed: ", conditionMessage(e)))
      pp <- tryCatch(brms::pp_check(fits[[family_name]], ndraws = 100), error = function(e) NULL)
      if (!is.null(pp)) ggplot2::ggsave(
        file.path(delay_dist_dir, paste0(family_name, "_pp_check.png")), pp, width = 7, height = 5)
      if (!is.null(loos[[family_name]]))
        saveRDS(loos[[family_name]], file.path(delay_dist_dir, paste0(family_name, "_loo.rds")))
    }

    best_family <- NA_character_
    comparison <- NULL
    loos <- loos[!vapply(loos, is.null, logical(1))]
    if (length(loos) >= 2) {
      comparison <- tryCatch(loo::loo_compare(x = loos), error = function(e) {
        message("    loo_compare() failed: ", conditionMessage(e)); NULL })
      if (!is.null(comparison)) {
        best_family <- rownames(comparison)[1]
        message("    Best fit by LOO for ", case_group, " / ", delay_name, ": ", best_family)
        writeLines(c(paste("Best family:", best_family), "",
                     utils::capture.output(print(comparison))),
                   file.path(delay_dist_dir, "loo_compare.txt"))
        saveRDS(comparison, file.path(delay_dist_dir, "loo_compare.rds"))
      }
    } else if (length(loos) == 1) {
      best_family <- names(loos)
    }

    family_metrics <- Filter(Negate(is.null), lapply(names(fits), function(fam) {
      extract_delay_distribution_summary(fits[[fam]], loos[[fam]], fam) }))
    if (length(family_metrics)) {
      fmdf <- dplyr::bind_rows(family_metrics)
      elpd_diff_lookup <- setNames(rep(NA_real_, nrow(fmdf)), fmdf$family)
      se_diff_lookup <- elpd_diff_lookup
      if (!is.null(comparison)) {
        elpd_diff_lookup[rownames(comparison)] <- comparison[, "elpd_diff"]
        se_diff_lookup[rownames(comparison)] <- comparison[, "se_diff"]
      }
      delay_distribution_summary_rows[[group_key]] <- fmdf |>
        dplyr::mutate(
          case_group = case_group,
          delay = delay_name,
          n_eligible = nrow(eligible_by_delay[[group_key]]),
          is_best = family == best_family,
          elpd_diff = elpd_diff_lookup[family],
          se_diff = se_diff_lookup[family]) |>
        dplyr::relocate(case_group, delay, family, n_eligible, is_best, .before = 1) |>
        dplyr::relocate(elpd_diff, se_diff, .after = looic)
    }
    message("    Wrote ", case_group, " / ", delay_name, " outputs to ", delay_dist_dir)
  }

  future::plan(future::sequential)

  if (length(delay_distribution_summary_rows)) {
    delay_distribution_summary <- dplyr::bind_rows(delay_distribution_summary_rows)
    summary_csv <- file.path(
      data_root, "outputs", basename(target_epi_dirs), "delay_distributions",
      "delay_distribution_fit_summary.csv"
    )
    write.csv(delay_distribution_summary, summary_csv, row.names = FALSE, na = "")
    message("")
    message("  Wrote delay distribution fit summary: ", summary_csv)
  } else {
    message("")
    message("  No delay distribution fits succeeded this run -- summary file not written.")
  }
}

# ----------------------------- Write output -----------------------------------
# Note: date columns are now real Date objects (via load_linelist_properly),
# so they'll be written out here in normalized ISO (YYYY-MM-DD) format, even
# where the raw ctsafe export had mixed formats.
message("")
message("D. WRITING OUTPUT FILES")

# outputs/<DATE>/ is the default destination for everything this pipeline
# produces -- only the upstream-synced ctsafe linelist stays under
# data/<DATE>/. dir.create() here since, unlike delay_distributions/, this
# top-level outputs/<DATE>/ dir may not exist yet (e.g. FIT_DELAY_DISTRIBUTIONS=FALSE).
epi_outputs_dir <- file.path(data_root, "outputs", basename(target_epi_dirs))
dir.create(epi_outputs_dir, recursive = TRUE, showWarnings = FALSE)
output_csv <- file.path(epi_outputs_dir, "/delay_distributions/", cfg$data$files$linelist_with_delays)
write.table(
  df,
  output_csv,
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  na = "",
  qmethod = "double"
)
message("  Wrote: ", output_csv)

# --------------------------------- Cleanup ------------------------------------
# Kept under a distinct name (not "processed_dhis2_linelist")
processed_dhis2_linelist_with_delays <- df
fit_delay_distributions_complete <- TRUE

keep_objects <- c(keep_objects, "processed_dhis2_linelist_with_delays", "fit_delay_distributions_complete")
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
