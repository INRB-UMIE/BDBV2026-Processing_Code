#!/usr/bin/env Rscript
# Step 2: Date imputation for the epi linelist pipeline.
#   - Identifies onset dates that are missing, or flagged out_of_range by
#     1-fit_delay_distributions.R's onset_to_reporting_days_range_flag
#   - For rows with a usable reporting_date, imputes a replacement onset date
#     by drawing a random delay from the best-fit onset_to_reporting_days
#     distribution for the configured case group (date_imputation.case_group,
#     default "cases" = confirmed + probable; per
#     delay_distribution_fit_summary.csv) and subtracting
#     it from reporting_date -- a stochastic draw rather than a fixed
#     point-estimate offset, so imputed onset dates don't all cluster at one
#     constant gap behind reporting_date (which would distort an onset-date
#     epi curve). Draws that fall outside the plausible [min_days, max_days]
#     delay window are re-sampled (capped at max_resample_attempts), then
#     clamped to that window as a last resort.
#   - Also records a [lower, upper] onset-date interval per imputed row, from
#     the fitted distribution's own quantiles (pipeline_configs.json's
#     date_imputation.interval_width, default 95%) -- one fixed, fit-implied
#     interval shared by every imputed row, not a resampled range.
#   - random_seed (pipeline_configs.json) is fixed so reruns against
#     unchanged data reproduce the same imputed dates rather than reshuffling
#     them every run.
#   - Writes dhis2_linelist_with_imputed_onset.csv. The original
#     date_of_symptom_onset column is left untouched; a new
#     date_of_symptom_onset_imputed column is what downstream steps should
#     use as the analysis-ready onset date.
#
# NOT YET IMPLEMENTED: this only imputes onset dates. Other missing/
# suspicious dates (notification, investigation, hospitalisation, etc.)
# aren't in scope here.

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

# --------------- Determine which processed dir to handle ----------------------
# resolve_epi_latest_dir() returns the single latest synced date folder.
target_epi_dirs <- resolve_epi_latest_dir()

#------------------- Load Config for Epi Pipeline ------------------------------
cfg <- pipeline_configs$epi_linelist
cfg$version <- pipeline_configs$version

DATE_COLUMNS <- cfg$qc$date_columns
DELAY_MIN_DAYS <- as.integer(cfg$nowcasting$delay$min_days)
DELAY_MAX_DAYS <- as.integer(cfg$nowcasting$delay$max_days)
IMPUTATION_INTERVAL_WIDTH <- cfg$date_imputation$interval_width
MAX_RESAMPLE_ATTEMPTS <- as.integer(cfg$date_imputation$max_resample_attempts)
IMPUTATION_SEED <- as.integer(cfg$date_imputation$random_seed)
# Which case group's fit (1-fit_delay_distributions.R case_groups) to impute from.
IMPUTATION_CASE_GROUP <- cfg$date_imputation$case_group
if (is.null(IMPUTATION_CASE_GROUP) || !nzchar(IMPUTATION_CASE_GROUP)) IMPUTATION_CASE_GROUP <- "cases"

# outputs/<DATE>/ is the default destination for everything this pipeline
# produces -- only the upstream-synced ctsafe linelist stays under data/<DATE>/.
epi_outputs_dir <- file.path(data_root, "outputs", basename(target_epi_dirs))
dir.create(epi_outputs_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------- Load Linelist ----------------------------------
message("======== Date Imputation: ", basename(target_epi_dirs), " =========")
message("")
message("A. LOADING LINELIST WITH DELAYS")

if (exists("processed_dhis2_linelist_with_delays", inherits = TRUE)) {
  df <- get("processed_dhis2_linelist_with_delays", inherits = TRUE)
  message("  Using in-memory linelist from 1-fit_delay_distributions.R, ", nrow(df), " rows")
} else {
  input_csv <- file.path(epi_outputs_dir, "/delay_distributions/", cfg$data$files$linelist_with_delays)
  if (!file.exists(input_csv)) {
    stop(
      "Linelist with delays not found: ", input_csv,
      ". Run 1-fit_delay_distributions.R first.", call. = FALSE
    )
  }
  df <- load_linelist_properly(input_csv, date_columns = DATE_COLUMNS)
  message("  Loaded linelist file, ", nrow(df), " rows")
}

has_cols <- function(...) all(c(...) %in% names(df))

if (!has_cols("date_of_symptom_onset")) {
  stop("date_of_symptom_onset column missing -- cannot proceed with date imputation.", call. = FALSE)
}

# --------------------- Identify Rows Needing Imputation ------------------------
message("")
message("B. IDENTIFYING ROWS NEEDING ONSET DATE IMPUTATION")

# Guarded the same way 1-fit_delay_distributions.R guards missing source
# columns: a schema change (range-flag or reporting_date column absent from
# this sync) degrades to "nothing flagged" rather than erroring the script.
range_flag_col <- "onset_to_reporting_days_range_flag"
df <- df |>
  dplyr::mutate(
    onset_missing = is.na(date_of_symptom_onset),
    onset_out_of_range = if (has_cols(range_flag_col)) {
      !is.na(.data[[range_flag_col]]) & .data[[range_flag_col]] == "out_of_range"
    } else {
      FALSE
    },
    onset_needs_imputation = onset_missing | onset_out_of_range,
    reporting_date_available = if (has_cols("reporting_date")) !is.na(reporting_date) else FALSE
  )

n_missing <- sum(df$onset_missing)
n_out_of_range <- sum(df$onset_out_of_range)
n_needs_imputation <- sum(df$onset_needs_imputation)
n_imputable <- sum(df$onset_needs_imputation & df$reporting_date_available)

message(
  "  ", n_needs_imputation, " row(s) need onset imputation (",
  n_missing, " missing, ", n_out_of_range, " out_of_range); ",
  n_imputable, " have a usable reporting_date to impute from"
)

# --------------- Load Best-Fit Onset-to-Reporting Distribution -----------------
message("")
message("C. LOADING BEST-FIT ONSET-TO-REPORTING DISTRIBUTION")

delay_dist_dir <- file.path(
  epi_outputs_dir, "delay_distributions", IMPUTATION_CASE_GROUP, "onset_to_reporting_days"
)
fit_summary_csv <- file.path(epi_outputs_dir, "delay_distributions", "delay_distribution_fit_summary.csv")

best_family <- NA_character_
family_params <- NULL

if (!file.exists(fit_summary_csv)) {
  message("  Fit summary not found: ", fit_summary_csv, " -- run 1-fit_delay_distributions.R first. Skipping imputation.")
} else {
  fit_summary <- read.csv(fit_summary_csv, stringsAsFactors = FALSE)
  if (!"case_group" %in% names(fit_summary)) {
    message("  Fit summary has no case_group column (written by an older 1-fit_delay_distributions.R) -- rerun step 1.")
    fit_summary <- fit_summary[0, , drop = FALSE]
  }
  best_rows <- fit_summary[
    fit_summary$case_group == IMPUTATION_CASE_GROUP &
      fit_summary$delay == "onset_to_reporting_days" & fit_summary$is_best %in% TRUE,
  ]
  if (!nrow(best_rows)) {
    message("  No best-fit row for ", IMPUTATION_CASE_GROUP, " / onset_to_reporting_days in ",
            fit_summary_csv, " -- skipping imputation.")
  } else {
    if (nrow(best_rows) > 1) {
      message(
        "  WARNING: multiple is_best rows found for onset_to_reporting_days -- using the first (",
        best_rows$family[1], ")."
      )
    }
    best_family <- best_rows$family[1]

    fit_path <- file.path(delay_dist_dir, paste0(best_family, "_fit.rds"))
    fit <- tryCatch(readRDS(fit_path), error = function(e) NULL)
    if (is.null(fit)) {
      message("  Could not load cached fit for family '", best_family, "' at ", fit_path, " -- skipping imputation.")
      best_family <- NA_character_
    } else {
      family_params <- epidist_extract_family_params(fit, best_family)
      if (is.null(family_params)) {
        message("  Could not extract parameters from the '", best_family, "' fit -- skipping imputation.")
        best_family <- NA_character_
      } else {
        message(
          "  Using ", IMPUTATION_CASE_GROUP, " ", best_family, " fit for onset_to_reporting_days (mu=",
          round(family_params$mu, 3), ", disp=", round(family_params$disp, 3), ")"
        )
      }
    }
  }
}

# --------------------- Impute Missing/Out-of-Range Onset Dates -----------------
message("")
message("D. IMPUTING MISSING/OUT-OF-RANGE ONSET DATES")

# Analysis-ready columns. date_of_symptom_onset_imputed starts as a copy of
# the original (untouched) column and is only overwritten for rows actually
# imputed below -- so a needs-imputation row that couldn't be fixed (e.g. no
# reporting_date to anchor on) keeps showing its original, still-suspect
# value here, with onset_imputation_status flagging that it wasn't fixed.
df$date_of_symptom_onset_imputed <- df$date_of_symptom_onset
df$date_of_symptom_onset_imputed_lower <- as.Date(NA)
df$date_of_symptom_onset_imputed_upper <- as.Date(NA)
df$onset_date_was_imputed <- FALSE
df$onset_imputation_status <- ifelse(df$onset_needs_imputation, NA_character_, "not_needed")
df$onset_imputation_family <- NA_character_

impute_idx <- which(df$onset_needs_imputation & df$reporting_date_available)
no_reporting_idx <- which(df$onset_needs_imputation & !df$reporting_date_available)

if (length(no_reporting_idx)) {
  df$onset_imputation_status[no_reporting_idx] <- "not_imputed_no_reporting_date"
}

if (is.na(best_family)) {
  if (length(impute_idx)) {
    df$onset_imputation_status[impute_idx] <- "not_imputed_no_distribution"
  }
  message("  Nothing imputed (no usable onset_to_reporting_days distribution).")
} else if (!length(impute_idx)) {
  message("  Nothing to impute (no rows both need it and have a reporting_date).")
} else {

  rq <- epidist_family_rq_params(best_family, family_params$mu, family_params$disp)
  sample_fun <- switch(best_family,
    lognormal = stats::rlnorm, weibull = stats::rweibull, gamma = stats::rgamma
  )
  quantile_fun <- switch(best_family,
    lognormal = stats::qlnorm, weibull = stats::qweibull, gamma = stats::qgamma
  )

  # Fixed seed -> reruns against unchanged data reproduce the same imputed
  # dates rather than reshuffling them every run (same spirit as the fit/loo
  # .rds caching in 1-fit_delay_distributions.R, just for a cheap step where
  # caching the result itself isn't necessary).
  set.seed(IMPUTATION_SEED)

  n_impute <- length(impute_idx)
  delays <- do.call(sample_fun, c(list(n = n_impute), rq))

  out_of_bounds <- delays < DELAY_MIN_DAYS | delays > DELAY_MAX_DAYS
  attempt <- 0L
  while (any(out_of_bounds) && attempt < MAX_RESAMPLE_ATTEMPTS) {
    attempt <- attempt + 1L
    redraw <- do.call(sample_fun, c(list(n = sum(out_of_bounds)), rq))
    delays[out_of_bounds] <- redraw
    out_of_bounds <- delays < DELAY_MIN_DAYS | delays > DELAY_MAX_DAYS
  }

  # Still out of bounds after max_resample_attempts (a heavy-tailed fit can
  # make this rare-but-nonzero) -- clamp rather than leave an implausible
  # imputed onset date.
  clamped <- out_of_bounds
  if (any(clamped)) {
    delays[clamped] <- pmin(pmax(delays[clamped], DELAY_MIN_DAYS), DELAY_MAX_DAYS)
  }

  lower_q <- (1 - IMPUTATION_INTERVAL_WIDTH) / 2
  upper_q <- 1 - lower_q
  delay_lower_q <- do.call(quantile_fun, c(list(p = lower_q), rq))
  delay_upper_q <- do.call(quantile_fun, c(list(p = upper_q), rq))
  # Same clamping as the point draws, and for the same reason -- an
  # unclamped tail quantile of a heavy-tailed fit can itself be an
  # implausible number of days.
  delay_lower_q <- min(max(delay_lower_q, DELAY_MIN_DAYS), DELAY_MAX_DAYS)
  delay_upper_q <- min(max(delay_upper_q, DELAY_MIN_DAYS), DELAY_MAX_DAYS)

  # onset = reporting_date - delay, so a *larger* delay means an *earlier*
  # onset -- the delay's upper quantile gives the earliest plausible onset
  # (the date lower bound), and the delay's lower quantile gives the latest
  # plausible onset (the date upper bound).
  df$date_of_symptom_onset_imputed[impute_idx] <- df$reporting_date[impute_idx] - round(delays)
  df$date_of_symptom_onset_imputed_lower[impute_idx] <- df$reporting_date[impute_idx] - round(delay_upper_q)
  df$date_of_symptom_onset_imputed_upper[impute_idx] <- df$reporting_date[impute_idx] - round(delay_lower_q)
  df$onset_date_was_imputed[impute_idx] <- TRUE
  df$onset_imputation_family[impute_idx] <- best_family
  df$onset_imputation_status[impute_idx] <- ifelse(clamped, "imputed_clamped", "imputed")

  message(
    "  Imputed ", n_impute, " onset date(s) using ", best_family,
    " (", sum(clamped), " clamped to [", DELAY_MIN_DAYS, ", ", DELAY_MAX_DAYS,
    "] days after ", MAX_RESAMPLE_ATTEMPTS, " resample attempts); ",
    "interval width ", IMPUTATION_INTERVAL_WIDTH
  )
}

# Keep the imputed date/bounds next to the column they're derived from,
# rather than tacked on at the end where the rest of section D's columns land.
df <- df |>
  dplyr::relocate(
    date_of_symptom_onset_imputed,
    date_of_symptom_onset_imputed_lower,
    date_of_symptom_onset_imputed_upper,
    .after = date_of_symptom_onset
  )

# ----------------------------- Write output ------------------------------------
message("")
message("E. WRITING OUTPUT FILES")

output_csv <- file.path(epi_outputs_dir, cfg$data$files$linelist_with_imputed_onset)
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
processed_dhis2_linelist_with_imputed_onset <- df
date_imputation_complete <- TRUE

keep_objects <- c(keep_objects, "processed_dhis2_linelist_with_imputed_onset", "date_imputation_complete")
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
