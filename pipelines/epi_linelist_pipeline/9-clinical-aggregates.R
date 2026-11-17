#!/usr/bin/env Rscript
# Step 9: Clinical summary tables for the dashboard's Clinical Symptoms tab.
#   - Reads dhis2_linelist_with_imputed_onset.csv (2-date_imputation.R's
#     output) for this run's date; confirmed cases only
#     (clinical.classification in pipeline_configs.json)
#   - Writes, to outputs/<DATE>/clinical/, for national plus every province
#     with at least clinical.min_province_cases confirmed cases:
#       clinical_summary.csv       one row per location: case counts, cases
#                                  hospitalised, length of stay and
#                                  onset-to-admission (median, IQR), dead at
#                                  sampling, health workers
#       clinical_symptoms.csv      how often each symptom code is reported,
#                                  among cases with any symptoms recorded --
#                                  overall and by age group, sex and status at
#                                  sampling (alive / deceased)
#       clinical_length_of_stay.csv  histogram of days in hospital
#                                  (admission to discharge date), overall and
#                                  by status at sampling
#       clinical_age_sex.csv       cases by age band x sex (age pyramid), with
#                                  how many were dead at sampling
#       clinical_health_workers.csv  confirmed health workers by month of
#                                  onset and by role (nurse / doctor / other)
#       clinical_meta.json         settings the dashboard needs to describe
#                                  the tables (suppression threshold etc.)
#   - PUBLISHED DATA: these tables feed the public dashboard, so every count
#     from 1 to clinical.min_cell_count - 1 is suppressed (NA, suppressed =
#     TRUE), and any percentage or median computed from fewer cases is
#     dropped too. Zeros are kept.
#   - Caveat: there is no per-case outcome in the line list. "Dead at
#     sampling" (sample_collection_status = deceased) is NOT a case fatality
#     rate: it only counts people who had already died when sampled
#   - Caveat: symptoms are recorded less often for people dead at sampling,
#     so comparisons between the two groups use only cases with symptoms
#     recorded (the denominator is in the table)
#   - Symptom codes come from assets/maps/Symptoms_en.csv / Symptoms_fr.csv.
#     Codes with no label there (DT, DRO, ICT as of Nov 2026) keep their raw
#     code and label_confirmed = FALSE

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

# ------------------------------- Config ---------------------------------------
clin_cfg <- pipeline_configs$epi_linelist$clinical
if (is.null(clin_cfg)) {
  stop("pipeline_configs.json has no epi_linelist.clinical block.", call. = FALSE)
}
CLASSIFICATION      <- as.character(clin_cfg$classification)
MIN_CELL            <- as.integer(clin_cfg$min_cell_count)
MIN_PROVINCE_CASES  <- as.integer(clin_cfg$min_province_cases)
MAX_DELAY_DAYS      <- as.numeric(clin_cfg$max_delay_days)
SYMPTOM_AGE_BREAKS  <- as.numeric(unlist(clin_cfg$symptom_age_groups))
PYRAMID_BAND        <- as.numeric(clin_cfg$pyramid_age_band_years)
PYRAMID_TOP         <- as.numeric(clin_cfg$pyramid_top_age)
LOS_BINS            <- as.numeric(unlist(clin_cfg$los_bins))

# --------------- Determine which processed dir to handle ----------------------
epi_dir <- resolve_epi_latest_dir()
epi_outputs_dir <- file.path(data_root, "outputs", basename(epi_dir))
clinical_dir <- file.path(epi_outputs_dir, "clinical")
dir.create(clinical_dir, recursive = TRUE, showWarnings = FALSE)

message("======== Clinical aggregates: ", basename(epi_dir), " =========")

# ----------------------------- Load Linelist ----------------------------------
message("")
message("A. LOADING LINELIST FILE")
ll <- load_linelist_properly(file.path(epi_outputs_dir, "dhis2_linelist_with_imputed_onset.csv"))
ll$date_of_symptom_onset_imputed <- as.Date(ll$date_of_symptom_onset_imputed)
cases <- ll[!is.na(ll$final_mve_case_classification) & ll$final_mve_case_classification == CLASSIFICATION, , drop = FALSE]
message("  ", nrow(cases), " ", CLASSIFICATION, " rows (of ", nrow(ll), ")")
if (!nrow(cases)) stop("No ", CLASSIFICATION, " rows in the linelist.", call. = FALSE)

# ------------------------------ Helpers ---------------------------------------
num <- function(x) suppressWarnings(as.numeric(x))
blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# Suppress small counts: 1..MIN_CELL-1 -> NA. Zeros are kept.
suppress <- function(n) ifelse(!is.na(n) & n > 0 & n < MIN_CELL, NA_integer_, as.integer(n))
is_suppressed <- function(n) !is.na(n) & n > 0 & n < MIN_CELL

# Median / quartiles, or NA when fewer than MIN_CELL values.
q_or_na <- function(x, p) {
  x <- x[!is.na(x)]
  if (length(x) < MIN_CELL) return(NA_real_)
  as.numeric(stats::quantile(x, p, names = FALSE, type = 7))
}

# Unaccented lower case, for matching free-text roles.
norm_txt <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- suppressWarnings(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
  gsub("[^a-z0-9 ]", "", x)
}

# ----------------------------- Derived fields ---------------------------------
cases$age_num <- num(cases$age)
cases$status_at_sampling <- dplyr::case_when(
  cases$sample_collection_status %in% "deceased" ~ "deceased",
  cases$sample_collection_status %in% "alive" ~ "alive",
  TRUE ~ NA_character_
)
cases$sex_clean <- ifelse(cases$sex %in% c("female", "male"), cases$sex, NA_character_)

sym_age_labels <- vapply(seq_along(SYMPTOM_AGE_BREAKS), function(i) {
  lo <- SYMPTOM_AGE_BREAKS[i]
  if (i == length(SYMPTOM_AGE_BREAKS)) paste0(lo, "+") else paste0(lo, "-", SYMPTOM_AGE_BREAKS[i + 1] - 1)
}, character(1))
cases$symptom_age_group <- as.character(cut(
  cases$age_num, breaks = c(SYMPTOM_AGE_BREAKS, Inf), right = FALSE, labels = sym_age_labels
))

pyr_breaks <- c(seq(0, PYRAMID_TOP, by = PYRAMID_BAND), Inf)
pyr_labels <- vapply(seq_len(length(pyr_breaks) - 1), function(i) {
  lo <- pyr_breaks[i]
  if (is.infinite(pyr_breaks[i + 1])) paste0(lo, "+") else paste0(lo, "-", pyr_breaks[i + 1] - 1)
}, character(1))
cases$pyramid_band <- as.character(cut(cases$age_num, breaks = pyr_breaks, right = FALSE, labels = pyr_labels))

# Length of stay / onset to admission, only where step 1's range check passed
# and inside [0, MAX_DELAY_DAYS].
in_range <- function(value, flag) {
  v <- num(value)
  ok <- !is.na(v) & v >= 0 & v <= MAX_DELAY_DAYS
  if (!is.null(flag)) ok <- ok & (is.na(flag) | flag != "out_of_range")
  ifelse(ok, v, NA_real_)
}
cases$los_days <- in_range(cases$hospitalisation_start_to_end_days, cases$hospitalisation_start_to_end_days_range_flag)
cases$onset_admit_days <- in_range(cases$onset_to_hospitalisation_start_days, cases$onset_to_hospitalisation_start_days_range_flag)
cases$hospitalised <- !is.na(cases$date_of_hospitalisation_start)

cases$hcw_role <- dplyr::case_when(
  blank(cases$health_worker_position) ~ NA_character_,
  grepl("infirm|nurse", norm_txt(cases$health_worker_position)) ~ "nurse",
  grepl("medecin|doctor|docteur", norm_txt(cases$health_worker_position)) ~ "doctor",
  TRUE ~ "other"
)
cases$onset_month <- format(cases$date_of_symptom_onset_imputed, "%Y-%m")

# Symptom codes: comma-separated, upper case, deduplicated per case.
cases$symptom_list <- lapply(as.character(cases$symptoms), function(s) {
  if (is.na(s) || !nzchar(trimws(s))) return(character(0))
  unique(toupper(trimws(strsplit(s, ",", fixed = TRUE)[[1]])))
})
cases$symptom_list <- lapply(cases$symptom_list, function(v) v[nzchar(v)])
cases$has_symptoms <- lengths(cases$symptom_list) > 0

# Labels. A blank value in the map = code not yet identified.
# Uses 0-setup.R's load_map_csv() (handles a byte-order mark from Excel saves).
read_symptom_map <- function(name) {
  m <- tryCatch(load_map_csv(name), error = function(e) {
    warning("Symptom labels not loaded (", conditionMessage(e), "); raw codes used.", call. = FALSE)
    stats::setNames(character(0), character(0))
  })
  stats::setNames(trimws(unname(m)), toupper(trimws(names(m))))
}
SYM_EN <- read_symptom_map("Symptoms_en")
SYM_FR <- read_symptom_map("Symptoms_fr")
label_of <- function(code, map) {
  v <- unname(map[code])
  unname(ifelse(is.na(v) | !nzchar(v), code, v))
}

# ------------------------------ Locations -------------------------------------
province_counts <- table(cases$province[!blank(cases$province)])
provinces <- sort(names(province_counts)[province_counts >= MIN_PROVINCE_CASES])
message("  Provinces with >= ", MIN_PROVINCE_CASES, " cases: ", paste(provinces, collapse = ", "))

locations <- c(list(list(scale = "national", location = "National", rows = rep(TRUE, nrow(cases)))),
               lapply(provinces, function(p) list(scale = "province", location = p,
                                                  rows = !is.na(cases$province) & cases$province == p)))

# ------------------------------- Tables ---------------------------------------
message("")
message("B. BUILDING TABLES")

summary_rows <- list(); symptom_rows <- list(); los_rows <- list()
age_sex_rows <- list(); hcw_rows <- list()

los_labels <- vapply(seq_along(LOS_BINS), function(i) {
  lo <- LOS_BINS[i]
  hi <- if (i < length(LOS_BINS)) LOS_BINS[i + 1] - 1 else MAX_DELAY_DAYS
  if (lo == hi) as.character(lo) else paste0(lo, "-", hi)
}, character(1))
los_his <- c(LOS_BINS[-1] - 1, MAX_DELAY_DAYS)

for (loc in locations) {
  d <- cases[loc$rows, , drop = FALSE]
  key <- data.frame(spatial_scale = loc$scale, location = loc$location, stringsAsFactors = FALSE)

  # --- summary ---
  n_hosp <- sum(d$hospitalised)
  n_dead <- sum(d$status_at_sampling %in% "deceased")
  n_hcw  <- sum(!is.na(d$hcw_role))
  n_sym  <- sum(d$has_symptoms)
  summary_rows[[length(summary_rows) + 1L]] <- cbind(key, data.frame(
    n_cases = suppress(nrow(d)),
    n_symptoms_recorded = suppress(n_sym),
    n_hospitalised = suppress(n_hosp),
    n_dead_at_sampling = suppress(n_dead),
    n_health_workers = suppress(n_hcw),
    n_los = suppress(sum(!is.na(d$los_days))),
    los_median = q_or_na(d$los_days, 0.5),
    los_q25 = q_or_na(d$los_days, 0.25),
    los_q75 = q_or_na(d$los_days, 0.75),
    n_onset_admit = suppress(sum(!is.na(d$onset_admit_days))),
    onset_admit_median = q_or_na(d$onset_admit_days, 0.5),
    onset_admit_q25 = q_or_na(d$onset_admit_days, 0.25),
    onset_admit_q75 = q_or_na(d$onset_admit_days, 0.75),
    stringsAsFactors = FALSE
  ))

  # --- symptoms: overall and by breakdown, among cases with symptoms ---
  ds <- d[d$has_symptoms, , drop = FALSE]
  groups <- list(list(breakdown = "all", group = "all", rows = rep(TRUE, nrow(ds))))
  for (g in sym_age_labels) groups[[length(groups) + 1L]] <-
    list(breakdown = "age_group", group = g, rows = ds$symptom_age_group %in% g)
  for (g in c("female", "male")) groups[[length(groups) + 1L]] <-
    list(breakdown = "sex", group = g, rows = ds$sex_clean %in% g)
  for (g in c("alive", "deceased")) groups[[length(groups) + 1L]] <-
    list(breakdown = "status_at_sampling", group = g, rows = ds$status_at_sampling %in% g)

  all_codes <- sort(unique(unlist(cases$symptom_list)))
  for (g in groups) {
    sl <- ds$symptom_list[g$rows]
    denom <- length(sl)
    # unname(): vapply over a character vector names its result by the codes,
    # and data.frame() would then try to use those as row names.
    counts <- unname(vapply(all_codes, function(cd) sum(vapply(sl, function(v) cd %in% v, logical(1))), integer(1)))
    n_s <- suppress(counts)
    den_s <- suppress(denom)
    pct <- ifelse(is.na(n_s) | is.na(den_s) | denom == 0, NA_real_, round(100 * counts / denom, 1))
    symptom_rows[[length(symptom_rows) + 1L]] <- cbind(key[rep(1, length(all_codes)), , drop = FALSE], data.frame(
      breakdown = g$breakdown, group = g$group,
      symptom_code = all_codes,
      symptom_label_en = label_of(all_codes, SYM_EN),
      symptom_label_fr = label_of(all_codes, SYM_FR),
      label_confirmed = unname(!is.na(SYM_EN[all_codes]) & nzchar(SYM_EN[all_codes])),
      n_with_symptom = n_s,
      n_symptoms_recorded = den_s,
      pct = pct,
      suppressed = is_suppressed(counts) | is_suppressed(denom),
      stringsAsFactors = FALSE, row.names = NULL
    ), row.names = NULL)
  }

  # --- length of stay histogram ---
  for (grp in c("all", "alive", "deceased")) {
    v <- if (grp == "all") d$los_days else d$los_days[d$status_at_sampling %in% grp]
    v <- v[!is.na(v)]
    bin <- findInterval(v, LOS_BINS)
    counts <- vapply(seq_along(LOS_BINS), function(i) sum(bin == i), integer(1))
    los_rows[[length(los_rows) + 1L]] <- cbind(key[rep(1, length(LOS_BINS)), , drop = FALSE], data.frame(
      status_at_sampling = grp, stay_bin = los_labels, bin_lo = LOS_BINS, bin_hi = los_his,
      n = suppress(counts), suppressed = is_suppressed(counts), stringsAsFactors = FALSE, row.names = NULL
    ), row.names = NULL)
  }

  # --- age pyramid ---
  for (band in pyr_labels) for (sx in c("female", "male")) {
    sel <- d$pyramid_band %in% band & d$sex_clean %in% sx
    n <- sum(sel); nd <- sum(sel & d$status_at_sampling %in% "deceased")
    age_sex_rows[[length(age_sex_rows) + 1L]] <- cbind(key, data.frame(
      age_band = band, sex = sx, n = suppress(n), n_dead_at_sampling = suppress(nd),
      suppressed = is_suppressed(n) | is_suppressed(nd), stringsAsFactors = FALSE
    ))
  }

  # --- health workers ---
  h <- d[!is.na(d$hcw_role), , drop = FALSE]
  months <- sort(unique(stats::na.omit(cases$onset_month)))
  for (m in months) {
    n <- sum(h$onset_month %in% m)
    hcw_rows[[length(hcw_rows) + 1L]] <- cbind(key, data.frame(
      breakdown = "onset_month", group = m, n = suppress(n), suppressed = is_suppressed(n), stringsAsFactors = FALSE))
  }
  for (r in c("nurse", "doctor", "other")) {
    n <- sum(h$hcw_role %in% r)
    hcw_rows[[length(hcw_rows) + 1L]] <- cbind(key, data.frame(
      breakdown = "role", group = r, n = suppress(n), suppressed = is_suppressed(n), stringsAsFactors = FALSE))
  }
}

# -------------------------------- Write ---------------------------------------
message("")
message("C. WRITING")
write_tbl <- function(rows, name) {
  df <- dplyr::bind_rows(rows)
  path <- file.path(clinical_dir, name)
  utils::write.csv(df, path, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  message("  Wrote: ", path, " (", nrow(df), " rows)")
}
write_tbl(summary_rows, "clinical_summary.csv")
write_tbl(symptom_rows, "clinical_symptoms.csv")
write_tbl(los_rows, "clinical_length_of_stay.csv")
write_tbl(age_sex_rows, "clinical_age_sex.csv")
write_tbl(hcw_rows, "clinical_health_workers.csv")

meta <- list(
  reference_date = basename(epi_dir),
  classification = CLASSIFICATION,
  min_cell_count = MIN_CELL,
  min_province_cases = MIN_PROVINCE_CASES,
  max_delay_days = MAX_DELAY_DAYS,
  symptom_age_groups = sym_age_labels,
  pyramid_age_bands = pyr_labels,
  unconfirmed_symptom_codes = sort(setdiff(unique(unlist(cases$symptom_list)), names(SYM_EN)[nzchar(SYM_EN)]))
)
jsonlite::write_json(meta, file.path(clinical_dir, "clinical_meta.json"), auto_unbox = TRUE, pretty = TRUE)
message("  Wrote: ", file.path(clinical_dir, "clinical_meta.json"))

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
