#!/usr/bin/env Rscript
# Prepare the cleaned DHIS2 linelist for nowcasting (reads pipeline_configs.json):
#   - QC codes on ages and dates (*_qc columns; NA = OK)
#   - Test-result classification (positive / negative / invalid / blank / NA)
#   - Delay intervals, eligibility flags, daily count series, run metadata JSON

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

sandbox_mode <- F

# --------------- Determine which processed dirs to aggregate ------------------
# dhis2_force_stems has no source in this repo (no Shiny app here); kept only
# for parity with the upstream script. Normally we just take the latest synced
# date folder.
if (exists("dhis2_force_stems") && length(dhis2_force_stems) > 0) {
  agg_processed_dir <- file.path(data_root, "data")
  target_epi_dirs <- file.path(agg_processed_dir, dhis2_force_stems)
  target_epi_dirs <- target_epi_dirs[dir.exists(target_epi_dirs)]
  if (!length(target_epi_dirs))
    stop("None of the selected date folders were found under data/.", call. = FALSE)
} else {
  target_epi_dirs <- resolve_epi_latest_dir()
}

for (epi_dir in target_epi_dirs) {
  message("=== Nowcasting Prep: ", basename(epi_dir), " ===")
}

#------------------- Load Config for Epi Pipeline ------------------------------
config_path <- file.path(code_root, "pipelines", "pipeline_configs.json")
if (!file.exists(config_path)) {
  stop("Pipeline config not found: ", config_path, call. = FALSE)
}

pipeline_configs <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
cfg <- pipeline_configs$epi_linelist
cfg$version <- pipeline_configs$version
processed_dir <- resolve_epi_latest_dir()
# outputs/<DATE>/ is the default destination for everything this pipeline
# produces -- only the upstream-synced ctsafe linelist stays under
# data/<DATE>/ (processed_dir is kept only to derive the date stamp here).
# Both the linelist_with_delays input (from step 1) and every file this step
# writes now live under the same outputs/<DATE>/ directory.
outputs_dir <- file.path(data_root, "outputs", basename(processed_dir))
dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
cfg_path <- function(key) {
  file.path(outputs_dir, cfg$data$files[[key]])
}


#--------------------- Set input and output paths ------------------------------
# linelist_with_delays.csv is written by 1-fit_delay_distributions.R into
# outputs/<DATE>/delay_distributions/ (alongside the per-delay fit
# subfolders), not directly under outputs/<DATE>/ -- cfg_path() alone would
# point one level too shallow, so it's joined onto delay_distributions/ here
# to match where step 1 (and 2-date_imputation.R) actually read/write it.
input_csv <- file.path(outputs_dir, "delay_distributions", cfg$data$files$linelist_with_delays)
output_csv <- cfg_path("nowcasting_prep")
qc_log_csv <- cfg_path("nowcasting_qc_log")
qc_codes_csv <- cfg_path("nowcasting_qc_codes")
daily_counts_csv <- cfg_path("nowcasting_daily_counts")
daily_counts_province_csv <- cfg_path("nowcasting_daily_counts_by_province")
run_metadata_json <- cfg_path("nowcasting_run_metadata")

#--------------------- Retrieve/set some config variables ----------------------
EPIDEMIC_START <- as.Date(cfg$qc$epidemic_start)
EPIDEMIC_END <- as.Date(cfg$qc$epidemic_end)
PLAUSIBLE_YEAR_MIN <- as.integer(cfg$qc$plausible_year_min)
PLAUSIBLE_YEAR_MAX <- as.integer(cfg$qc$plausible_year_max)
AGE_MIN_YEARS <- cfg$qc$age_min_years
AGE_MAX_YEARS <- cfg$qc$age_max_years
EPOCH_DATE <- as.Date(cfg$qc$epoch_date)
DELAY_MIN_DAYS <- as.integer(cfg$nowcasting$delay$min_days)
DELAY_MAX_DAYS <- as.integer(cfg$nowcasting$delay$max_days)
ANALYSIS_START <- as.Date(cfg$nowcasting$analysis_start)
TEST_DAYS <- as.integer(cfg$nowcasting$test_days)
DATE_COLUMNS <- cfg$qc$date_columns

load_qc_codes <- function(codes_path, cfg) {
  if (!file.exists(codes_path)) {
    stop("QC codes catalog not found: ", codes_path, call. = FALSE)
  }
  qc <- read.csv(codes_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("code", "applies_to", "label", "description")
  missing <- setdiff(required, names(qc))
  if (length(missing)) {
    stop(
      "QC codes catalog missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  qc$code <- as.integer(qc$code)

  template_vars <- list(
    epidemic_start = cfg$qc$epidemic_start,
    epidemic_end = cfg$qc$epidemic_end,
    plausible_year_min = cfg$qc$plausible_year_min,
    plausible_year_max = cfg$qc$plausible_year_max,
    age_min_years = cfg$qc$age_min_years,
    age_max_years = cfg$qc$age_max_years,
    epoch_date = cfg$qc$epoch_date
  )
  interpolate_description <- function(template) {
    out <- template
    for (nm in names(template_vars)) {
      out <- gsub(
        sprintf("\\{%s\\}", nm),
        as.character(template_vars[[nm]]),
        out
      )
    }
    out
  }
  qc$description <- vapply(qc$description, interpolate_description, character(1))
  qc
}

qc_codes_source <- file.path(code_root, cfg$qc$codes_file)
QC_CODES <- load_qc_codes(qc_codes_source, cfg)



blank_or_na <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

parse_linelist_date <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  raw <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(raw))
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") {
      next
    }
    val <- sub("[T ].*$", "", val)
    parsed <- suppressWarnings(tryCatch(
      as.Date(val, tryFormats = c(
        "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%OS"
      )),
      error = function(e) NA
    ))
    if (!is.na(parsed)) {
      out[i] <- parsed
    }
  }
  out
}

age_years_numeric <- function(age, age_months) {
  age_num <- suppressWarnings(as.numeric(trimws(as.character(age))))
  months_num <- suppressWarnings(as.numeric(trimws(as.character(age_months))))
  if (length(months_num) != length(age_num)) {
    months_num <- rep(NA_real_, length(age_num))
  }
  years <- age_num
  has_months <- !is.na(months_num) & months_num > 0
  years[has_months] <- ifelse(
    is.na(years[has_months]), 0, years[has_months]
  ) + months_num[has_months] / 12
  years
}

init_qc_vector <- function(n) {
  rep(NA_integer_, n)
}

# Keep the lowest (most severe) code when multiple issues apply to one field.
apply_qc_code <- function(qc, idx, code) {
  if (length(idx) == 0L) {
    return(qc)
  }
  for (i in idx) {
    if (is.na(qc[i]) || code < qc[i]) {
      qc[i] <- code
    }
  }
  qc
}

qc_label <- function(code) {
  out <- QC_CODES$label[match(code, QC_CODES$code)]
  out[is.na(out)] <- "UNKNOWN"
  out
}

append_qc_log <- function(log_df, row_index, record_id, field, qc_code,
                          original_value, notes = "") {
  rbind(
    log_df,
    data.frame(
      row_index = row_index,
      record_id = record_id,
      field = field,
      qc_code = as.integer(qc_code),
      qc_label = qc_label(qc_code),
      original_value = as.character(original_value),
      notes = notes,
      stringsAsFactors = FALSE
    )
  )
}

record_id_for_row <- function(df, i) {
  if ("alert_id" %in% names(df) && !blank_or_na(df$alert_id[i])) {
    return(as.character(df$alert_id[i]))
  }
  as.character(i)
}

log_qc_rows <- function(log_df, df, idx, field, qc_code, value, notes = "") {
  for (i in idx) {
    log_df <- append_qc_log(
      log_df, i, record_id_for_row(df, i), field, qc_code, value[i], notes
    )
  }
  log_df
}

insert_after_column <- function(col_order, source_col, new_col) {
  if (!source_col %in% col_order || new_col %in% col_order) {
    return(unique(c(col_order, new_col)))
  }
  pos <- match(source_col, col_order)
  append(col_order[seq_len(pos)], new_col, after = pos)
}

sample_was_collected <- function(df, parsed_sample_dates = NULL) {
  n <- nrow(df)
  out <- rep(FALSE, n)

  flag_one <- function(x) {
    v <- suppressWarnings(as.integer(trimws(as.character(x))))
    !is.na(v) & v >= 1L
  }

  if ("samples_received" %in% names(df)) {
    out <- out | flag_one(df$samples_received)
  }
  if ("samples_analyzed" %in% names(df)) {
    out <- out | flag_one(df$samples_analyzed)
  }
  if ("sample_already_submitted" %in% names(df)) {
    raw <- tolower(trimws(as.character(df$sample_already_submitted)))
    out <- out | raw %in% c("yes", "oui", "1", "true")
  }
  if (!is.null(parsed_sample_dates)) {
    out <- out | !is.na(parsed_sample_dates)
  } else if ("date_of_sample_collection" %in% names(df)) {
    out <- out | !blank_or_na(df$date_of_sample_collection)
  }
  if ("sample_number" %in% names(df)) {
    out <- out | !blank_or_na(df$sample_number)
  }

  out
}

combine_dates <- function(date_lists) {
  if (!length(date_lists)) {
    return(as.Date(character()))
  }
  out <- do.call(c, date_lists)
  out <- as.Date(out, origin = "1970-01-01")
  out[!is.na(out)]
}

compute_run_cutoffs <- function(parsed_dates, cfg) {
  obs_fields <- cfg$nowcasting$obs_cutoff_fields
  obs_dates <- combine_dates(parsed_dates[intersect(obs_fields, names(parsed_dates))])
  obs_cutoff <- if (length(obs_dates)) max(obs_dates) else Sys.Date()

  trunc_field <- cfg$nowcasting$trunc_cutoff_field
  trunc_dates <- parsed_dates[[trunc_field]]
  trunc_dates <- as.Date(trunc_dates, origin = "1970-01-01")
  trunc_dates <- trunc_dates[!is.na(trunc_dates)]
  trunc_cutoff <- if (length(trunc_dates)) max(trunc_dates) else obs_cutoff

  test_days <- as.integer(cfg$nowcasting$test_days)
  train_cutoff <- trunc_cutoff - test_days

  list(
    analysis_start = as.Date(cfg$nowcasting$analysis_start),
    obs_cutoff = obs_cutoff,
    trunc_cutoff = trunc_cutoff,
    train_cutoff = train_cutoff,
    test_days = test_days,
    delay_onset_end = train_cutoff
  )
}

build_daily_counts <- function(df, cutoffs, complete_series = TRUE) {
  onset <- parse_linelist_date(df$date_of_symptom_onset)
  ok <- !is.na(onset) & !is.na(df$test_result_category)
  if (!any(ok)) {
    return(data.frame(
      date = as.Date(character()),
      d_pos = integer(),
      d_neg = integer(),
      d_inv = integer(),
      d_blk = integer(),
      d_pos_plus_blank = integer(),
      stringsAsFactors = FALSE
    ))
  }

  work <- data.frame(
    date = onset[ok],
    d_pos = as.integer(df$is_positive[ok]),
    d_neg = as.integer(df$is_negative[ok]),
    d_inv = as.integer(df$is_invalid[ok]),
    d_blk = as.integer(df$is_blank[ok]),
    stringsAsFactors = FALSE
  )
  daily <- stats::aggregate(
    cbind(d_pos, d_neg, d_inv, d_blk) ~ date,
    data = work,
    FUN = sum
  )
  daily$d_pos_plus_blank <- daily$d_pos + daily$d_blk

  if (!complete_series) {
    return(daily[order(daily$date), , drop = FALSE])
  }

  series_start <- cutoffs$analysis_start
  series_end <- cutoffs$trunc_cutoff
  all_dates <- seq(series_start, series_end, by = "day")
  daily <- merge(
    data.frame(date = all_dates, stringsAsFactors = FALSE),
    daily,
    by = "date",
    all.x = TRUE
  )
  daily[is.na(daily)] <- 0L
  daily[order(daily$date), , drop = FALSE]
}

province_confirmed_totals <- function(df) {
  if (!"province" %in% names(df)) {
    return(stats::setNames(numeric(), character()))
  }
  prov <- trimws(as.character(df$province))
  ok <- !blank_or_na(prov)
  if (!any(ok)) {
    return(stats::setNames(numeric(), character()))
  }
  pos <- as.integer(df$is_positive[ok])
  totals <- stats::aggregate(pos ~ prov[ok], FUN = sum)
  stats::setNames(totals$pos, totals$prov)
}

eligible_provinces_for_nowcast <- function(df, min_confirmed) {
  totals <- province_confirmed_totals(df)
  if (!length(totals)) {
    return(character())
  }
  names(totals)[totals > min_confirmed]
}

build_daily_counts_by_province <- function(
    df,
    cutoffs,
    complete_series = TRUE,
    eligible_provinces = NULL
) {
  if (!"province" %in% names(df)) {
    return(NULL)
  }
  onset <- parse_linelist_date(df$date_of_symptom_onset)
  ok <- !is.na(onset) & !is.na(df$test_result_category) & !blank_or_na(df$province)
  if (!is.null(eligible_provinces) && length(eligible_provinces)) {
    ok <- ok & df$province %in% eligible_provinces
  }
  if (!any(ok)) {
    return(NULL)
  }

  work <- data.frame(
    province = df$province[ok],
    date = onset[ok],
    d_pos = as.integer(df$is_positive[ok]),
    d_neg = as.integer(df$is_negative[ok]),
    d_inv = as.integer(df$is_invalid[ok]),
    d_blk = as.integer(df$is_blank[ok]),
    stringsAsFactors = FALSE
  )
  daily <- stats::aggregate(
    cbind(d_pos, d_neg, d_inv, d_blk) ~ province + date,
    data = work,
    FUN = sum
  )
  daily$d_pos_plus_blank <- daily$d_pos + daily$d_blk

  if (!complete_series) {
    return(daily[order(daily$province, daily$date), , drop = FALSE])
  }

  parts <- split(daily, daily$province)
  out <- lapply(names(parts), function(prov) {
    chunk <- parts[[prov]]
    all_dates <- data.frame(
      date = seq(cutoffs$analysis_start, cutoffs$trunc_cutoff, by = "day"),
      stringsAsFactors = FALSE
    )
    merged <- merge(all_dates, chunk, by = "date", all.x = TRUE)
    merged$province <- prov
    num_cols <- c("d_pos", "d_neg", "d_inv", "d_blk", "d_pos_plus_blank")
    for (nm in num_cols) {
      if (!nm %in% names(merged)) {
        merged[[nm]] <- 0L
      }
      merged[[nm]][is.na(merged[[nm]])] <- 0L
    }
    merged[, c("province", "date", num_cols), drop = FALSE]
  })
  do.call(rbind, out)
}

classify_dhis2_results <- function(final_mve_result, sample_collected) {
  raw <- trimws(tolower(as.character(final_mve_result)))
  n <- length(raw)
  if (length(sample_collected) != n) {
    stop("sample_collected length must match final_mve_result.", call. = FALSE)
  }

  empty <- blank_or_na(raw)
  is_positive <- !empty & raw == "positive"
  is_negative <- !empty & raw == "negative"
  is_invalid <- !empty & raw == "invalid"
  is_blank <- empty & sample_collected

  category <- rep(NA_character_, n)
  category[is_positive] <- "positive"
  category[is_negative] <- "negative"
  category[is_invalid] <- "invalid"
  category[is_blank] <- "blank"

  list(
    test_result_category = category,
    is_positive = is_positive,
    is_negative = is_negative,
    is_invalid = is_invalid,
    is_blank = is_blank,
    sample_collected = sample_collected
  )
}

classify_mve_case_classification <- function(case_classification, sample_collected) {
  raw <- trimws(tolower(as.character(case_classification)))
  n <- length(raw)
  if (length(sample_collected) != n) {
    stop(
      "sample_collected length must match final_mve_case_classification.",
      call. = FALSE
    )
  }

  empty <- blank_or_na(raw)
  is_positive <- !empty & raw == "confirmed_case"
  is_negative <- !empty & raw == "not_a_case"
  is_invalid <- rep(FALSE, n)
  is_blank <- (!empty & raw %in% c("suspected_case", "probable_case")) |
    (empty & sample_collected)

  category <- rep(NA_character_, n)
  category[is_positive] <- "positive"
  category[is_negative] <- "negative"
  category[is_invalid] <- "invalid"
  category[is_blank] <- "blank"

  list(
    test_result_category = category,
    is_positive = is_positive,
    is_negative = is_negative,
    is_invalid = is_invalid,
    is_blank = is_blank,
    sample_collected = sample_collected
  )
}

classify_for_nowcasting <- function(df, sample_collected) {
  if ("final_mve_case_classification" %in% names(df)) {
    return(classify_mve_case_classification(
      df$final_mve_case_classification,
      sample_collected
    ))
  }
  if ("final_mve_result" %in% names(df)) {
    return(classify_dhis2_results(df$final_mve_result, sample_collected))
  }
  stop(
    "final_mve_case_classification (or legacy final_mve_result) required ",
    "for nowcasting classification.",
    call. = FALSE
  )
}

if (exists("processed_dhis2_linelist_with_delays", inherits = TRUE)) {
  df <- get("processed_dhis2_linelist_with_delays", inherits = TRUE)
} else if (file.exists(input_csv)) {
  df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  stop("Linelist with delays not found: ", input_csv,
       ". Run 1-fit_delay_distributions.R first.", call. = FALSE)
}

n <- nrow(df)
message("Nowcasting prep: ", n, " rows")

qc_log <- data.frame(
  row_index = integer(),
  record_id = character(),
  field = character(),
  qc_code = integer(),
  qc_label = character(),
  original_value = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

parsed_dates <- list()
date_qc <- list()
for (col in intersect(DATE_COLUMNS, names(df))) {
  parsed_dates[[col]] <- parse_linelist_date(df[[col]])
  date_qc[[col]] <- init_qc_vector(n)
}

# ── Age QC codes 11–13 ──────────────────────────────────────────────────────────
df$age_years_combined <- age_years_numeric(
  if ("age" %in% names(df)) df$age else rep(NA, n),
  if ("age_months" %in% names(df)) df$age_months else rep(NA, n)
)

age_months_num <- if ("age_months" %in% names(df)) {
  suppressWarnings(as.numeric(trimws(as.character(df$age_months))))
} else {
  rep(NA_real_, n)
}

age_raw <- if ("age" %in% names(df)) trimws(as.character(df$age)) else rep("", n)

age_qc <- init_qc_vector(n)
age_months_qc <- init_qc_vector(n)

idx_age_range <- which(!is.na(df$age_years_combined) & (
  df$age_years_combined < AGE_MIN_YEARS | df$age_years_combined > AGE_MAX_YEARS
))
idx_months_range <- which(!is.na(age_months_num) & (
  age_months_num < 0 | age_months_num > 11
))
idx_age_unparseable <- which(
  !blank_or_na(age_raw) & is.na(suppressWarnings(as.numeric(age_raw)))
)

age_qc <- apply_qc_code(age_qc, idx_age_range, 11L)
age_months_qc <- apply_qc_code(age_months_qc, idx_months_range, 12L)
age_qc <- apply_qc_code(age_qc, idx_age_unparseable, 13L)

if ("age" %in% names(df)) {
  df$age_qc <- age_qc
}
if ("age_months" %in% names(df)) {
  df$age_months_qc <- age_months_qc
}

qc_log <- log_qc_rows(qc_log, df, idx_age_range, "age", 11L, df$age_years_combined)
qc_log <- log_qc_rows(qc_log, df, idx_months_range, "age_months", 12L, age_months_num)
qc_log <- log_qc_rows(qc_log, df, idx_age_unparseable, "age", 13L, age_raw)

message(
  "Age QC: code 11=", length(idx_age_range),
  " code 12=", length(idx_months_range),
  " code 13=", length(idx_age_unparseable)
)

# ── Date value QC codes 1–3 ───────────────────────────────────────────────────
for (col in names(parsed_dates)) {
  d <- parsed_dates[[col]]
  qc <- date_qc[[col]]

  idx_epoch <- which(!is.na(d) & d == EPOCH_DATE)
  idx_window <- which(!is.na(d) & (d < EPIDEMIC_START | d > EPIDEMIC_END))
  idx_year <- which(!is.na(d) & (
    as.integer(format(d, "%Y")) < PLAUSIBLE_YEAR_MIN |
      as.integer(format(d, "%Y")) > PLAUSIBLE_YEAR_MAX
  ))

  qc <- apply_qc_code(qc, idx_epoch, 1L)
  qc <- apply_qc_code(qc, idx_window, 2L)
  qc <- apply_qc_code(qc, idx_year, 3L)
  date_qc[[col]] <- qc

  df[[paste0(col, "_qc")]] <- qc

  qc_log <- log_qc_rows(qc_log, df, idx_epoch, col, 1L, as.character(d[idx_epoch]))
  qc_log <- log_qc_rows(qc_log, df, idx_window, col, 2L, as.character(d[idx_window]))
  qc_log <- log_qc_rows(qc_log, df, idx_year, col, 3L, as.character(d[idx_year]))
}

# ── Date order QC codes 4–10 (assigned to the offending field) ─────────────────
onset <- parsed_dates[["date_of_symptom_onset"]]
sample <- parsed_dates[["date_of_sample_collection"]]
notification <- parsed_dates[["date_of_notification"]]
reporting <- parsed_dates[["reporting_date"]]
investigation <- parsed_dates[["investigation_datetime"]]
hosp_start <- parsed_dates[["date_of_hospitalisation_start"]]
hosp_end <- parsed_dates[["date_of_hospitalisation_end"]]

both <- function(a, b) !is.na(a) & !is.na(b)

order_checks <- list(
  list(code = 4L, field = "date_of_symptom_onset", mask = both(onset, sample) & onset > sample),
  list(code = 5L, field = "date_of_symptom_onset", mask = both(onset, notification) & onset > notification),
  list(code = 6L, field = "date_of_symptom_onset", mask = both(onset, reporting) & onset > reporting),
  list(code = 7L, field = "date_of_symptom_onset", mask = both(onset, investigation) & onset > investigation),
  list(code = 8L, field = "date_of_symptom_onset", mask = both(onset, hosp_start) & onset > hosp_start),
  list(code = 9L, field = "date_of_hospitalisation_start", mask = both(hosp_start, hosp_end) & hosp_start > hosp_end),
  list(code = 10L, field = "date_of_sample_collection", mask = both(sample, reporting) & sample > reporting)
)

for (chk in order_checks) {
  field <- chk$field
  if (!field %in% names(date_qc)) {
    next
  }
  idx <- which(chk$mask)
  date_qc[[field]] <- apply_qc_code(date_qc[[field]], idx, chk$code)
  df[[paste0(field, "_qc")]] <- date_qc[[field]]
  qc_log <- log_qc_rows(
    qc_log, df, idx, field, chk$code,
    rep("date order violation", length(idx)),
    QC_CODES$description[match(chk$code, QC_CODES$code)]
  )
}

date_issue_count <- sum(!is.na(unlist(date_qc)))
message("Date QC issues (field-level): ", date_issue_count)

# ── Case classification ───────────────────────────────────────────────────────
# positive / negative / invalid / blank (pending result, sample collected) / NA
if (!"final_mve_case_classification" %in% names(df) &&
    !"final_mve_result" %in% names(df)) {
  stop(
    "final_mve_case_classification (or legacy final_mve_result) required ",
    "for nowcasting classification.",
    call. = FALSE
  )
}

sample_collected <- sample_was_collected(
  df,
  parsed_sample_dates = parsed_dates[["date_of_sample_collection"]]
)
cls <- classify_for_nowcasting(df, sample_collected)
df$sample_collected <- cls$sample_collected
df$test_result_category <- cls$test_result_category
df$is_positive <- cls$is_positive
df$is_negative <- cls$is_negative
df$is_invalid <- cls$is_invalid
df$is_blank <- cls$is_blank

flag_sum <- sum(df$is_positive) + sum(df$is_negative) +
  sum(df$is_invalid) + sum(df$is_blank)
na_category <- sum(is.na(df$test_result_category))
if (flag_sum + na_category != n) {
  stop(
    "Case classification does not partition rows: ",
    flag_sum, " classified + ", na_category, " NA != ", n,
    call. = FALSE
  )
}
if (any(df$is_blank & !df$sample_collected, na.rm = TRUE)) {
  stop("is_blank=TRUE requires sample_collected=TRUE.", call. = FALSE)
}

message(
  "Case classification: positive=", sum(df$is_positive),
  " negative=", sum(df$is_negative),
  " invalid=", sum(df$is_invalid),
  " blank=", sum(df$is_blank),
  " NA=", na_category,
  " (no result, no sample)"
)

# ── Delay intervals (days) ────────────────────────────────────────────────────
# Computed upstream in 1-fit_delay_distributions.R now -- df already has these
# columns (read from dhis2_linelist_with_delays.csv). Keep the name list here
# only for the column-ordering step further down.
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

# ── Analysis cutoffs, eligibility, and daily aggregates ───────────────────────
cutoffs <- compute_run_cutoffs(parsed_dates, cfg)
onset <- parsed_dates[["date_of_symptom_onset"]]
sample <- parsed_dates[["date_of_sample_collection"]]

df$has_onset <- !is.na(onset)
df$has_onset_and_sample <- !is.na(onset) & !is.na(sample)
df$in_analysis_window <- !is.na(onset) &
  onset >= cutoffs$analysis_start &
  onset <= cutoffs$trunc_cutoff
df$in_training_window <- !is.na(sample) & sample <= cutoffs$train_cutoff
df$in_delay_onset_window <- !is.na(onset) &
  onset >= cutoffs$analysis_start &
  onset <= cutoffs$delay_onset_end

delay_valid <- !is.na(df$onset_to_sample_days) &
  df$onset_to_sample_days >= DELAY_MIN_DAYS &
  df$onset_to_sample_days <= DELAY_MAX_DAYS
require_positive <- isTRUE(cfg$nowcasting$delay$require_positive)
df$eligible_for_delay <- df$in_delay_onset_window &
  delay_valid &
  (!require_positive | df$is_positive)

message(
  "Cutoffs: analysis_start=", cutoffs$analysis_start,
  " obs=", cutoffs$obs_cutoff,
  " trunc=", cutoffs$trunc_cutoff,
  " train=", cutoffs$train_cutoff
)
message(
  "Eligibility: has_onset=", sum(df$has_onset),
  " has_onset_and_sample=", sum(df$has_onset_and_sample),
  " in_analysis_window=", sum(df$in_analysis_window),
  " in_training_window=", sum(df$in_training_window),
  " eligible_for_delay=", sum(df$eligible_for_delay)
)

complete_series <- isTRUE(cfg$nowcasting$daily_counts$complete_series)
province_min_confirmed <- cfg$nowcasting$province$min_confirmed_cases
if (is.null(province_min_confirmed)) {
  province_min_confirmed <- 50L
}
province_confirmed <- province_confirmed_totals(df)
provinces_nowcast <- eligible_provinces_for_nowcast(df, province_min_confirmed)
provinces_excluded <- setdiff(names(province_confirmed), provinces_nowcast)

df$in_province_nowcast <- !blank_or_na(df$province) &
  df$province %in% provinces_nowcast

daily_counts <- build_daily_counts(df, cutoffs, complete_series = complete_series)
daily_counts_province <- build_daily_counts_by_province(
  df,
  cutoffs,
  complete_series = complete_series,
  eligible_provinces = provinces_nowcast
)

if (nrow(daily_counts) > 0) {
  message(
    "Daily counts: ", nrow(daily_counts), " days",
    " d_pos=", sum(daily_counts$d_pos),
    " d_blk=", sum(daily_counts$d_blk)
  )
}
message(
  "Province nowcasting (confirmed > ", province_min_confirmed, "): ",
  if (length(provinces_nowcast)) paste(provinces_nowcast, collapse = ", ") else "(none)"
)
if (length(provinces_excluded)) {
  excluded_detail <- paste(
    sprintf("%s=%d", provinces_excluded, province_confirmed[provinces_excluded]),
    collapse = ", "
  )
  message("  Excluded provinces: ", excluded_detail)
}

run_metadata <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
  config_source = file.path("pipelines", "pipeline_configs.json"),
  config_version = cfg$version,
  pipeline = cfg$pipeline,
  data = cfg$data,
  nowcasting = list(
    analysis_start = format(cutoffs$analysis_start, "%Y-%m-%d"),
    obs_cutoff = format(cutoffs$obs_cutoff, "%Y-%m-%d"),
    trunc_cutoff = format(cutoffs$trunc_cutoff, "%Y-%m-%d"),
    train_cutoff = format(cutoffs$train_cutoff, "%Y-%m-%d"),
    delay_onset_end = format(cutoffs$delay_onset_end, "%Y-%m-%d"),
    test_days = cutoffs$test_days,
    trunc_cutoff_field = cfg$nowcasting$trunc_cutoff_field,
    daily_counts = cfg$nowcasting$daily_counts,
    province = cfg$nowcasting$province,
    delay = cfg$nowcasting$delay,
    growth = cfg$nowcasting$growth,
    provinces_included = as.list(provinces_nowcast),
    provinces_excluded = as.list(provinces_excluded),
    province_confirmed_totals = as.list(as.integer(province_confirmed))
  ),
  qc = cfg$qc,
  row_counts = list(
    total_rows = n,
    positive = sum(df$is_positive),
    negative = sum(df$is_negative),
    invalid = sum(df$is_invalid),
    blank = sum(df$is_blank),
    category_na = sum(is.na(df$test_result_category)),
    has_onset = sum(df$has_onset),
    has_onset_and_sample = sum(df$has_onset_and_sample),
    in_analysis_window = sum(df$in_analysis_window),
    in_training_window = sum(df$in_training_window),
    eligible_for_delay = sum(df$eligible_for_delay),
    in_province_nowcast = sum(df$in_province_nowcast)
  ),
  outputs = cfg$data$files
)

# ── Column order: place each *_qc immediately after its source field ──────────
col_order <- names(df)
for (col in intersect(DATE_COLUMNS, names(df))) {
  col_order <- insert_after_column(col_order, col, paste0(col, "_qc"))
}
if ("age" %in% names(df)) {
  col_order <- insert_after_column(col_order, "age", "age_qc")
}
if ("age_months" %in% names(df)) {
  col_order <- insert_after_column(col_order, "age_months", "age_months_qc")
}
if ("age_years_combined" %in% names(df)) {
  age_anchor <- if ("age_months_qc" %in% col_order) {
    "age_months_qc"
  } else if ("age_qc" %in% col_order) {
    "age_qc"
  } else {
    "age"
  }
  col_order <- insert_after_column(col_order, age_anchor, "age_years_combined")
}
classification_anchor <- if ("final_mve_case_classification" %in% names(df)) {
  "final_mve_case_classification"
} else {
  "final_mve_result"
}
if (classification_anchor %in% names(df)) {
  col_order <- insert_after_column(col_order, classification_anchor, "sample_collected")
  col_order <- insert_after_column(col_order, "sample_collected", "test_result_category")
  class_anchor <- "test_result_category"
  for (flag_col in c("is_positive", "is_negative", "is_invalid", "is_blank")) {
    col_order <- insert_after_column(col_order, class_anchor, flag_col)
    class_anchor <- flag_col
  }
}
eligibility_cols <- c(
  "has_onset", "has_onset_and_sample", "in_analysis_window",
  "in_training_window", "in_delay_onset_window", "eligible_for_delay",
  "in_province_nowcast"
)
col_order <- unique(c(col_order, delay_cols, eligibility_cols))
col_order <- col_order[!duplicated(col_order)]
df <- df[, intersect(col_order, names(df)), drop = FALSE]

# ── Write outputs ─────────────────────────────────────────────────────────────
dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
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
write.csv(qc_log, qc_log_csv, row.names = FALSE)
write.csv(QC_CODES, qc_codes_csv, row.names = FALSE)
counts_scope <- cfg$nowcasting$daily_counts$scope
if (counts_scope %in% c("national", "both")) {
  write.csv(daily_counts, daily_counts_csv, row.names = FALSE)
}
if (counts_scope %in% c("province", "both") &&
    !is.null(daily_counts_province) && nrow(daily_counts_province) > 0) {
  write.csv(daily_counts_province, daily_counts_province_csv, row.names = FALSE)
}
jsonlite::write_json(
  run_metadata,
  run_metadata_json,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

qc_cols <- grep("_qc$", names(df), value = TRUE)
rows_with_qc <- sum(apply(df[, qc_cols, drop = FALSE], 1, function(row) {
  any(!is.na(row))
}))

message("Wrote: ", output_csv)
message("Wrote: ", qc_log_csv, " (", nrow(qc_log), " entries)")
message("Wrote: ", qc_codes_csv)
if (counts_scope %in% c("national", "both")) {
  message("Wrote: ", daily_counts_csv)
}
if (counts_scope %in% c("province", "both") &&
    !is.null(daily_counts_province) && nrow(daily_counts_province) > 0) {
  message("Wrote: ", daily_counts_province_csv)
}
message("Wrote: ", run_metadata_json)
message("Rows with any QC code: ", rows_with_qc, " / ", nrow(df))

processed_dhis2_nowcasting_prep <- df
nowcasting_prep_complete <- TRUE

keep_objects <- c(
  keep_objects,
  "processed_dhis2_nowcasting_prep",
  "nowcasting_prep_complete",
  "code_root",
  "data_root",
  "QC_CODES",
  "cfg",
  "cutoffs",
  "run_metadata"
)
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
