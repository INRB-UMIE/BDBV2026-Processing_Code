#!/usr/bin/env Rscript
# Step 1: Delay intervals and imputation for the epi linelist pipeline.
#   - Computes raw delay intervals (days) between key linelist dates (onset,
#     sample collection, notification, reporting, investigation, hospitalisation)
#   - Flags delays outside the plausible [min_days, max_days] window from
#     pipeline_configs.json (nowcasting.delay) -- informational only for now
#   - Writes dhis2_linelist_with_delays.csv, which 3-nowcasting_prep.R (and any
#     later step) reads instead of the raw ctsafe linelist
#
# This logic was moved out of 3-nowcasting_prep.R verbatim (same columns, same
# bounds-check messages) so step 3 no longer computes delays itself.
#
# NOT YET IMPLEMENTED: fitting parametric delay distributions (fitdistrplus)
# and imputing missing dates from them -- 0-setup.R already loads fitdistrplus
# and pammtools in anticipation of this. To be designed next.

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

sandbox_mode <- F

# --------------- Determine which processed dirs to handle ---------------------
# resolve_epi_latest_dir() returns the single latest synced date folder; the
# loop below matches steps 2/3's structure in case that ever returns more than
# one directory again.
target_epi_dirs <- resolve_epi_latest_dir()

#------------------- Load Config for Epi Pipeline ------------------------------
cfg <- pipeline_configs$epi_linelist
cfg$version <- pipeline_configs$version

DATE_COLUMNS <- cfg$qc$date_columns
DELAY_MIN_DAYS <- as.integer(cfg$nowcasting$delay$min_days)
DELAY_MAX_DAYS <- as.integer(cfg$nowcasting$delay$max_days)

# ---------------------------- Helper functions ---------------------------------
# parse_linelist_date() and load_linelist_properly() now live in 0-setup.R,
# shared with any other step that needs them.
days_between <- function(later, earlier) {
  out <- rep(NA_integer_, length(later))
  ok <- !is.na(later) & !is.na(earlier)
  out[ok] <- as.integer(later[ok] - earlier[ok])
  out
}

# ----------------------------- Load Linelist ----------------------------------
message("======== Delays & Imputation: ", basename(epi_dir), " =========")
message("")
message("A. LOADING LINELIST FILE")
input_csv <- file.path(target_epi_dirs, cfg$data$files$processed_linelist)
df <- load_linelist_properly(input_csv, date_columns = DATE_COLUMNS)
message("Loaded linelist file,  ", nrow(df), " rows")

# --------------------- Calculate Delay Intervals ------------------------------
message("")
message("B. CALCULATING DELAYS AND FLAGGING SUSPICIOUS DATES")
  onset <- df[["date_of_symptom_onset"]]
  sample <- df[["date_of_sample_collection"]]
  notification <- df[["date_of_notification"]]
  reporting <- df[["reporting_date"]]
  investigation <- df[["investigation_datetime"]]
  hosp_start <- df[["date_of_hospitalisation_start"]]
  hosp_end <- df[["date_of_hospitalisation_end"]]

  df$onset_to_sample_days <- days_between(sample, onset)
  df$onset_to_notification_days <- days_between(notification, onset)
  df$onset_to_investigation_days <- days_between(investigation, onset)
  df$onset_to_reporting_days <- days_between(reporting, onset)
  df$onset_to_hospitalisation_start_days <- days_between(hosp_start, onset)
  df$onset_to_hospitalisation_end_days <- days_between(hosp_end, onset)
  df$notification_to_sample_days <- days_between(sample, notification)
  df$hospitalisation_start_to_end_days <- days_between(hosp_end, hosp_start)

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

  for (col in delay_cols) {
    bad <- !is.na(df[[col]]) & (
      df[[col]] < DELAY_MIN_DAYS | df[[col]] > DELAY_MAX_DAYS
    )
    if (any(bad)) {
      message(
        "  ", col, ": ", sum(!is.na(df[[col]])), " observed, ",
        sum(bad), " outside [", DELAY_MIN_DAYS, ", ", DELAY_MAX_DAYS, "] days"
      )
    } else {
      observed <- sum(!is.na(df[[col]]))
      if (observed > 0) {
        vals <- df[[col]][!is.na(df[[col]])]
        message(
          "  ", col, ": n=", observed,
          " median=", median(vals),
          " mean=", round(mean(vals), 1)
        )
      }
    }
  }

  # --------------------------- Imputation (not yet implemented) -----------------
  # Placeholder for fitting delay distributions (fitdistrplus) and imputing
  # missing dates from them -- see header comment. Left as a no-op for now.

  # ----------------------------- Write output ------------------------------------
  # Note: date columns are now real Date objects (via load_linelist_properly),
  # so they'll be written out here in normalized ISO (YYYY-MM-DD) format, even
  # where the raw ctsafe export had mixed formats.
  output_csv <- file.path(epi_dir, cfg$data$files$linelist_with_delays)
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

} # end for (epi_dir in target_epi_dirs)

# -----------------------------
# Cleanup
# -----------------------------
# Kept under a distinct name (not "processed_dhis2_linelist") so it can't be
# silently overwritten by 2-aggregate-linelist.R's same-named local variable if
# scripts are ever sourced together in one session -- 3-nowcasting_prep.R's
# exists()-check looks for this exact name.
processed_dhis2_linelist_with_delays <- df
delays_and_imputation_complete <- TRUE

keep_objects <- c(keep_objects, "processed_dhis2_linelist_with_delays", "delays_and_imputation_complete")
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
