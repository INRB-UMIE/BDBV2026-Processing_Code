#!/usr/bin/env Rscript
# Step 3: Aggregation for the epi linelist pipeline.
#   - Reads dhis2_linelist_with_imputed_onset.csv (2-date_imputation.R's output)
#   - Writes status_aggregated.csv: daily case-status counts (blank/
#     confirmed_case/not_a_case/probable_case/suspected_case), at national,
#     province, and health-zone spatial scales (one long table, a
#     "spatial_scale" column plus NA placeholders distinguish the scales)
#   - Writes status_aggregated_positives_only.csv: same table with only
#     confirmed_case counts (non-positive classifications dropped)
#   - Writes cumulative_positive_deaths.csv: daily and cumulative counts of
#     confirmed death alerts (type_of_alert = death,
#     final_mve_case_classification = confirmed_case) by reporting_date, at the
#     same three spatial scales
#   - Writes rolling_positivity.csv: 5-day rolling test positivity (Wilson CI),
#     at the same three spatial scales, when lab_analysis_date is present in
#     this sync -- else an empty placeholder
#   - Writes lab_positivity_aggregated.csv: the same Wilson-CI + 5-day-rolling
#     positivity, but by lab and on lab_analysis_date rather than onset date
#   - sandbox_mode (off by default) is a placeholder for exploratory ad hoc
#     work, not run automatically -- currently empty

# ------------------------- Check if initialised -------------------------------
# Adapted for BDBV2026-Contact_Tracing_Analysis: there is no local
# 1-colname-standard.R here (raw exports and PII-bearing cleaning stay in
# BDBV2026-Linelist_Processing). This repo consumes the de-identified
# "ctsafe" snapshot that gets auto-synced in; resolve_epi_latest_dir()
# (see pipelines/0-setup.R) finds the latest one.
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

sandbox_mode <- F

# --------------- Determine which processed dir to handle ----------------------
# resolve_epi_latest_dir() returns the single latest synced date folder.
epi_dir <- resolve_epi_latest_dir()

# outputs/<DATE>/ is the default destination for everything this pipeline
# produces -- only the upstream-synced ctsafe linelist stays under data/<DATE>/.
epi_outputs_dir <- file.path(data_root, "outputs", basename(epi_dir))
dir.create(epi_outputs_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------- Load Linelist ----------------------------------
message("======== Aggregation: ", basename(epi_dir), " =========")
message("")
message("A. LOADING LINELIST FILE")
processed_dhis2_linelist <- load_linelist_properly(file.path(epi_outputs_dir, "dhis2_linelist_with_imputed_onset.csv"))
# date_of_symptom_onset_imputed isn't in qc.date_columns (it's a 2-date_imputation.R
# addition, not part of the standard DATE_COLUMNS set load_linelist_properly() parses),
# so it needs an explicit cast here rather than relying on that to have handled it.
processed_dhis2_linelist$date_of_symptom_onset_imputed <- as.Date(processed_dhis2_linelist$date_of_symptom_onset_imputed)
message("  Loaded linelist file, ", nrow(processed_dhis2_linelist), " rows")

# --------------------------- Status Aggregation ------------------------------
message("")
message("B. STATUS AGGREGATION")

# Each spatial scale is aggregated separately, keeping only the identifier
# columns that actually apply at that scale (e.g. no province/health_zone for
# "national"). 
linelist_status_base <- processed_dhis2_linelist |>
  dplyr::filter(!date_of_symptom_onset_imputed < as.Date("2026-04-01")) |> #these dates are likely errors, we don't want them on the dashboard
  dplyr::mutate(final_mve_case_classification = ifelse(is.na(final_mve_case_classification), "blank", final_mve_case_classification))

classification_cols <- c("blank", "confirmed_case", "not_a_case", "probable_case", "suspected_case")

national_status <- linelist_status_base |>
  dplyr::group_by(country, date_of_symptom_onset_imputed, onset_date_was_imputed, final_mve_case_classification) |>
  dplyr::mutate(n = n()) |>
  dplyr::select(country, date_of_symptom_onset_imputed, onset_date_was_imputed, final_mve_case_classification, n) |>
  dplyr::distinct() |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = n) |>
  dplyr::mutate(dplyr::across(dplyr::any_of(classification_cols), ~ tidyr::replace_na(.x, 0))) |>
  dplyr::mutate(spatial_scale = "national", province = NA_character_, health_zone = NA_character_)

province_status <- linelist_status_base |>
  dplyr::group_by(country, province, date_of_symptom_onset_imputed, onset_date_was_imputed, final_mve_case_classification) |>
  dplyr::mutate(n = n()) |>
  dplyr::select(country, province, date_of_symptom_onset_imputed, onset_date_was_imputed, final_mve_case_classification, n) |>
  dplyr::distinct() |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = n) |>
  dplyr::mutate(dplyr::across(dplyr::any_of(classification_cols), ~ tidyr::replace_na(.x, 0))) |>
  dplyr::mutate(spatial_scale = "province", health_zone = NA_character_)

healthzone_status <- linelist_status_base |>
  dplyr::group_by(country, health_zone, date_of_symptom_onset_imputed, onset_date_was_imputed, final_mve_case_classification) |>
  dplyr::mutate(n = n()) |>
  dplyr::select(country, health_zone, date_of_symptom_onset_imputed, onset_date_was_imputed, final_mve_case_classification, n) |>
  dplyr::distinct() |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = n) |>
  dplyr::mutate(dplyr::across(dplyr::any_of(classification_cols), ~ tidyr::replace_na(.x, 0))) |>
  dplyr::mutate(spatial_scale = "healthzone", province = NA_character_)

dhis2_linelist_aggregated_status <- dplyr::bind_rows(national_status, province_status, healthzone_status) |>
  dplyr::arrange(date_of_symptom_onset_imputed)

status_aggregated_csv <- file.path(epi_outputs_dir, "status_aggregated.csv")
write.csv(dhis2_linelist_aggregated_status, status_aggregated_csv, row.names = FALSE)
message("  Wrote: ", status_aggregated_csv, " (", nrow(dhis2_linelist_aggregated_status), " rows)")

dhis2_linelist_aggregated_status_filtered <- dhis2_linelist_aggregated_status |>
  dplyr::select(!c(blank, not_a_case, probable_case, suspected_case))

status_aggregated_positives_only_csv <- file.path(epi_outputs_dir, "status_aggregated_positives_only.csv")
write.csv(dhis2_linelist_aggregated_status_filtered, status_aggregated_positives_only_csv, row.names = FALSE)
message("  Wrote: ", status_aggregated_positives_only_csv, " (", nrow(dhis2_linelist_aggregated_status), " rows)")

# ---------------------- Cumulative Positive Deaths -----------------------------
message("")
message("C. CUMULATIVE POSITIVE DEATHS")

deaths_base <- processed_dhis2_linelist |>
  dplyr::filter(trimws(as.character(type_of_alert)) == "death") |>
  dplyr::filter(final_mve_case_classification == "confirmed_case") |>
  dplyr::filter(!is.na(reporting_date)) |>
  dplyr::filter(!reporting_date < as.Date("2026-04-01"))

national_deaths <- deaths_base |>
  dplyr::group_by(country, reporting_date) |>
  dplyr::summarise(daily_deaths = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(reporting_date) |>
  dplyr::group_by(country) |>
  dplyr::mutate(cumulative_deaths = cumsum(daily_deaths)) |>
  dplyr::ungroup() |>
  dplyr::mutate(spatial_scale = "national", province = NA_character_, health_zone = NA_character_)

province_deaths <- deaths_base |>
  dplyr::group_by(country, province, reporting_date) |>
  dplyr::summarise(daily_deaths = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(province, reporting_date) |>
  dplyr::group_by(province) |>
  dplyr::mutate(cumulative_deaths = cumsum(daily_deaths)) |>
  dplyr::ungroup() |>
  dplyr::mutate(spatial_scale = "province", health_zone = NA_character_)

healthzone_deaths <- deaths_base |>
  dplyr::group_by(country, health_zone, reporting_date) |>
  dplyr::summarise(daily_deaths = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(health_zone, reporting_date) |>
  dplyr::group_by(health_zone) |>
  dplyr::mutate(cumulative_deaths = cumsum(daily_deaths)) |>
  dplyr::ungroup() |>
  dplyr::mutate(spatial_scale = "healthzone", province = NA_character_)

dhis2_linelist_cumulative_positive_deaths <- dplyr::bind_rows(
  national_deaths, province_deaths, healthzone_deaths
) |>
  dplyr::arrange(reporting_date)

cumulative_positive_deaths_csv <- file.path(epi_outputs_dir, "cumulative_positive_deaths.csv")
write.csv(dhis2_linelist_cumulative_positive_deaths, cumulative_positive_deaths_csv, row.names = FALSE)
message("  Wrote: ", cumulative_positive_deaths_csv, " (", nrow(dhis2_linelist_cumulative_positive_deaths), " rows)")

# -------------------------- Positivity Aggregation -----------------------------
message("")
message("D. ROLLING POSITIVITY")

# positivity_aggregated.csv requires lab_analysis_date — skip gracefully for older linelists
if ("lab_analysis_date" %in% names(processed_dhis2_linelist)) {

  # Uses onset date (lab_analysis_date used only for filter / earliest sample).
  # date_of_symptom_onset_imputed is used at face value here -- imputed and
  # observed onset dates are treated the same, no onset_date_was_imputed split.
  positivity_base <- processed_dhis2_linelist |>
    dplyr::filter(!date_of_symptom_onset_imputed < as.Date("2026-04-01")) |>
    dplyr::filter(!is.na(lab_analysis_date)) |>
    dplyr::mutate(final_mve_case_classification = ifelse(final_mve_case_classification == "", "no_result", final_mve_case_classification))

  # Each spatial scale aggregated separately, same reasoning as the status
  # aggregation above -- province/health_zone only pivoted alongside the
  # identifier that actually applies at that scale, so counts don't fragment.
  national_positivity <- positivity_base |>
    dplyr::group_by(country) |>
    dplyr::mutate(total_samples_analysed = n()) |>
    dplyr::filter(total_samples_analysed > 10) |>
    dplyr::mutate(earliest_analysed_sample = min(na.omit(lab_analysis_date))) |>
    dplyr::group_by(country, date_of_symptom_onset_imputed) |>
    dplyr::mutate(total_samples_analysed_daily = n()) |>
    dplyr::group_by(country, date_of_symptom_onset_imputed, final_mve_case_classification) |>
    dplyr::mutate(num = n()) |>
    dplyr::select(country, date_of_symptom_onset_imputed, earliest_analysed_sample, total_samples_analysed, total_samples_analysed_daily, final_mve_case_classification, num) |>
    dplyr::distinct() |>
    dplyr::filter(!country == "") |>
    tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = num) |>
    dplyr::mutate(across(everything(), ~ replace_na(., 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(daily_positivity_mean = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$mean,
                  daily_positivity_lower = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$lower,
                  daily_positivity_upper = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$upper) |>
    dplyr::mutate(spatial_scale = "national", province = NA_character_, health_zone = NA_character_)

  province_positivity <- positivity_base |>
    dplyr::group_by(province) |>
    dplyr::mutate(total_samples_analysed = n()) |>
    dplyr::filter(total_samples_analysed > 10) |>
    dplyr::mutate(earliest_analysed_sample = min(na.omit(lab_analysis_date))) |>
    dplyr::group_by(province, date_of_symptom_onset_imputed) |>
    dplyr::mutate(total_samples_analysed_daily = n()) |>
    dplyr::group_by(province, date_of_symptom_onset_imputed, final_mve_case_classification) |>
    dplyr::mutate(num = n()) |>
    dplyr::select(province, date_of_symptom_onset_imputed, earliest_analysed_sample, total_samples_analysed, total_samples_analysed_daily, final_mve_case_classification, num) |>
    dplyr::distinct() |>
    dplyr::filter(!province == "") |>
    tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = num) |>
    dplyr::mutate(across(everything(), ~ replace_na(., 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(daily_positivity_mean = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$mean,
                  daily_positivity_lower = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$lower,
                  daily_positivity_upper = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$upper) |>
    dplyr::mutate(spatial_scale = "province", health_zone = NA_character_)

  healthzone_positivity <- positivity_base |>
    dplyr::group_by(health_zone) |>
    dplyr::mutate(total_samples_analysed = n()) |>
    dplyr::filter(total_samples_analysed > 10) |>
    dplyr::mutate(earliest_analysed_sample = min(na.omit(lab_analysis_date))) |>
    dplyr::group_by(health_zone, date_of_symptom_onset_imputed) |>
    dplyr::mutate(total_samples_analysed_daily = n()) |>
    dplyr::group_by(health_zone, date_of_symptom_onset_imputed, final_mve_case_classification) |>
    dplyr::mutate(num = n()) |>
    dplyr::select(health_zone, date_of_symptom_onset_imputed, earliest_analysed_sample, total_samples_analysed, total_samples_analysed_daily, final_mve_case_classification, num) |>
    dplyr::distinct() |>
    dplyr::filter(!health_zone == "") |>
    tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = num) |>
    dplyr::mutate(across(everything(), ~ replace_na(., 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(daily_positivity_mean = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$mean,
                  daily_positivity_lower = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$lower,
                  daily_positivity_upper = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$upper) |>
    dplyr::mutate(spatial_scale = "healthzone", province = NA_character_)

  # 5 day rolling, per scale
  national_positivity_5day <- national_positivity |>
    dplyr::select(country, date_of_symptom_onset_imputed, confirmed_case, total_samples_analysed_daily, spatial_scale, province, health_zone) |>
    dplyr::mutate(date_of_symptom_onset_imputed = as.Date(date_of_symptom_onset_imputed)) |>
    dplyr::filter(!is.na(date_of_symptom_onset_imputed)) |>
    dplyr::arrange(date_of_symptom_onset_imputed) |>
    dplyr::mutate(
      rolling_confirmed = slider::slide_index_sum(
        confirmed_case, date_of_symptom_onset_imputed,
        before = 4L, after = 0L
      ),
      rolling_total = slider::slide_index_sum(
        total_samples_analysed_daily, date_of_symptom_onset_imputed,
        before = 4L, after = 0L
      )
    ) |>
    dplyr::filter(rolling_total > 0) |>
    dplyr::mutate(
      daily_positivity_mean  = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$mean,
      daily_positivity_lower = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$lower,
      daily_positivity_upper = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$upper
    )

  province_positivity_5day <- province_positivity |>
    dplyr::select(province, date_of_symptom_onset_imputed, confirmed_case, total_samples_analysed_daily, spatial_scale, health_zone) |>
    dplyr::mutate(date_of_symptom_onset_imputed = as.Date(date_of_symptom_onset_imputed)) |>
    dplyr::filter(!is.na(date_of_symptom_onset_imputed)) |>
    dplyr::arrange(province, date_of_symptom_onset_imputed) |>
    dplyr::group_by(province) |>
    dplyr::mutate(
      rolling_confirmed = slider::slide_index_sum(
        confirmed_case, date_of_symptom_onset_imputed,
        before = 4L, after = 0L
      ),
      rolling_total = slider::slide_index_sum(
        total_samples_analysed_daily, date_of_symptom_onset_imputed,
        before = 4L, after = 0L
      )
    ) |>
    dplyr::filter(rolling_total > 0) |>
    dplyr::mutate(
      daily_positivity_mean  = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$mean,
      daily_positivity_lower = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$lower,
      daily_positivity_upper = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$upper
    ) |>
    dplyr::ungroup()

  healthzone_positivity_5day <- healthzone_positivity |>
    dplyr::select(health_zone, date_of_symptom_onset_imputed, confirmed_case, total_samples_analysed_daily, spatial_scale, province) |>
    dplyr::mutate(date_of_symptom_onset_imputed = as.Date(date_of_symptom_onset_imputed)) |>
    dplyr::filter(!is.na(date_of_symptom_onset_imputed)) |>
    dplyr::arrange(health_zone, date_of_symptom_onset_imputed) |>
    dplyr::group_by(health_zone) |>
    dplyr::mutate(
      rolling_confirmed = slider::slide_index_sum(
        confirmed_case, date_of_symptom_onset_imputed,
        before = 4L, after = 0L
      ),
      rolling_total = slider::slide_index_sum(
        total_samples_analysed_daily, date_of_symptom_onset_imputed,
        before = 4L, after = 0L
      )
    ) |>
    dplyr::filter(rolling_total > 0) |>
    dplyr::mutate(
      daily_positivity_mean  = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$mean,
      daily_positivity_lower = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$lower,
      daily_positivity_upper = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$upper
    ) |>
    dplyr::ungroup()

  dhis2_linelist_positivity_aggregated <- dplyr::bind_rows(national_positivity_5day, province_positivity_5day, healthzone_positivity_5day) |>
    dplyr::arrange(spatial_scale, date_of_symptom_onset_imputed)

  positivity_aggregated_csv <- file.path(epi_outputs_dir, "rolling_positivity.csv")
  write.csv(dhis2_linelist_positivity_aggregated, positivity_aggregated_csv, row.names = FALSE)
  message("  Wrote: ", positivity_aggregated_csv, " (", nrow(dhis2_linelist_positivity_aggregated), " rows)")

} else {
  positivity_aggregated_csv <- file.path(epi_outputs_dir, "rolling_positivity.csv")
  message("  Skipping positivity_aggregated — lab_analysis_date column absent in ", basename(epi_dir))
  write.csv(data.frame(), positivity_aggregated_csv, row.names = FALSE)
  message("  Wrote: ", positivity_aggregated_csv, " (empty placeholder)")
}

# ---------------------------- Status Aggregation --------------------------------
message("")
message("E. LAB POSITIVITY")

if (!"lab_analysis_date" %in% names(processed_dhis2_linelist)) {
  message("  Skipping lab table — lab_analysis_date column absent in latest processed linelist.")
} else {
  # lab_name_map.csv (assets/maps/) is the authoritative lab code -> long name
  # lookup -- it's saved in Mac OS Roman, not this folder's usual UTF-8, hence
  # the explicit encoding override. Unmapped codes fall back to NA, same as
  # the old recode()'s .default did.
  LAB_NAME_MAP <- load_map_csv("lab_name_map", encoding = "macintosh")
  LAB_LOCATION_MAP <- load_lab_location_map()

  laboratory_table <- processed_dhis2_linelist |>
    dplyr::filter(!lab_analysis_date < as.Date("2026-04-01")) |> #theres a date from february that is probably an error
    dplyr::group_by(lab_name) |>
    dplyr::mutate(earliest_analysed_sample  = min(na.omit(lab_analysis_date))) |>
    dplyr::mutate(total_samples_analysed = n()) |>
    dplyr::group_by(lab_name, lab_analysis_date) |>
    dplyr::mutate(total_samples_analysed_daily = n()) |>
    dplyr::group_by(lab_name, lab_analysis_date, final_mve_case_classification) |>
    dplyr::mutate(num = n()) |>
    dplyr::select(lab_name, lab_analysis_date, earliest_analysed_sample, total_samples_analysed, total_samples_analysed_daily, final_mve_case_classification, num) |>
    dplyr::distinct() |>
    dplyr::filter(!lab_name == "") |>
    dplyr::mutate(final_mve_case_classification = ifelse(final_mve_case_classification == "", "no_result", final_mve_case_classification)) |>
    tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = num) |>
    dplyr::mutate(across(everything(), ~ replace_na(., 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(daily_positivity = confirmed_case/total_samples_analysed_daily) |>
    dplyr::group_by(lab_name) |>
    dplyr::mutate(overall_positivity = sum(confirmed_case)/total_samples_analysed,
                  scaler = max(total_samples_analysed_daily)) |>
    dplyr::ungroup() |>
    dplyr::left_join(LAB_LOCATION_MAP, by = c("lab_name" = "lab_code")) |>
    dplyr::mutate(lab_name_long = unname(LAB_NAME_MAP[lab_name])) |>
    dplyr::mutate(earliest_overall = min(lab_analysis_date),
                  latest_overall = max(lab_analysis_date),
                  lab_analysis_date = as.Date(lab_analysis_date))
  
  message("  Built laboratory_table, ", nrow(laboratory_table), " rows")
  
  # Same Wilson-CI + 5-day-rolling positivity as the national/province/health_zone
  # aggregation above, but by lab and on lab_analysis_date (the lab's own analysis
  # day) rather than onset date -- reuses the counts laboratory_table already built.
  lab_positivity_5day <- laboratory_table |>
    dplyr::select(
      lab_name, lab_name_long, health_zone, province, lab_analysis_date,
      earliest_analysed_sample, confirmed_case, total_samples_analysed_daily
    ) |>
    dplyr::mutate(lab_analysis_date = as.Date(lab_analysis_date)) |>
    dplyr::filter(!is.na(lab_analysis_date)) |>
    dplyr::arrange(lab_name, lab_analysis_date) |>
    dplyr::group_by(lab_name) |>
    dplyr::mutate(
      rolling_confirmed = slider::slide_index_sum(
        confirmed_case, lab_analysis_date,
        before = 4L, after = 0L
      ),
      rolling_total = slider::slide_index_sum(
        total_samples_analysed_daily, lab_analysis_date,
        before = 4L, after = 0L
      )
    ) |>
    dplyr::filter(rolling_total > 0) |>
    dplyr::mutate(
      daily_positivity_mean  = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$mean,
      daily_positivity_lower = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$lower,
      daily_positivity_upper = binom.confint(x = rolling_confirmed, n = rolling_total, methods = "wilson")$upper
    ) |>
    dplyr::ungroup()
  
  lab_positivity_aggregated_csv <- file.path(epi_outputs_dir, "lab_positivity_aggregated.csv")
  write.csv(lab_positivity_5day, lab_positivity_aggregated_csv, row.names = FALSE)
  message("  Wrote: ", lab_positivity_aggregated_csv, " (", nrow(lab_positivity_5day), " rows)")

} # end if ("lab_analysis_date" %in% names(processed_dhis2_linelist))




# --------------------------------- Sandbox ------------------------------------
if (sandbox_mode) {
  
}


# --------------------------------- Cleanup ------------------------------------
dhis2_linelist_aggregated = T

# dhis2_linelist_aggregated_status/dhis2_linelist_positivity_aggregated/
# lab_positivity_5day are the objects 4-make-dashboard-plots.R actually looks
# for via exists(..., inherits = TRUE) when steps are sourced together in one
# session -- keep them so that fast path works (it's otherwise silently dead:
# rm() below would wipe them before step 4 gets a chance to look).
keep_objects <- c(
  keep_objects, "dhis2_linelist_aggregated",
  "dhis2_linelist_aggregated_status", "dhis2_linelist_cumulative_positive_deaths",
  "dhis2_linelist_positivity_aggregated", "lab_positivity_5day"
)

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)

