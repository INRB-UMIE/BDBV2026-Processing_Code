##### Check if initialised #####
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

# ---- Determine which processed dirs to aggregate ----------------------------
# dhis2_force_stems has no source in this repo (no Shiny app here); this branch
# is kept only for parity with the upstream script and expects date-folder
# names (e.g. "2026-07-14") if ever set. Normally we just take the latest
# synced date folder.
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
message("=== Aggregating: ", basename(epi_dir), " ===")

# ----------------------------- Load Linelist ----------------------------------
processed_dhis2_linelist <- read.csv(file.path(epi_dir, "dhis2_processed_linelist_ctsafe.csv"),
                                     stringsAsFactors = FALSE, na.strings = "")

dhis2_linelist_province_aggregated <- processed_dhis2_linelist |>
  dplyr::filter(final_mve_case_classification == "confirmed_case") |>
  dplyr::filter(!date_of_symptom_onset < as.Date("2026-04-01")) |> #these dates are likely errors, we don't want them on the dashboard
  dplyr::group_by(province, date_of_symptom_onset) |>
  dplyr::mutate(total_positive = n(),
                date_of_symptom_onset = as.Date(date_of_symptom_onset)) |>
  dplyr::select(province, date_of_symptom_onset, total_positive) |>
  dplyr::distinct() |>
  dplyr::arrange(date_of_symptom_onset)

write.csv(dhis2_linelist_province_aggregated, file.path(epi_dir, "province_aggregated.csv"), row.names = FALSE)

# positivity_aggregated.csv requires lab_analysis_date — skip gracefully for older linelists
if ("lab_analysis_date" %in% names(processed_dhis2_linelist)) {

  # Uses onset date (lab_analysis_date used only for filter / earliest sample)
  dhis2_linelist_province_positivity <- processed_dhis2_linelist |>
    dplyr::filter(!date_of_symptom_onset < as.Date("2026-04-01")) |>
    dplyr::filter(!is.na(lab_analysis_date)) |>
    dplyr::group_by(province) |>
    dplyr::mutate(total_samples_analysed = n()) |>
    dplyr::filter(total_samples_analysed > 10) |>
    dplyr::mutate(earliest_analysed_sample = min(na.omit(na_if(lab_analysis_date, "")))) |>
    dplyr::group_by(province, date_of_symptom_onset) |>
    dplyr::mutate(total_samples_analysed_daily = n()) |>
    dplyr::group_by(province, date_of_symptom_onset, final_mve_case_classification) |>
    dplyr::mutate(num = n()) |>
    dplyr::select(province, date_of_symptom_onset, earliest_analysed_sample, total_samples_analysed, total_samples_analysed_daily, final_mve_case_classification, num) |>
    dplyr::distinct() |>
    dplyr::filter(!province == "") |>
    dplyr::mutate(final_mve_case_classification = ifelse(final_mve_case_classification == "", "no_result", final_mve_case_classification)) |>
    tidyr::pivot_wider(names_from = final_mve_case_classification, values_from = num) |>
    dplyr::mutate(across(everything(), ~ replace_na(., 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(daily_positivity_mean = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$mean,
                  daily_positivity_lower = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$lower,
                  daily_positivity_upper = binom.confint(x = confirmed_case, n = total_samples_analysed_daily, methods = "wilson")$upper)

  # 5 day rolling
  dhis2_linelist_province_positivity_5day <- dhis2_linelist_province_positivity |>
    dplyr::select(province, date_of_symptom_onset, confirmed_case, total_samples_analysed_daily) |>
    dplyr::mutate(date_of_symptom_onset = as.Date(date_of_symptom_onset)) |>
    dplyr::filter(!is.na(date_of_symptom_onset)) |>
    dplyr::arrange(province, date_of_symptom_onset) |>
    dplyr::group_by(province) |>
    dplyr::mutate(
      rolling_confirmed = slider::slide_index_sum(
        confirmed_case, date_of_symptom_onset,
        before = 4L, after = 0L
      ),
      rolling_total = slider::slide_index_sum(
        total_samples_analysed_daily, date_of_symptom_onset,
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

  write.csv(dhis2_linelist_province_positivity_5day, file.path(epi_dir, "positivity_aggregated.csv"), row.names = FALSE)

} else {
  message("  Skipping positivity_aggregated — lab_analysis_date column absent in ", basename(epi_dir))
  write.csv(data.frame(), file.path(epi_dir, "positivity_aggregated.csv"), row.names = FALSE)
}

dhis2_linelist_status_aggregated <- processed_dhis2_linelist |>
  dplyr::filter(samples_received == 1) |>
  dplyr::group_by(date_of_symptom_onset, final_mve_case_classification) |>
  dplyr::mutate(total_samples = n()) |>
  dplyr::select(date_of_symptom_onset, final_mve_case_classification, total_samples) |>
  dplyr::distinct()

write.csv(dhis2_linelist_status_aggregated, file.path(epi_dir, "status_aggregated.csv"), row.names = FALSE)

healthzones_with_positives <- processed_dhis2_linelist |>
  dplyr::filter(final_mve_case_classification == "confirmed_case") |>
  dplyr::group_by(health_zone) |>
  dplyr::mutate(total_pos = n()) |>
  dplyr::filter(date_of_symptom_onset == min(na.omit(date_of_symptom_onset))) |>
  dplyr::select(date_of_symptom_onset, health_zone, total_pos) |>
  dplyr::distinct() 

write.csv(healthzones_with_positives, file.path(epi_dir, "healthzones_with_positives.csv"), row.names = FALSE)

} # end for (epi_dir in target_epi_dirs)


# ----------------------------- Make Lab Table ---------------------------------
if (!"lab_analysis_date" %in% names(processed_dhis2_linelist)) {
  message("Skipping lab table — lab_analysis_date column absent in latest processed linelist.")
} else {
laboratory_table <- processed_dhis2_linelist |>
  dplyr::filter(!lab_analysis_date < as.Date("2026-04-01")) |> #theres a date from february that is probably an error
  dplyr::group_by(lab_name) |>
  dplyr::mutate(earliest_analysed_sample  = min(na.omit(na_if(lab_analysis_date, "")))) |>
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
  dplyr::mutate(health_zone = recode(lab_name, 
                                  LPSPBN = "Bunia",
                                  INRBK = "Kinshasa",
                                  INRBGM = "Goma",
                                  LBNYA = "Nyankunde",
                                  INRBATB = "Beni",
                                  Lab_MGL = "Mongbwalu",
                                  INRBANTB = "Butembo",
                                  .default = NA_character_
                                  )) |>
  dplyr::mutate(lab_name_long = recode(lab_name, 
                                     LPSPBN = "Laboratoire Provincial de Santé Publique de Bunia",
                                     INRBK = "INRB Kinshasa",
                                     INRBGM = "INRB Goma",
                                     LBNYA = "Laboratoire de Nyankunde",
                                     INRBATB = "INRB Antenne Beni",
                                     Lab_MGL = "Laboratoire de Mongbwalu",
                                     INRBANTB = "INRB Antenne Butembo",
                                     .default = NA_character_
  )) |>
  dplyr::ungroup() |>
  dplyr::mutate(earliest_overall = min(lab_analysis_date),
                latest_overall = max(lab_analysis_date),
                lab_analysis_date = as.Date(lab_analysis_date))

} # end if ("lab_analysis_date" %in% names(processed_dhis2_linelist))


palette <- c("#f6f5f2", "#ffffff", "#e7e3db", "#2a2a27", "#B3DEE2",
             "#9c968b", "#7c1d1d", "#b23b2e", "#9b7d4e", "#f1ccc6",
             "#d8d3c9", "#7c4a12", "#9b7d4e", "#5b86b3")
gradient_continuous_palette <- colorRampPalette(c("#f1ccc6", "#b23b2e", "#7c1d1d"))
gradient_continuous_palette_browns <- colorRampPalette(c("#e7e3db", "#9b7d4e", "#7c4a12"))
gradient_continuous_palette_blues <- colorRampPalette(c("#e7e3db", "#B3DEE2", "#5b86b3"))
gradient_continuous_palette_bluebrown <- colorRampPalette(c("#5b86b3", "#B3DEE2", "#e7e3db", "#9b7d4e", "#7c4a12"))


generate_lab_plot <- function(df) {
  # Find the scaler we used
  scale <- unique(df$scaler)
  
  # fit a gam
  df_for_modelling <- df |>
    dplyr::mutate(date_num = as.numeric(lab_analysis_date))
  
  library(mgcv)
  
  fit <- gam(
    cbind(confirmed_case, total_samples_analysed_daily - confirmed_case) ~ s(date_num),
    data = df_for_modelling,
    family = binomial(link = "logit")
  )
  
  pred <- predict(fit, type = "response", se.fit = T)
  
  df_for_modelling$mean <- pred$fit
  df_for_modelling$se <- pred$se.fit
  
  plotting_df <- df_for_modelling |>
    dplyr::mutate(lower = mean - 1.96 * se,
                  upper = mean + 1.96 * se) |>
    dplyr::mutate(mean_pred_scaled = mean * scale,
                  lower_pred_scaled = lower * scale,
                  upper_pred_scaled = upper * scale)
  
  # get the prediction of currently
  current_predicted_positivity <- plotting_df |>
    dplyr::filter(date_num == max(date_num))
  
  
  marker <- data.frame(x = rep(unique(as.Date(df$earliest_analysed_sample)), 2), y = c(0, scale), 
                       text = paste0("Earliest Sample: ", unique(df$earliest_analysed_sample)))
  
  t <- paste0("Samples analysed and test positivity over time in ", unique(df$lab_name_long))
  st <- paste0("Test positivity overall: ", round(unique(df$overall_positivity), 3), "; modelled on ", current_predicted_positivity$lab_analysis_date, ": ", round(current_predicted_positivity$mean, 3), " (", round(current_predicted_positivity$lower, 3), ", ", round(current_predicted_positivity$upper, 3), ")")
  
  p <- ggplot(plotting_df, aes(x = lab_analysis_date)) +
    geom_line(data = marker, aes(x = x, y = y*1.1), linetype = 2) +
    geom_bar(aes(y = total_samples_analysed_daily), col = "#f1ccc6", fill = "#f1ccc6", stat = "identity") + 
    geom_point(aes(y = daily_positivity*scaler), col = "#5b86b3") +
    geom_line(aes(y = mean_pred_scaled), col = "#5b86b3") +
    geom_ribbon(aes(ymin = lower_pred_scaled, ymax = upper_pred_scaled), fill = "#B3DEE2", alpha = 0.4) +
    scale_x_date(limits = as.Date(c("2026-04-30", "2026-06-25"))) +
    geom_text(data = marker[2,], aes(x = x, y = y*1.1, label = text)) +
    scale_y_continuous(
      name = "Samples Analysed",
      sec.axis = sec_axis(~./scale, name = "Sample Positivity (%)")
    ) +
    labs(x = "Analysis Date")+
    ggtitle(stringr::str_wrap(t, width = 80), subtitle = st) +
    theme_classic()
  
  ggsave(paste0("outputs/dashboard_plots/", unique(df$lab_name), ".png"), p, height = 5, width = 7.5, units = "in")

}


# Commented out for now
# for (l in unique(laboratory_table$lab_name)[c(1,3,4,5,7,8)]) {
#   print(l)
#   generate_lab_plot(dplyr::filter(laboratory_table, lab_name == l))
# }




# --------------------------------- Sandbox ------------------------------------
if (sandbox_mode) {
  library(HDInterval)
  date_focus <- processed_dhis2_linelist |>
    dplyr::mutate(across(contains("date"), as.Date)) |>
    dplyr::transmute(reporting_date = reporting_date - date_of_symptom_onset,
                    investigation_datetime = investigation_datetime - date_of_symptom_onset,
                    date_of_hospitalisation_start = date_of_hospitalisation_start - date_of_symptom_onset,
                    date_of_hospitalisation_end = date_of_hospitalisation_end - date_of_symptom_onset,
                    date_of_hospitalisation_end_2 = date_of_hospitalisation_end_2 - date_of_symptom_onset,
                    date_of_notification = date_of_notification - date_of_symptom_onset,
                    date_of_sample_collection = date_of_sample_collection - date_of_symptom_onset) |>
    tidyr::pivot_longer(everything(), names_to = "Date_Label", values_to = "Days_Post_Symptoms") |>
    dplyr::filter(!is.na(Days_Post_Symptoms)) |>
    dplyr::filter(!(Days_Post_Symptoms > 365 | Days_Post_Symptoms < -150)) |>
    dplyr::group_by(Date_Label) |>
    dplyr::mutate(Median = median(as.numeric(Days_Post_Symptoms)),
                  Lower_95 = HDInterval::hdi(as.numeric(Days_Post_Symptoms), 0.95)[1],
                  Upper_95 = HDInterval::hdi(as.numeric(Days_Post_Symptoms), 0.95)[2]) |>
    dplyr::arrange(Median) |>
    dplyr::select(Date_Label, Median, Lower_95, Upper_95) |>
    tidyr::pivot_longer(!c(Date_Label, Median), names_to = "Confidence_Interval_Boundary", values_to = "Confidence_Interval")
  
  date_focus_factored <- date_focus |>
    dplyr::mutate(Date_Label = factor(Date_Label, levels = rev(unique(date_focus$Date_Label))))

  ggplot(date_focus_factored, aes(y = Date_Label, group = Date_Label)) +
    geom_line(aes(x = Confidence_Interval, col = Date_Label), show.legend = F) +
    geom_point(aes(x = Median, col = Date_Label), size = 2, shape = 21, fill = "white", show.legend = F) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    labs(x = "Days post symptom onset date = median (point) and 0.95 HDI", y = "Date label post cleaning") +
    theme_classic()
  
}


# -----------------------------
# Cleanup
# -----------------------------

dhis2_linelist_aggregated = T

keep_objects <- c(keep_objects, "dhis2_linelist_aggregated", "dhis2_linelist_province_aggregated")

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)

