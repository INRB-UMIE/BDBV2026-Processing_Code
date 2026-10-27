#!/usr/bin/env Rscript
# Step 6: Reporting backfill (how long until daily counts "settle").
#   - Reads status_aggregated.csv (3-aggregate-linelist.R's output) from every
#     past outputs/<DATE>/ folder up to and including this run's date, i.e.
#     the same onset-date epi curve as it looked in each earlier export
#   - Treats this run's counts as the reference ("eventual") count for each
#     onset date, but only for onset dates at least
#     reporting_backfill.settled_after_days old -- newer dates are still
#     filling in themselves, so they can't be used as the reference yet
#   - For every earlier export, works out how many days had passed since each
#     onset date ("days since onset" = export date - onset date) and what
#     share of the reference count had already been reported by then
#   - Writes reporting_backfill/backfill_snapshots.csv: one row per export x
#     onset date x location x classification (count then vs reference count)
#   - Writes reporting_backfill/backfill_completeness_by_lag.csv: completeness
#     by days since onset -- pooled (sum of counts / sum of reference counts)
#     plus the median and 10th/90th percentile across individual onset dates
#   - Writes reporting_backfill/backfill_summary.json: days since onset until
#     each reporting_backfill.completeness_thresholds level is reached
#     (pooled), per location, alongside the current nowcasting.test_days
#     window the dashboard shades as "reporting likely incomplete"
#   - Renders two plots (SVG + PNG) per location under reporting_backfill/plots/:
#     the epi curve as seen in successive exports, and the completeness curve
#   - Caveat: this run's own counts aren't truly final either (a small trickle
#     of very late reports carries on past settled_after_days), so
#     completeness here is a slight overestimate
#   - Caveat: onset dates with counts in an earlier export but no row at all
#     in this run's export (e.g. an imputed onset that has since moved) aren't
#     compared -- only onset dates present in this run are used as references
#   - Caveat: imputed onset dates (2-date_imputation.R) are redrawn whenever
#     the linelist changes, so they can move between exports. Both observed
#     and imputed onsets are counted together here, as on the dashboard
#   - sandbox_mode (off by default) is a placeholder for exploratory ad hoc
#     work, not run automatically -- currently empty

# ------------------------- Check if initialised -------------------------------
# Only needs step 3's status_aggregated.csv on disk (for this run's date and
# for every earlier date) -- it doesn't re-source 3-aggregate-linelist.R the
# way 4-make-dashboard-plots.R does, since each CI step runs as its own
# Rscript process and that would just redo step 3's work.
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

sandbox_mode <- F

# --------------- Determine which processed dir to handle ----------------------
# resolve_epi_latest_dir() returns the single latest synced date folder (or the
# EPI_PIPELINE_DATE-pinned one). Its date is this run's reference export date.
epi_dir <- resolve_epi_latest_dir()
reference_date <- as.Date(basename(epi_dir))

epi_outputs_dir <- file.path(data_root, "outputs", basename(epi_dir))
backfill_dir <- file.path(epi_outputs_dir, "reporting_backfill")
backfill_plots_dir <- file.path(backfill_dir, "plots")
dir.create(backfill_plots_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------- Config ------------------------------------
backfill_cfg <- pipeline_configs$epi_linelist$reporting_backfill
if (is.null(backfill_cfg)) {
  stop("pipeline_configs.json has no epi_linelist.reporting_backfill block.", call. = FALSE)
}
BACKFILL_CLASSIFICATIONS <- as.character(backfill_cfg$classifications)
BACKFILL_SCALES <- as.character(backfill_cfg$spatial_scales)
BACKFILL_ONSET_START <- as.Date(backfill_cfg$onset_start)
SETTLED_AFTER_DAYS <- as.integer(backfill_cfg$settled_after_days)
MAX_DAYS_SINCE_ONSET <- as.integer(backfill_cfg$max_days_since_onset)
MIN_REFERENCE_COUNT <- as.integer(backfill_cfg$min_reference_count)
COMPLETENESS_THRESHOLDS <- as.numeric(backfill_cfg$completeness_thresholds)
SNAPSHOT_SPACING_DAYS <- as.integer(backfill_cfg$snapshot_spacing_days)
N_SNAPSHOTS <- as.integer(backfill_cfg$n_snapshots)
MIN_LOCATION_CASES_FOR_PLOT <- as.integer(backfill_cfg$min_location_cases_for_plot)

# The dashboard's current "reporting likely incomplete" window, reported
# alongside the estimates so the two can be compared directly.
# Falls back to the same default as 4-make-dashboard-plots.R's
# DEFAULT_INCOMPLETE_DAYS if the config value is missing/invalid.
incomplete_reporting_days <- suppressWarnings(as.integer(pipeline_configs$epi_linelist$nowcasting$test_days))
if (!length(incomplete_reporting_days) || is.na(incomplete_reporting_days) || incomplete_reporting_days <= 0L) {
  incomplete_reporting_days <- 7L
}

# Onset dates up to this one have had at least SETTLED_AFTER_DAYS to fill in,
# so this run's counts for them are used as the reference.
reference_onset_end <- reference_date - SETTLED_AFTER_DAYS

# ------------------------------ Aesthetics -------------------------------------
# Mirrors 4-make-dashboard-plots.R's palette/theme so these sit alongside the
# dashboard plots without looking out of place.
COLOR_INK <- "#2a2a27"
COLOR_MUTED <- "#9c968b"
COLOR_INCOMPLETE <- "#9c968b"
COLOR_COMPLETENESS <- "#5b86b3"
COLOR_THRESHOLD <- "#b23b2e"
PLOT_WIDTH_IN <- 9
PLOT_HEIGHT_IN <- 4.5
BASE_FONT_SIZE <- 12
ROLLING_DAYS <- 7

backfill_plot_theme <- function() {
  theme_minimal(base_size = BASE_FONT_SIZE) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.caption = element_text(hjust = 0, color = COLOR_MUTED),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = element_blank()
    )
}

save_backfill_plot <- function(plot, stem) {
  svg_path <- file.path(backfill_plots_dir, paste0(stem, ".svg"))
  png_path <- file.path(backfill_plots_dir, paste0(stem, ".png"))
  ggsave(svg_path, plot = plot, device = "svg", width = PLOT_WIDTH_IN, height = PLOT_HEIGHT_IN, units = "in")
  ggsave(png_path, plot = plot, device = "png", width = PLOT_WIDTH_IN, height = PLOT_HEIGHT_IN, units = "in", dpi = 150)
  message("  Wrote: ", svg_path, " (+ .png)")
}

location_slug <- function(scale, location) {
  slug <- if (scale == "national") "national" else paste0(scale, "_", location)
  slug <- tolower(gsub("[^A-Za-z0-9]+", "-", slug))
  gsub("(^-|-$)", "", slug)
}

# ------------------------------ Helpers ----------------------------------------
#' Read one export's status_aggregated.csv and collapse it to daily counts per
#' location for the configured scales/classifications. Column order has varied
#' between runs (pivot_wider order follows the data), so everything is matched
#' by name; observed and imputed onset rows are summed together.
read_status_snapshot <- function(path, export_date) {
  status <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                            col_types = readr::cols(.default = readr::col_character()))
  # A classification column that's absent from an export means "not
  # recorded", not "zero cases" -- skip that export rather than letting it
  # count as 0% complete (all exports so far have every column).
  missing_cls <- setdiff(BACKFILL_CLASSIFICATIONS, names(status))
  if (length(missing_cls)) {
    warning("Skipping ", path, ": no column(s) ", paste(missing_cls, collapse = ", "), call. = FALSE)
    return(NULL)
  }

  status |>
    dplyr::filter(spatial_scale %in% BACKFILL_SCALES) |>
    dplyr::mutate(
      onset_date = as.Date(date_of_symptom_onset_imputed),
      location = dplyr::case_when(
        spatial_scale == "national" ~ "National",
        spatial_scale == "province" ~ province,
        spatial_scale == "healthzone" ~ health_zone,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(onset_date), !is.na(location)) |>
    tidyr::pivot_longer(dplyr::all_of(BACKFILL_CLASSIFICATIONS), names_to = "classification", values_to = "count") |>
    dplyr::mutate(count = suppressWarnings(as.numeric(count))) |>
    dplyr::group_by(spatial_scale, location, classification, onset_date) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(export_date = export_date)
}

#' First days-since-onset at which pooled completeness reaches `threshold`
#' (NA if it never does within MAX_DAYS_SINCE_ONSET).
days_to_threshold <- function(lag_df, threshold) {
  hit <- lag_df$days_since_onset[!is.na(lag_df$pooled_completeness) & lag_df$pooled_completeness >= threshold]
  if (!length(hit)) NA_integer_ else as.integer(min(hit))
}

# ------------------------------ Load Exports -----------------------------------
message("======== Reporting Backfill: ", basename(epi_dir), " =========")
message("")
message("A. LOADING PAST STATUS_AGGREGATED EXPORTS")

reference_csv <- file.path(epi_outputs_dir, "status_aggregated.csv")
if (!file.exists(reference_csv)) {
  stop("No status_aggregated.csv for this run (", reference_csv, "). ",
       "Run 3-aggregate-linelist.R first.", call. = FALSE)
}

output_date_dirs <- list.dirs(file.path(data_root, "outputs"), full.names = FALSE, recursive = FALSE)
# vapply() drops the Date class, so restore it before comparing/sorting.
output_dates <- as.Date(vapply(output_date_dirs, parse_synced_date_dir, as.Date(NA)), origin = "1970-01-01")
has_status <- vapply(output_date_dirs, function(d) {
  file.exists(file.path(data_root, "outputs", d, "status_aggregated.csv"))
}, logical(1))
# Only exports up to this run's date, so an EPI_PIPELINE_DATE-pinned rerun
# reproduces what that date's run would have seen.
use_dates <- !is.na(output_dates) & has_status & output_dates <= reference_date
export_dirs <- output_date_dirs[use_dates]
export_dates <- output_dates[use_dates]
message("  Found ", length(export_dirs), " exports with status_aggregated.csv (",
        format(min(export_dates)), " to ", format(max(export_dates)), ")")

# Indexed rather than purrr::map2() over export_dates directly, so each
# export_date keeps its Date class.
snapshots_raw <- dplyr::bind_rows(lapply(seq_along(export_dirs), function(j) {
  read_status_snapshot(file.path(data_root, "outputs", export_dirs[j], "status_aggregated.csv"), export_dates[j])
}))

# ------------------------- Compare Against Reference ---------------------------
message("")
message("B. COMPARING EACH EXPORT AGAINST THIS RUN'S COUNTS")

reference_counts <- snapshots_raw |>
  dplyr::filter(export_date == reference_date) |>
  dplyr::select(spatial_scale, location, classification, onset_date, reference_count = count)

# Every export x onset date combination where the onset date is in the
# settled window and had already happened by that export. An onset date
# missing from an earlier export means 0 reported by then, so the snapshot is
# completed against the reference dates rather than inner-joined.
backfill_snapshots <- reference_counts |>
  dplyr::filter(onset_date >= BACKFILL_ONSET_START, onset_date <= reference_onset_end) |>
  tidyr::crossing(export_date = export_dates) |>
  dplyr::filter(export_date >= onset_date, export_date < reference_date) |>
  dplyr::left_join(snapshots_raw, by = c("spatial_scale", "location", "classification", "onset_date", "export_date")) |>
  dplyr::mutate(
    count = tidyr::replace_na(count, 0),
    days_since_onset = as.integer(export_date - onset_date),
    completeness = ifelse(reference_count > 0, count / reference_count, NA_real_)
  ) |>
  dplyr::filter(days_since_onset <= MAX_DAYS_SINCE_ONSET) |>
  dplyr::arrange(spatial_scale, location, classification, export_date, onset_date)

backfill_snapshots_csv <- file.path(backfill_dir, "backfill_snapshots.csv")
write.csv(backfill_snapshots, backfill_snapshots_csv, row.names = FALSE)
message("  Wrote: ", backfill_snapshots_csv, " (", nrow(backfill_snapshots), " rows)")

# Pooled completeness is the headline number (robust to small daily counts);
# the per-date median/percentiles only use dates with enough reference cases
# to show how much individual days vary around it.
backfill_by_lag <- backfill_snapshots |>
  dplyr::group_by(spatial_scale, location, classification, days_since_onset) |>
  # NOTE: the per-date percentiles must come before count/reference_count
  # are summed -- summarise() evaluates in order, so anything after
  # `reference_count = sum(...)` would see the summed value, not the per-date one.
  dplyr::summarise(
    n_onset_dates = dplyr::n(),
    median_completeness = stats::median(completeness[reference_count >= MIN_REFERENCE_COUNT], na.rm = TRUE),
    p10_completeness = stats::quantile(completeness[reference_count >= MIN_REFERENCE_COUNT], 0.1, na.rm = TRUE, names = FALSE),
    p90_completeness = stats::quantile(completeness[reference_count >= MIN_REFERENCE_COUNT], 0.9, na.rm = TRUE, names = FALSE),
    count = sum(count),
    reference_count = sum(reference_count),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pooled_completeness = ifelse(reference_count > 0, count / reference_count, NA_real_),
    dplyr::across(dplyr::ends_with("_completeness"), ~ ifelse(is.nan(.x), NA_real_, .x))
  ) |>
  dplyr::relocate(count, reference_count, pooled_completeness, .after = n_onset_dates)

backfill_by_lag_csv <- file.path(backfill_dir, "backfill_completeness_by_lag.csv")
write.csv(backfill_by_lag, backfill_by_lag_csv, row.names = FALSE)
message("  Wrote: ", backfill_by_lag_csv, " (", nrow(backfill_by_lag), " rows)")

# ---------------------------------- Summary ------------------------------------
message("")
message("C. SUMMARY")

summary_groups <- backfill_by_lag |>
  dplyr::group_by(spatial_scale, location, classification) |>
  dplyr::group_split()

summary_entries <- lapply(summary_groups, function(g) {
  g <- dplyr::arrange(g, days_since_onset)
  thresholds <- setNames(
    lapply(COMPLETENESS_THRESHOLDS, function(t) days_to_threshold(g, t)),
    paste0("days_to_", round(100 * COMPLETENESS_THRESHOLDS), "pct")
  )
  at_window <- g$pooled_completeness[g$days_since_onset == incomplete_reporting_days]
  c(
    list(
      spatial_scale = g$spatial_scale[1],
      location = g$location[1],
      classification = g$classification[1],
      reference_cases = sum(reference_counts$reference_count[
        reference_counts$spatial_scale == g$spatial_scale[1] &
          reference_counts$location == g$location[1] &
          reference_counts$classification == g$classification[1] &
          reference_counts$onset_date >= BACKFILL_ONSET_START &
          reference_counts$onset_date <= reference_onset_end
      ])
    ),
    thresholds,
    list(completeness_at_current_window = if (length(at_window)) round(at_window, 3) else NA_real_)
  )
})

backfill_summary <- list(
  reference_date = format(reference_date),
  n_exports = length(export_dates),
  first_export = format(min(export_dates)),
  onset_window = c(format(BACKFILL_ONSET_START), format(reference_onset_end)),
  settled_after_days = SETTLED_AFTER_DAYS,
  current_incomplete_window_days = incomplete_reporting_days,
  note = paste(
    "days_to_XXpct = first days since onset at which pooled completeness",
    "(count then / count in this run) reaches XX%. NA = not reached within",
    MAX_DAYS_SINCE_ONSET, "days. completeness_at_current_window = pooled",
    "completeness at nowcasting.test_days days since onset."
  ),
  locations = summary_entries
)

backfill_summary_json <- file.path(backfill_dir, "backfill_summary.json")
writeLines(
  jsonlite::toJSON(backfill_summary, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  backfill_summary_json
)
message("  Wrote: ", backfill_summary_json)

for (entry in summary_entries) {
  if (entry$spatial_scale != "national") next
  message(
    "  ", entry$location, " / ", entry$classification, ": ",
    paste(
      sprintf("%s%% by day %s", round(100 * COMPLETENESS_THRESHOLDS),
              vapply(entry[paste0("days_to_", round(100 * COMPLETENESS_THRESHOLDS), "pct")],
                     function(x) if (is.na(x)) "NA" else as.character(x), character(1))),
      collapse = ", "
    ),
    " | at the current ", incomplete_reporting_days, "-day window: ",
    round(100 * entry$completeness_at_current_window), "% complete"
  )
}

# ----------------------------------- Plots -------------------------------------
message("")
message("D. BUILDING PLOTS")

# Exports nearest to every SNAPSHOT_SPACING_DAYS back from this run, so the
# epi-curve plot shows a handful of evenly spaced "what we saw then" curves.
snapshot_targets <- reference_date - SNAPSHOT_SPACING_DAYS * (seq_len(N_SNAPSHOTS) - 1L)
snapshot_exports <- unique(as.Date(vapply(snapshot_targets, function(t) {
  candidates <- export_dates[export_dates <= t]
  if (!length(candidates)) NA_real_ else as.numeric(max(candidates))
}, numeric(1)), origin = "1970-01-01"))
# Each needs at least ROLLING_DAYS onset dates for the rolling average.
snapshot_exports <- sort(snapshot_exports[!is.na(snapshot_exports) &
                                            snapshot_exports >= BACKFILL_ONSET_START + ROLLING_DAYS - 1])

# Only plot locations with enough reference cases for the curves to mean
# anything -- small provinces are still in the CSVs/summary JSON.
plot_locations <- data.frame(
  spatial_scale = vapply(summary_entries, function(e) e$spatial_scale, character(1)),
  location = vapply(summary_entries, function(e) e$location, character(1)),
  classification = vapply(summary_entries, function(e) e$classification, character(1)),
  reference_cases = vapply(summary_entries, function(e) as.numeric(e$reference_cases), numeric(1)),
  stringsAsFactors = FALSE
) |>
  dplyr::filter(spatial_scale == "national" | reference_cases >= MIN_LOCATION_CASES_FOR_PLOT)
# Empty when there's nothing to compare yet (e.g. only one export, or no onset
# dates old enough to be settled) -- the loop below then just does nothing.
message("  Plotting ", nrow(plot_locations), " location(s) (national, plus any with >= ",
        MIN_LOCATION_CASES_FOR_PLOT, " reference cases)")

for (i in seq_len(nrow(plot_locations))) {
  loc <- plot_locations[i, ]
  stem_base <- paste0(location_slug(loc$spatial_scale, loc$location), "_", loc$classification)
  class_label <- tools::toTitleCase(gsub("_", " ", loc$classification))

  # --- Epi curve as seen in successive exports ---
  # Every onset date from BACKFILL_ONSET_START up to each export's own date,
  # so days with no cases show as 0 rather than being skipped by the line.
  curve_grid <- dplyr::bind_rows(lapply(seq_along(snapshot_exports), function(j) {
    e <- snapshot_exports[j]
    data.frame(export_date = e, onset_date = seq(BACKFILL_ONSET_START, e, by = "day"))
  }))
  curve_df <- snapshots_raw |>
    dplyr::filter(
      spatial_scale == loc$spatial_scale, location == loc$location,
      classification == loc$classification, export_date %in% snapshot_exports
    ) |>
    dplyr::select(export_date, onset_date, count) |>
    dplyr::right_join(curve_grid, by = c("export_date", "onset_date")) |>
    dplyr::mutate(count = tidyr::replace_na(count, 0)) |>
    dplyr::group_by(export_date) |>
    dplyr::arrange(onset_date, .by_group = TRUE) |>
    dplyr::mutate(rolling = as.numeric(stats::filter(count, rep(1 / ROLLING_DAYS, ROLLING_DAYS), sides = 2))) |>
    dplyr::ungroup() |>
    dplyr::mutate(export_label = factor(format(export_date, "Export %d %b"), levels = format(sort(snapshot_exports), "Export %d %b")))

  if (nrow(curve_df)) {
    p_curve <- ggplot(curve_df, aes(x = onset_date, y = rolling, color = export_label)) +
      geom_vline(data = dplyr::distinct(curve_df, export_date, export_label),
                 aes(xintercept = export_date, color = export_label), linetype = "dotted", linewidth = 0.4, show.legend = FALSE) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      scale_color_viridis_d(end = 0.9) +
      scale_x_date(date_labels = "%d %b") +
      labs(
        title = paste0("Same epidemic curve, seen in successive exports (", loc$location, ")"),
        subtitle = "Each curve drops off before its export date (dotted line), then fills in later",
        x = "Date of symptom onset",
        y = paste0(class_label, " per day (", ROLLING_DAYS, "-day average)"),
        caption = "The drop at each curve's right edge is late reporting (backfill), not a real decline."
      ) +
      guides(color = guide_legend(nrow = 1)) +
      backfill_plot_theme()
    save_backfill_plot(p_curve, paste0(stem_base, "_epicurve_by_export"))
  }

  # --- Completeness by days since onset ---
  lag_df <- backfill_by_lag |>
    dplyr::filter(spatial_scale == loc$spatial_scale, location == loc$location, classification == loc$classification)

  if (nrow(lag_df)) {
    p_lag <- ggplot(lag_df, aes(x = days_since_onset)) +
      annotate("rect", xmin = 0, xmax = incomplete_reporting_days, ymin = -Inf, ymax = Inf,
               fill = COLOR_INCOMPLETE, alpha = 0.25) +
      annotate("text", x = incomplete_reporting_days / 2, y = 3, size = 3, color = COLOR_INK,
               label = paste0("current\n\"incomplete\"\n(", incomplete_reporting_days, " days)"), vjust = 0) +
      geom_hline(yintercept = 100 * COMPLETENESS_THRESHOLDS, linetype = "dashed", color = COLOR_MUTED, linewidth = 0.4) +
      geom_ribbon(aes(ymin = 100 * p10_completeness, ymax = 100 * p90_completeness, fill = "Middle 80% of onset dates"),
                  alpha = 0.3, na.rm = TRUE) +
      geom_line(aes(y = 100 * pooled_completeness, color = "All onset dates pooled"), linewidth = 1.1, na.rm = TRUE) +
      scale_fill_manual(values = COLOR_COMPLETENESS) +
      scale_color_manual(values = COLOR_COMPLETENESS) +
      scale_x_continuous(expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, MAX_DAYS_SINCE_ONSET), ylim = c(0, 105)) +
      labs(
        title = paste0("How complete is a day's count, by days since that day? (", loc$location, ")"),
        subtitle = paste0("Compared with the ", format(reference_date, "%d %b"), " export; dashed lines at ",
                          paste0(round(100 * COMPLETENESS_THRESHOLDS), "%", collapse = " / ")),
        x = "Days between symptom onset and export",
        y = paste0("% of eventual ", tolower(class_label), " count reported"),
        caption = paste0("Onset dates ", format(BACKFILL_ONSET_START, "%d %b"), " to ", format(reference_onset_end, "%d %b"),
                         ", across ", length(export_dates), " exports. This run's counts are not quite final, so completeness is slightly overestimated.")
      ) +
      backfill_plot_theme()
    save_backfill_plot(p_lag, paste0(stem_base, "_completeness_by_days_since_onset"))
  }
}

# --------------------------------- Sandbox ------------------------------------
if (sandbox_mode) {

}

# --------------------------------- Cleanup ------------------------------------
reporting_backfill_done = T

keep_objects <- c(keep_objects, "reporting_backfill_done")

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
