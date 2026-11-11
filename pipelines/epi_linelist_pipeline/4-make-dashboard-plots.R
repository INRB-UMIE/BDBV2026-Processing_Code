#!/usr/bin/env Rscript
# Step 4: Dashboard plots for the epi linelist pipeline.
#   - Reads status_aggregated.csv (confirmed-case counts) and
#     rolling_positivity.csv (5-day rolling test positivity), both from
#     outputs/<DATE>/, at all three spatial scales 3-aggregate-linelist.R
#     produces (national/province/healthzone), and renders one ggplot2 chart
#     per entry, saved as SVG via svglite under
#     dashboard_plots/confirmed_cases_and_positivity/{national,provincial,healthzone}/
#   - Reads rolling_positivity.csv and renders one positivity chart per entry
#     (ribbon + line on a dedicated y-axis) under
#     dashboard_plots/rolling_positivity/{national,provincial,healthzone}/
#   - Reads cumulative_positive_deaths.csv and renders cumulative-death line
#     plots at the same three spatial scales under
#     dashboard_plots/cumulative_deaths/{national,provincial,healthzone}/
#   - Only province-scale confirmed-case and cumulative-death plots are
#     dashboard-facing: they're listed in outputs/manifest.json -- a single
#     stable (un-dated) path, always overwritten with this run's date and file
#     locations, so the dashboard/trigger-dashboard-rebuild.yml don't need any
#     "find the latest dated folder" logic of their own; they just always read
#     outputs/manifest.json. National and health-zone plots are still generated
#     and saved, but not referenced in the manifest
#   - Recent dates (within pipeline_configs.json's nowcasting.test_days, or
#     DEFAULT_INCOMPLETE_DAYS if that's unavailable) get a shaded background
#     band rather than recoloring the bars, noting "reporting likely
#     incomplete"
#   - Confirmed-case bars are stacked by onset_date_was_imputed: observed
#     onset dates at the bottom in the usual Confirmed color, imputed onset
#     dates on top in a lighter tone (see ONSET_STATUS_SERIES)
#   - Also reads lab_positivity_aggregated.csv and renders one chart per lab
#     (bars = total samples analysed, plus the same rolling-positivity
#     overlay, plus a dashed marker for that lab's first analysed sample),
#     saved flat under dashboard_plots/lab_plots/. Lab location metadata comes
#     from assets/maps/lab_location_map.csv and is written into
#     manifest.json under plots.lab_plots, with convenience indexes under
#     indexes.by_health_zone and indexes.by_province for location-based
#     subsetting (e.g. all Bunia labs + health-zone plots)
#   - Also carries a per-lab GAM-smoothed positivity plot helper
#     (generate_lab_plot()) duplicated from 3-aggregate-linelist.R -- not
#     currently invoked anywhere in this file either (superseded by the
#     lab_positivity_aggregated.csv-based plots above)

# ------------------------- Check if initialised -------------------------------
# Adapted for BDBV2026-Contact_Tracing_Analysis: no local 1-colname-standard.R
# (see note in 3-aggregate-linelist.R). 3-aggregate-linelist.R itself resolves
# the latest synced ctsafe snapshot, so we only need to chain to that.
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

if (!exists("dhis2_linelist_aggregated")) {
  source("pipelines/epi_linelist_pipeline/3-aggregate-linelist.R")
} else if (!dhis2_linelist_aggregated) {
  source("pipelines/epi_linelist_pipeline/3-aggregate-linelist.R")
}

# --------------- Determine which processed dir to handle ----------------------
# resolve_epi_latest_dir() returns the single latest synced date folder. Not
# reused from 3-aggregate-linelist.R's own epi_dir, since that isn't carried
# through its keep_objects cleanup -- resolved again here instead.
CODE_ROOT <- code_root
DATA_ROOT <- data_root
EPI_LATEST_DIR <- resolve_epi_latest_dir()
# All pipeline-produced files (steps 1-3's CSVs/JSON, this step's plots) now
# default to outputs/<DATE>/ -- only the upstream-synced ctsafe linelist stays
# under data/<DATE>/ (EPI_LATEST_DIR is kept only to derive the date stamp).
EPI_OUTPUTS_DIR <- file.path(DATA_ROOT, "outputs", basename(EPI_LATEST_DIR))
OUTPUT_DIR <- file.path(EPI_OUTPUTS_DIR, "dashboard_plots")
STATUS_AGGREGATED_CSV <- file.path(EPI_OUTPUTS_DIR, "status_aggregated.csv")
POSITIVITY_AGGREGATED_CSV <- file.path(EPI_OUTPUTS_DIR, "rolling_positivity.csv")
LAB_POSITIVITY_AGGREGATED_CSV <- file.path(EPI_OUTPUTS_DIR, "lab_positivity_aggregated.csv")
CUMULATIVE_DEATHS_CSV <- file.path(EPI_OUTPUTS_DIR, "cumulative_positive_deaths.csv")

PLOT_TYPE_CONFIRMED <- "confirmed_cases_and_positivity"
PLOT_TYPE_POSITIVITY <- "rolling_positivity"
PLOT_TYPE_DEATHS <- "cumulative_deaths"
PLOT_TYPE_LAB <- "lab_plots"
SPATIAL_SCOPES <- c("national", "provincial", "healthzone")
# A single stable (un-dated) path, sibling to every outputs/<DATE>/ folder --
# overwritten every run with this run's date and file locations, so
# downstream consumers (the dashboard build, trigger-dashboard-rebuild.yml)
# always read the same path instead of needing their own "find the latest
# dated folder" resolution logic.
MANIFEST_PATH <- file.path(DATA_ROOT, "outputs", "manifest.json")

#' Express a path under DATA_ROOT relative to outputs/ -- e.g.
#' ".../outputs/2026-07-15/dashboard_plots/x.svg" -> "2026-07-15/dashboard_plots/x.svg".
#' Used for every path written into outputs/manifest.json, since that file is
#' read from a different machine/checkout than the one that generated it.
relative_to_outputs <- function(path) {
  sub(paste0("^", file.path(DATA_ROOT, "outputs"), "/?"), "", path)
}

spatial_scope_dir <- function(scale) {
  switch(scale, national = "national", province = "provincial", healthzone = "healthzone", scale)
}

plot_type_dir <- function(plot_type, spatial_scope = NULL) {
  if (is.null(spatial_scope)) {
    file.path(OUTPUT_DIR, plot_type)
  } else {
    file.path(OUTPUT_DIR, plot_type, spatial_scope)
  }
}

ensure_plot_dirs <- function() {
  for (plot_type in c(PLOT_TYPE_CONFIRMED, PLOT_TYPE_POSITIVITY, PLOT_TYPE_DEATHS)) {
    for (scope in SPATIAL_SCOPES) {
      dir.create(plot_type_dir(plot_type, scope), recursive = TRUE, showWarnings = FALSE)
    }
  }
  dir.create(plot_type_dir(PLOT_TYPE_LAB), recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------ Aesthetics -------------------------------------
COLOR_INK <- "#2a2a27"
COLOR_MUTED <- "#9c968b"
COLOR_INCOMPLETE <- "#9c968b"
INCOMPLETE_LEGEND_LABEL <- "Reporting likely incomplete"
INCOMPLETE_CAPTION_NOTE <- "Shaded region: dates within the last week (reporting likely incomplete)."
DEFAULT_INCOMPLETE_DAYS <- 7L

# pipeline_configs is already loaded in-memory by 0-setup.R -- read the
# incomplete-reporting window straight from it rather than re-reading any
# JSON files from disk.
incomplete_reporting_days <- suppressWarnings(as.integer(pipeline_configs$epi_linelist$nowcasting$test_days))
if (!length(incomplete_reporting_days) || is.na(incomplete_reporting_days) || incomplete_reporting_days <= 0L) {
  incomplete_reporting_days <- DEFAULT_INCOMPLETE_DAYS
}

SERIES_DEFS <- data.frame(
  id = c("d_pos", "d_neg", "d_inv", "d_blk"),
  label = c("Confirmed", "Negative", "Invalid Result", "Unclassified"),
  color = c("#9B7D4E", "#5b86b3", "#d08163", "#9c968b"),
  stringsAsFactors = FALSE
)

# Confirmed cases are stacked by onset_date_was_imputed -- observed onset
# dates at the bottom in the usual Confirmed color, imputed onset dates
# stacked on top in a lighter tone, so the two are visually distinct without
# a separate legend/plot.
ONSET_STATUS_SERIES <- data.frame(
  id = c("observed", "imputed"),
  label = c("Confirmed (Observed Onset)", "Confirmed (Imputed Onset)"),
  color = c(SERIES_DEFS$color[SERIES_DEFS$id == "d_pos"], "#C9A266"),
  stringsAsFactors = FALSE
)

COLOR_POSITIVITY <- "#5b86b3"
COLOR_DEATHS <- "#7c1d1d"
POSITIVITY_LABEL <- "5-day Rolling Test Positivity"
POSITIVITY_POINT_R <- 1.8
PLOT_WIDTH_IN <- 9
PLOT_HEIGHT_IN <- 4.5
BASE_FONT_SIZE <- 12
COLOR_SAMPLES_ANALYSED <- COLOR_MUTED

palette <- c("#f6f5f2", "#ffffff", "#e7e3db", "#2a2a27", "#B3DEE2",
             "#9c968b", "#7c1d1d", "#b23b2e", "#9b7d4e", "#f1ccc6",
             "#d8d3c9", "#7c4a12", "#9b7d4e", "#5b86b3")
gradient_continuous_palette <- colorRampPalette(c("#f1ccc6", "#b23b2e", "#7c1d1d"))
gradient_continuous_palette_browns <- colorRampPalette(c("#e7e3db", "#9b7d4e", "#7c4a12"))
gradient_continuous_palette_blues <- colorRampPalette(c("#e7e3db", "#B3DEE2", "#5b86b3"))
gradient_continuous_palette_bluebrown <- colorRampPalette(c("#5b86b3", "#B3DEE2", "#e7e3db", "#9b7d4e", "#7c4a12"))

# ------------------------------ Lab Plot Helpers -------------------------------
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

  ggsave(
    file.path(
      data_root, "outputs", basename(epi_dir), "dashboard_plots",
      PLOT_TYPE_LAB, paste0(unique(df$lab_name), ".png")
    ),
    p, height = 5, width = 7.5, units = "in"
  )

}


# ------------------------------ Functions --------------------------------------
incomplete_reporting_dates <- function(dates, n_days = incomplete_reporting_days) {
  dates <- as.character(dates)
  if (!length(dates) || is.null(n_days) || is.na(n_days) || n_days <= 0L) {
    return(character())
  }
  cutoff <- format(Sys.Date() - as.integer(n_days), "%Y-%m-%d")
  dates[dates >= cutoff]
}

slugify <- function(name) {
  # Lowercase alnum tokens joined by hyphens. Do not pass the result through
  # sprintf() — slugs like "ituri" contain "%i", which sprintf would consume.
  parts <- strsplit(tolower(trimws(as.character(name))), "[^a-z0-9]+")[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return("unknown")
  }
  paste(parts, collapse = "-")
}

parse_date <- function(value) {
  if (inherits(value, "Date")) {
    if (length(value) != 1L || is.na(value)) {
      return(NULL)
    }
    yr <- as.integer(format(value, "%Y"))
    if (yr >= 2020L && yr <= 2035L) {
      return(format(value, "%Y-%m-%d"))
    }
    return(NULL)
  }
  value <- trimws(as.character(if (is.null(value)) "" else value))
  if (!nzchar(value)) {
    return(NULL)
  }
  value <- substr(value, 1, 10)
  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")) {
    parsed <- suppressWarnings(tryCatch(
      as.Date(value, format = fmt),
      error = function(e) NA
    ))
    if (!is.na(parsed)) {
      yr <- as.integer(format(parsed, "%Y"))
      if (yr >= 2020L && yr <= 2035L) {
        return(format(parsed, "%Y-%m-%d"))
      }
    }
  }
  NULL
}

to_int <- function(value) {
  if (is.null(value) || is.na(value)) {
    return(0L)
  }
  text <- trimws(as.character(value))
  if (!nzchar(text)) {
    return(0L)
  }
  out <- suppressWarnings(as.integer(round(as.numeric(text))))
  if (is.na(out)) 0L else out
}

# "Positive" means any count column (e.g. observed + imputed onset counts)
# is non-zero -- generalized past a single hardcoded d_pos column since a
# row's count can now be split across multiple sub-series (see
# ONSET_STATUS_SERIES below).
earliest_positive_date <- function(rows) {
  count_cols <- setdiff(names(rows), "date")
  totals <- rowSums(as.data.frame(rows[count_cols]), na.rm = TRUE)
  positive_dates <- rows$date[totals > 0]
  if (!length(positive_dates)) {
    return(NULL)
  }
  outbreak_positives <- positive_dates[positive_dates >= "2026-01-01"]
  pool <- if (length(outbreak_positives)) outbreak_positives else positive_dates
  min(pool)
}

complete_date_series <- function(rows, series_ids) {
  if (!nrow(rows)) {
    return(rows)
  }
  start <- earliest_positive_date(rows)
  if (is.null(start)) {
    return(rows)
  }
  rows <- rows[rows$date >= start, , drop = FALSE]
  end <- max(rows$date)
  all_dates <- format(seq(as.Date(start), as.Date(end), by = "day"), "%Y-%m-%d")
  rows <- rows[match(all_dates, rows$date), , drop = FALSE]
  rows$date <- all_dates
  for (key in series_ids) {
    rows[[key]][is.na(rows[[key]])] <- 0L
  }
  rows
}

# rolling_positivity.csv is long across spatial_scale (national/province/
# healthzone, see 3-aggregate-linelist.R) -- load every scale here; scale (and
# province/health_zone, where relevant) is filtered per-entry in
# scale_positivity_rows() below.
load_positivity_data <- function() {
  if (!file.exists(POSITIVITY_AGGREGATED_CSV)) {
    return(NULL)
  }
  raw <- read.csv(POSITIVITY_AGGREGATED_CSV, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(raw)) {
    return(NULL)
  }
  names(raw) <- tolower(trimws(names(raw)))
  required <- c("spatial_scale", "date_of_symptom_onset_imputed",
                "daily_positivity_mean", "daily_positivity_lower", "daily_positivity_upper")
  if (length(setdiff(required, names(raw)))) {
    return(NULL)
  }
  if ("province" %in% names(raw)) {
    raw$province <- trimws(as.character(raw$province))
  }
  if ("health_zone" %in% names(raw)) {
    raw$health_zone <- trimws(as.character(raw$health_zone))
  }
  raw
}

# scale is one of "national"/"province"/"healthzone"; location is ignored for
# "national" (there's only one row per date at that scale).
scale_positivity_rows <- function(positivity_data, scale, location) {
  if (is.null(positivity_data)) {
    return(NULL)
  }
  scale_df <- positivity_data[positivity_data$spatial_scale == scale, , drop = FALSE]
  location_col <- switch(scale, province = "province", healthzone = "health_zone", NULL)
  if (!is.null(location_col)) {
    if (!location_col %in% names(scale_df)) {
      return(NULL)
    }
    scale_df <- scale_df[scale_df[[location_col]] == location, , drop = FALSE]
  }
  if (!nrow(scale_df)) {
    return(NULL)
  }
  by_date <- list()
  for (i in seq_len(nrow(scale_df))) {
    date <- parse_date(scale_df$date_of_symptom_onset_imputed[[i]])
    if (is.null(date)) {
      next
    }
    by_date[[date]] <- list(
      mean  = suppressWarnings(as.numeric(scale_df$daily_positivity_mean[[i]])),
      lower = suppressWarnings(as.numeric(scale_df$daily_positivity_lower[[i]])),
      upper = suppressWarnings(as.numeric(scale_df$daily_positivity_upper[[i]]))
    )
  }
  if (!length(by_date)) {
    return(NULL)
  }
  rows <- data.frame(
    date      = names(by_date),
    pos_mean  = sapply(by_date, `[[`, "mean"),
    pos_lower = sapply(by_date, `[[`, "lower"),
    pos_upper = sapply(by_date, `[[`, "upper"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  rows[order(as.Date(rows$date)), , drop = FALSE]
}

# status_aggregated.csv is long across spatial_scale -- build one entry per
# (scale, location) pair across all three scales (national has a single
# location; province/healthzone get one entry per distinct value). Each
# entry's rows carry separate observed/imputed confirmed-case counts per
# date (see ONSET_STATUS_SERIES) rather than summing across
# onset_date_was_imputed, so the plot can stack them.
load_plot_entries <- function() {
  if (exists("dhis2_linelist_aggregated_status", inherits = TRUE)) {
    raw <- get("dhis2_linelist_aggregated_status", inherits = TRUE)
  } else if (file.exists(STATUS_AGGREGATED_CSV)) {
    raw <- read.csv(STATUS_AGGREGATED_CSV, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop(
      "Status aggregate not found. Run 3-aggregate-linelist.R or provide ",
      STATUS_AGGREGATED_CSV,
      call. = FALSE
    )
  }

  if (!nrow(raw)) {
    stop("status_aggregated.csv has no rows", call. = FALSE)
  }

  names(raw) <- tolower(trimws(names(raw)))
  required <- c("spatial_scale", "date_of_symptom_onset_imputed", "onset_date_was_imputed", "confirmed_case")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      "status_aggregated.csv missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  entries <- list()
  location_cols <- list(national = NULL, province = "province", healthzone = "health_zone")

  for (scale in names(location_cols)) {
    scale_df <- raw[raw$spatial_scale == scale, , drop = FALSE]
    if (!nrow(scale_df)) {
      next
    }

    location_col <- location_cols[[scale]]
    if (is.null(location_col)) {
      scale_df$.location <- "National"
    } else if (location_col %in% names(scale_df)) {
      scale_df[[location_col]] <- trimws(as.character(scale_df[[location_col]]))
      scale_df <- scale_df[nzchar(scale_df[[location_col]]), , drop = FALSE]
      scale_df$.location <- scale_df[[location_col]]
    } else {
      next
    }

    for (location in sort(unique(scale_df$.location))) {
      loc_df <- scale_df[scale_df$.location == location, , drop = FALSE]
      by_date <- list()

      for (i in seq_len(nrow(loc_df))) {
        date <- parse_date(loc_df$date_of_symptom_onset_imputed[[i]])
        if (is.null(date)) {
          next
        }
        count <- to_int(loc_df$confirmed_case[[i]])
        key <- if (isTRUE(as.logical(loc_df$onset_date_was_imputed[[i]]))) "imputed" else "observed"
        prev <- by_date[[date]]
        if (is.null(prev)) {
          prev <- c(observed = 0L, imputed = 0L)
        }
        prev[key] <- prev[key] + count
        by_date[[date]] <- prev
      }

      if (!length(by_date)) {
        next
      }

      rows <- data.frame(
        date     = names(by_date),
        observed = vapply(by_date, `[[`, integer(1), "observed"),
        imputed  = vapply(by_date, `[[`, integer(1), "imputed"),
        stringsAsFactors = FALSE
      )
      rows <- rows[order(rows$date), , drop = FALSE]

      # Skip locations with no confirmed cases at all (observed or imputed) --
      # nothing to plot, and complete_date_series() would just return an
      # all-zero series unchanged for these anyway.
      if (sum(rows$observed) + sum(rows$imputed) <= 0L) {
        next
      }

      rows <- complete_date_series(rows, ONSET_STATUS_SERIES$id)

      entries[[length(entries) + 1L]] <- list(
        scale = scale,
        slug = slugify(location),
        location = location,
        series = ONSET_STATUS_SERIES,
        rows = rows
      )
    }
  }

  if (!length(entries)) {
    stop("No plottable rows in status_aggregated.csv", call. = FALSE)
  }

  entries
}

complete_cumulative_series <- function(rows) {
  if (!nrow(rows)) {
    return(rows)
  }
  rows <- rows[order(rows$date), , drop = FALSE]
  start <- rows$date[1]
  end <- rows$date[nrow(rows)]
  all_dates <- format(seq(as.Date(start), as.Date(end), by = "day"), "%Y-%m-%d")
  idx <- match(all_dates, rows$date)
  daily <- ifelse(is.na(idx), 0L, rows$daily_deaths[idx])
  cumulative <- integer(length(all_dates))
  last_cum <- 0L
  for (i in seq_along(all_dates)) {
    if (!is.na(idx[i])) {
      last_cum <- rows$cumulative_deaths[idx[i]]
    }
    cumulative[i] <- last_cum
  }
  data.frame(
    date = all_dates,
    daily_deaths = daily,
    cumulative_deaths = cumulative,
    stringsAsFactors = FALSE
  )
}

load_cumulative_deaths_entries <- function() {
  if (exists("dhis2_linelist_cumulative_positive_deaths", inherits = TRUE)) {
    raw <- get("dhis2_linelist_cumulative_positive_deaths", inherits = TRUE)
  } else if (file.exists(CUMULATIVE_DEATHS_CSV)) {
    raw <- read.csv(CUMULATIVE_DEATHS_CSV, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    return(list())
  }

  if (!nrow(raw)) {
    return(list())
  }

  names(raw) <- tolower(trimws(names(raw)))
  # Step 3 now writes deaths by onset date (date_of_symptom_onset_imputed);
  # older outputs used reporting_date. Accept either.
  if (!"date_of_symptom_onset_imputed" %in% names(raw) && "reporting_date" %in% names(raw)) {
    raw$date_of_symptom_onset_imputed <- raw$reporting_date
  }
  required <- c("spatial_scale", "date_of_symptom_onset_imputed", "daily_deaths", "cumulative_deaths")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      "cumulative_positive_deaths.csv missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  entries <- list()
  location_cols <- list(national = NULL, province = "province", healthzone = "health_zone")

  for (scale in names(location_cols)) {
    scale_df <- raw[raw$spatial_scale == scale, , drop = FALSE]
    if (!nrow(scale_df)) {
      next
    }

    location_col <- location_cols[[scale]]
    if (is.null(location_col)) {
      scale_df$.location <- "National"
    } else if (location_col %in% names(scale_df)) {
      scale_df[[location_col]] <- trimws(as.character(scale_df[[location_col]]))
      scale_df <- scale_df[nzchar(scale_df[[location_col]]), , drop = FALSE]
      scale_df$.location <- scale_df[[location_col]]
    } else {
      next
    }

    for (location in sort(unique(scale_df$.location))) {
      loc_df <- scale_df[scale_df$.location == location, , drop = FALSE]
      by_date <- list()

      for (i in seq_len(nrow(loc_df))) {
        date <- parse_date(loc_df$date_of_symptom_onset_imputed[[i]])
        if (is.null(date)) {
          next
        }
        by_date[[date]] <- list(
          daily_deaths = to_int(loc_df$daily_deaths[[i]]),
          cumulative_deaths = to_int(loc_df$cumulative_deaths[[i]])
        )
      }

      if (!length(by_date)) {
        next
      }

      rows <- data.frame(
        date = names(by_date),
        daily_deaths = vapply(by_date, `[[`, integer(1), "daily_deaths"),
        cumulative_deaths = vapply(by_date, `[[`, integer(1), "cumulative_deaths"),
        stringsAsFactors = FALSE
      )
      rows <- rows[order(rows$date), , drop = FALSE]
      rows <- complete_cumulative_series(rows)

      entries[[length(entries) + 1L]] <- list(
        scale = scale,
        slug = slugify(location),
        location = location,
        rows = rows
      )
    }
  }

  entries
}

lookup_lab_location <- function(lab_code, lab_location_df) {
  row <- lab_location_df[lab_location_df$lab_code == lab_code, , drop = FALSE]
  if (!nrow(row)) {
    return(list(health_zone = NA_character_, province = NA_character_))
  }
  list(
    health_zone = row$health_zone[[1]],
    province = row$province[[1]]
  )
}

build_location_indexes <- function(manifest, healthzone_plot_files, lab_location_df) {
  lab_plots <- manifest$plots$lab_plots
  if (is.null(lab_plots)) {
    lab_plots <- list()
  }

  by_health_zone <- list()
  by_province <- list()

  append_lab_code <- function(bucket, key, lab_code) {
    if (is.null(key) || !nzchar(key) || is.null(lab_code) || !nzchar(lab_code)) {
      return(bucket)
    }
    if (is.null(bucket[[key]])) {
      bucket[[key]] <- character()
    }
    bucket[[key]] <- unique(c(bucket[[key]], lab_code))
    bucket
  }

  for (lab_code in names(lab_plots)) {
    lab <- lab_plots[[lab_code]]
    by_health_zone <- append_lab_code(by_health_zone, lab$health_zone, lab_code)
    by_province <- append_lab_code(by_province, lab$province, lab_code)
  }

  lookup_province_for_health_zone <- function(hz) {
    if (is.null(hz) || is.na(hz) || !nzchar(hz)) {
      return(NA_character_)
    }
    idx <- match(hz, lab_location_df$health_zone)
    if (is.na(idx)) {
      return(NA_character_)
    }
    lab_location_df$province[[idx]]
  }

  all_health_zones <- unique(c(
    names(healthzone_plot_files$confirmed_cases_and_positivity),
    names(healthzone_plot_files$rolling_positivity),
    names(healthzone_plot_files$cumulative_deaths),
    names(by_health_zone)
  ))
  all_health_zones <- all_health_zones[!is.na(all_health_zones) & nzchar(all_health_zones)]

  by_health_zone_out <- list()
  for (hz in sort(all_health_zones)) {
    province <- lookup_province_for_health_zone(hz)
    by_health_zone_out[[hz]] <- list(
      province = province,
      lab_codes = if (is.null(by_health_zone[[hz]])) character() else by_health_zone[[hz]],
      confirmed_cases_and_positivity = healthzone_plot_files$confirmed_cases_and_positivity[[hz]],
      rolling_positivity = healthzone_plot_files$rolling_positivity[[hz]],
      cumulative_deaths = healthzone_plot_files$cumulative_deaths[[hz]]
    )
  }

  all_provinces <- unique(c(
    names(manifest$plots$confirmed_cases_and_positivity),
    names(manifest$plots$rolling_positivity),
    names(manifest$plots$cumulative_deaths),
    names(by_province)
  ))
  all_provinces <- all_provinces[nzchar(all_provinces)]

  by_province_out <- list()
  for (prov in sort(all_provinces)) {
    confirmed <- manifest$plots$confirmed_cases_and_positivity[[prov]]
    positivity <- manifest$plots$rolling_positivity[[prov]]
    deaths <- manifest$plots$cumulative_deaths[[prov]]
    by_province_out[[prov]] <- list(
      lab_codes = if (is.null(by_province[[prov]])) character() else by_province[[prov]],
      confirmed_cases_and_positivity = if (is.null(confirmed)) NULL else confirmed$file,
      rolling_positivity = if (is.null(positivity)) NULL else positivity$file,
      cumulative_deaths = if (is.null(deaths)) NULL else deaths$file
    )
  }

  list(
    by_health_zone = by_health_zone_out,
    by_province = by_province_out
  )
}

location_plot_key <- function(scale, location) {
  paste(scale, slugify(location), sep = "::")
}

shared_date_limits <- function(...) {
  date_vectors <- lapply(list(...), function(rows) {
    if (is.null(rows) || !length(rows$date)) {
      return(as.Date(character()))
    }
    as.Date(rows$date)
  })
  all_dates <- do.call(c, date_vectors)
  all_dates <- all_dates[!is.na(all_dates)]
  if (!length(all_dates)) {
    return(NULL)
  }
  as.Date(c(min(all_dates), max(all_dates)))
}

dashboard_plot_theme <- function() {
  theme_minimal(base_size = BASE_FONT_SIZE) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(hjust = 0, color = COLOR_MUTED),
      panel.grid.minor = element_blank()
    )
}

add_incomplete_rect <- function(plot, incomplete_dates, xmax) {
  if (!length(incomplete_dates)) {
    return(plot)
  }
  plot +
    annotate(
      "rect",
      xmin = min(as.Date(incomplete_dates)) - 0.5,
      xmax = as.Date(xmax) + 0.5,
      ymin = -Inf, ymax = Inf,
      fill = COLOR_INCOMPLETE, alpha = 0.25
    )
}

apply_shared_x_scale <- function(plot, limits) {
  if (is.null(limits)) {
    return(plot)
  }
  # coord_cartesian keeps the incomplete-reporting band visible; scale_x_date(limits=...)
  # drops annotate/geom_rect layers that use -Inf/Inf on the y-axis.
  plot + coord_cartesian(xlim = as.Date(limits), expand = TRUE, clip = "on")
}

index_entries_by_location <- function(entries) {
  out <- list()
  for (entry in entries) {
    out[[location_plot_key(entry$scale, entry$location)]] <- entry
  }
  out
}

save_dashboard_svg <- function(plot, out_path) {
  ggsave(out_path, plot = plot, device = "svg", width = PLOT_WIDTH_IN, height = PLOT_HEIGHT_IN, units = "in")
}

# ------------------------------ Build Plots ------------------------------------
message("======== Dashboard Plots: ", basename(EPI_LATEST_DIR), " =========")
message("")
message("A. LOADING AGGREGATED DATA")

ensure_plot_dirs()
lab_location_df <- load_lab_location_map()
plot_entries <- load_plot_entries()
deaths_entries <- load_cumulative_deaths_entries()
positivity_data <- load_positivity_data()
healthzone_plot_files <- list(
  confirmed_cases_and_positivity = list(),
  rolling_positivity = list(),
  cumulative_deaths = list()
)
deaths_by_key <- index_entries_by_location(deaths_entries)
entries_by_scale <- table(vapply(plot_entries, function(e) e$scale, character(1)))
message("  Loaded ", length(plot_entries), " confirmed-case plot entries from ", STATUS_AGGREGATED_CSV,
        " (", paste(sprintf("%s: %d", names(entries_by_scale), entries_by_scale), collapse = ", "), ")")
if (length(deaths_entries)) {
  deaths_by_scale <- table(vapply(deaths_entries, function(e) e$scale, character(1)))
  message("  Loaded ", length(deaths_entries), " cumulative-death plot entries from ", CUMULATIVE_DEATHS_CSV,
          " (", paste(sprintf("%s: %d", names(deaths_by_scale), deaths_by_scale), collapse = ", "), ")")
} else {
  message("  No cumulative-death plot entries found in ", CUMULATIVE_DEATHS_CSV)
}

message("")
message("B. BUILDING CONFIRMED-CASE PLOTS (only provincial plots are dashboard-facing)")

manifest <- list(
  date = basename(EPI_LATEST_DIR),
  source_csv = relative_to_outputs(STATUS_AGGREGATED_CSV),
  series = lapply(seq_len(nrow(SERIES_DEFS)), function(i) {
    list(
      id = SERIES_DEFS$id[i],
      label = SERIES_DEFS$label[i],
      color = SERIES_DEFS$color[i]
    )
  }),
  incomplete_styling = list(
    days = incomplete_reporting_days,
    color = COLOR_INCOMPLETE,
    label = INCOMPLETE_LEGEND_LABEL
  ),
  plots = list(
    confirmed_cases_and_positivity = list(),
    rolling_positivity = list(),
    cumulative_deaths = list(),
    lab_plots = list()
  )
)

for (entry in plot_entries) {
  scale_subdir <- plot_type_dir(PLOT_TYPE_CONFIRMED, spatial_scope_dir(entry$scale))
  filename <- paste0("daily_onset_", entry$slug, ".svg")
  out_path <- file.path(scale_subdir, filename)
  incomplete_dates <- incomplete_reporting_dates(entry$rows$date, incomplete_reporting_days)
  pos_rows <- scale_positivity_rows(positivity_data, entry$scale, entry$location)
  death_entry <- deaths_by_key[[location_plot_key(entry$scale, entry$location)]]
  x_limits <- shared_date_limits(
    entry$rows,
    if (!is.null(death_entry)) death_entry$rows else NULL,
    pos_rows
  )

  plot_df <- entry$rows |>
    tidyr::pivot_longer(dplyr::all_of(entry$series$id), names_to = "series_id", values_to = "count") |>
    dplyr::mutate(
      date = as.Date(date),
      fill_label = factor(entry$series$label[match(series_id, entry$series$id)], levels = entry$series$label)
    )

  fill_values <- setNames(entry$series$color, entry$series$label)
  fill_breaks <- entry$series$label

  caption <- if (length(incomplete_dates)) {
    paste0("Confirmed cases by date of symptom onset. ", INCOMPLETE_CAPTION_NOTE)
  } else {
    "Confirmed cases by date of symptom onset"
  }

  x_max <- if (is.null(x_limits)) max(plot_df$date) else x_limits[[2]]
  p <- ggplot(plot_df, aes(x = date, y = count, fill = fill_label))
  p <- add_incomplete_rect(p, incomplete_dates, x_max)
  p <- p +
    geom_col(width = 0.9, position = position_stack(reverse = TRUE)) +
    scale_fill_manual(name = NULL, values = fill_values, breaks = fill_breaks) +
    scale_y_continuous(name = "Cases") +
    labs(
      title = sprintf("Daily Cases by Symptom Onset - %s", entry$location),
      x = NULL,
      caption = caption
    ) +
    dashboard_plot_theme()
  p <- apply_shared_x_scale(p, x_limits)

  save_dashboard_svg(p, out_path)

  if (entry$scale == "province") {
    manifest$plots$confirmed_cases_and_positivity[[entry$location]] <- list(
      file = relative_to_outputs(out_path),
      title = sprintf("Daily Cases by Symptom Onset - %s", entry$location),
      caption = caption
    )
  } else if (entry$scale == "healthzone") {
    healthzone_plot_files$confirmed_cases_and_positivity[[entry$location]] <- relative_to_outputs(out_path)
  }
  message(sprintf(
    "  Wrote: %s (%d days)%s",
    sub(paste0("^", DATA_ROOT, "/?"), "", out_path),
    nrow(entry$rows),
    if (entry$scale == "province") " [dashboard]" else ""
  ))
}

message("")
message("C. BUILDING ROLLING-POSITIVITY PLOTS (only provincial plots are dashboard-facing)")

for (entry in plot_entries) {
  pos_rows <- scale_positivity_rows(positivity_data, entry$scale, entry$location)
  if (is.null(pos_rows) || !nrow(pos_rows)) {
    next
  }

  scale_subdir <- plot_type_dir(PLOT_TYPE_POSITIVITY, spatial_scope_dir(entry$scale))
  filename <- paste0("rolling_positivity_", entry$slug, ".svg")
  out_path <- file.path(scale_subdir, filename)
  death_entry <- deaths_by_key[[location_plot_key(entry$scale, entry$location)]]
  incomplete_dates <- unique(c(
    incomplete_reporting_dates(entry$rows$date, incomplete_reporting_days),
    incomplete_reporting_dates(pos_rows$date, incomplete_reporting_days),
    if (!is.null(death_entry)) incomplete_reporting_dates(death_entry$rows$date, incomplete_reporting_days) else character()
  ))
  x_limits <- shared_date_limits(
    entry$rows,
    if (!is.null(death_entry)) death_entry$rows else NULL,
    pos_rows
  )

  pos_plot_df <- pos_rows |>
    dplyr::mutate(
      date = as.Date(date),
      mean_pct  = pmin(1, pmax(0, pos_mean)) * 100,
      lower_pct = pmin(1, pmax(0, pos_lower)) * 100,
      upper_pct = pmin(1, pmax(0, pos_upper)) * 100
    )

  caption <- if (length(incomplete_dates)) {
    paste0("5-day rolling test positivity by date of symptom onset. ", INCOMPLETE_CAPTION_NOTE)
  } else {
    "5-day rolling test positivity by date of symptom onset"
  }

  x_max <- if (is.null(x_limits)) max(pos_plot_df$date) else x_limits[[2]]
  p <- ggplot(pos_plot_df, aes(x = date))
  p <- add_incomplete_rect(p, incomplete_dates, x_max)
  p <- p +
    geom_ribbon(aes(ymin = lower_pct, ymax = upper_pct), fill = COLOR_POSITIVITY, alpha = 0.35) +
    geom_line(aes(y = mean_pct, color = POSITIVITY_LABEL), linewidth = 0.9) +
    geom_point(aes(y = mean_pct, color = POSITIVITY_LABEL), size = POSITIVITY_POINT_R) +
    scale_color_manual(name = NULL, values = setNames(COLOR_POSITIVITY, POSITIVITY_LABEL)) +
    scale_y_continuous(name = "Sample Positivity (%)", limits = c(0, NA)) +
    labs(
      title = sprintf("5-day Rolling Test Positivity - %s", entry$location),
      x = NULL,
      caption = caption
    ) +
    dashboard_plot_theme()
  p <- apply_shared_x_scale(p, x_limits)

  save_dashboard_svg(p, out_path)

  if (entry$scale == "province") {
    manifest$plots$rolling_positivity[[entry$location]] <- list(
      file = relative_to_outputs(out_path),
      title = sprintf("5-day Rolling Test Positivity - %s", entry$location),
      caption = caption
    )
  } else if (entry$scale == "healthzone") {
    healthzone_plot_files$rolling_positivity[[entry$location]] <- relative_to_outputs(out_path)
  }
  message(sprintf(
    "  Wrote: %s (%d days)%s",
    sub(paste0("^", DATA_ROOT, "/?"), "", out_path),
    nrow(pos_rows),
    if (entry$scale == "province") " [dashboard]" else ""
  ))
}

message("")
message("D. BUILDING CUMULATIVE-DEATH PLOTS (only provincial plots are dashboard-facing)")

cases_by_key <- index_entries_by_location(plot_entries)

for (entry in deaths_entries) {
  scale_subdir <- plot_type_dir(PLOT_TYPE_DEATHS, spatial_scope_dir(entry$scale))
  filename <- paste0("cumulative_deaths_", entry$slug, ".svg")
  out_path <- file.path(scale_subdir, filename)
  incomplete_dates <- incomplete_reporting_dates(entry$rows$date, incomplete_reporting_days)
  case_entry <- cases_by_key[[location_plot_key(entry$scale, entry$location)]]
  pos_rows <- scale_positivity_rows(positivity_data, entry$scale, entry$location)
  x_limits <- shared_date_limits(
    if (!is.null(case_entry)) case_entry$rows else NULL,
    entry$rows,
    pos_rows
  )

  plot_df <- entry$rows |>
    dplyr::mutate(date = as.Date(date))
  death_days <- plot_df[plot_df$daily_deaths > 0L, , drop = FALSE]

  caption <- if (length(incomplete_dates)) {
    paste0(
      "Cumulative confirmed deaths by date of symptom onset (death alerts with confirmed MVE classification). ",
      INCOMPLETE_CAPTION_NOTE
    )
  } else {
    "Cumulative confirmed deaths by date of symptom onset (death alerts with confirmed MVE classification)"
  }

  x_max <- if (is.null(x_limits)) max(plot_df$date) else x_limits[[2]]
  p <- ggplot(plot_df, aes(x = date, y = cumulative_deaths, group = 1))
  p <- add_incomplete_rect(p, incomplete_dates, x_max)
  p <- p + geom_line(color = COLOR_DEATHS, linewidth = 0.9)
  if (nrow(death_days)) {
    p <- p + geom_point(data = death_days, color = COLOR_DEATHS, size = POSITIVITY_POINT_R)
  }
  p <- p +
    scale_y_continuous(name = "Cumulative Deaths") +
    labs(
      title = sprintf("Cumulative Confirmed Deaths - %s", entry$location),
      x = NULL,
      caption = caption
    ) +
    dashboard_plot_theme()
  p <- apply_shared_x_scale(p, x_limits)

  save_dashboard_svg(p, out_path)

  if (entry$scale == "province") {
    manifest$plots$cumulative_deaths[[entry$location]] <- list(
      file = relative_to_outputs(out_path),
      title = sprintf("Cumulative Confirmed Deaths - %s", entry$location),
      caption = caption
    )
  } else if (entry$scale == "healthzone") {
    healthzone_plot_files$cumulative_deaths[[entry$location]] <- relative_to_outputs(out_path)
  }
  message(sprintf(
    "  Wrote: %s (%d days)%s",
    sub(paste0("^", DATA_ROOT, "/?"), "", out_path),
    nrow(entry$rows),
    if (entry$scale == "province") " [dashboard]" else ""
  ))
}

message("")
message("E. BUILDING LAB PLOTS")

lab_output_dir <- plot_type_dir(PLOT_TYPE_LAB)

if (exists("lab_positivity_5day", inherits = TRUE)) {
  lab_positivity_raw <- get("lab_positivity_5day", inherits = TRUE)
} else if (file.exists(LAB_POSITIVITY_AGGREGATED_CSV)) {
  lab_positivity_raw <- read.csv(LAB_POSITIVITY_AGGREGATED_CSV, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  lab_positivity_raw <- NULL
}

if (is.null(lab_positivity_raw) || !nrow(lab_positivity_raw)) {
  message("  Skipping lab plots -- ", LAB_POSITIVITY_AGGREGATED_CSV, " not found or empty.")
} else {
  names(lab_positivity_raw) <- tolower(trimws(names(lab_positivity_raw)))
  required <- c("lab_name", "lab_analysis_date", "earliest_analysed_sample",
                "total_samples_analysed_daily", "daily_positivity_mean",
                "daily_positivity_lower", "daily_positivity_upper")
  missing <- setdiff(required, names(lab_positivity_raw))
  if (length(missing)) {
    message("  Skipping lab plots -- missing columns: ", paste(missing, collapse = ", "))
  } else {
    lab_positivity_raw$lab_analysis_date <- as.Date(lab_positivity_raw$lab_analysis_date)
    lab_positivity_raw$earliest_analysed_sample <- as.Date(lab_positivity_raw$earliest_analysed_sample)

    # Every lab plot shares the same x-axis minimum -- the first sample
    # analysed across ALL labs, not just this one -- so labs are visually
    # comparable on how long each has been running relative to the others.
    global_earliest_sample <- min(lab_positivity_raw$earliest_analysed_sample, na.rm = TRUE)
    global_latest_sample <- max(lab_positivity_raw$lab_analysis_date, na.rm = TRUE)
    lab_x_limits <- c(global_earliest_sample, global_latest_sample)

    for (lab_name in sort(unique(lab_positivity_raw$lab_name))) {
      lab_df <- lab_positivity_raw[lab_positivity_raw$lab_name == lab_name, , drop = FALSE]
      lab_df <- lab_df[order(lab_df$lab_analysis_date), , drop = FALSE]
      if (!nrow(lab_df)) {
        next
      }

      lab_title <- if ("lab_name_long" %in% names(lab_df) && !is.na(lab_df$lab_name_long[1]) && nzchar(lab_df$lab_name_long[1])) {
        lab_df$lab_name_long[1]
      } else {
        lab_name
      }
      earliest_sample <- lab_df$earliest_analysed_sample[1]
      max_total <- max(1, max(lab_df$total_samples_analysed_daily, na.rm = TRUE))
      incomplete_dates <- incomplete_reporting_dates(lab_df$lab_analysis_date, incomplete_reporting_days)
      lab_df <- lab_df |>
        dplyr::mutate(
          mean_scaled  = pmin(1, pmax(0, daily_positivity_mean))  * max_total,
          lower_scaled = pmin(1, pmax(0, daily_positivity_lower)) * max_total,
          upper_scaled = pmin(1, pmax(0, daily_positivity_upper)) * max_total
        )

      out_path <- file.path(lab_output_dir, paste0("lab_", slugify(lab_name), ".svg"))
      p <- ggplot(lab_df, aes(x = lab_analysis_date, y = total_samples_analysed_daily))
      p <- add_incomplete_rect(p, incomplete_dates, lab_x_limits[[2]])
      p <- p +
        geom_col(fill = COLOR_SAMPLES_ANALYSED, width = 0.9) +
        geom_ribbon(aes(ymin = lower_scaled, ymax = upper_scaled), fill = COLOR_POSITIVITY, alpha = 0.35) +
        geom_line(aes(y = mean_scaled, color = POSITIVITY_LABEL), linewidth = 0.9) +
        geom_point(aes(y = mean_scaled, color = POSITIVITY_LABEL), size = POSITIVITY_POINT_R) +
        scale_color_manual(name = NULL, values = setNames(COLOR_POSITIVITY, POSITIVITY_LABEL)) +
        geom_vline(xintercept = as.numeric(earliest_sample), linetype = "dashed", color = COLOR_INK) +
        annotate(
          "text", x = earliest_sample, y = max_total * 1.02,
          label = paste0("Earliest Sample: ", format(earliest_sample, "%Y-%m-%d")),
          hjust = -0.05, vjust = 0, size = 3.2, color = COLOR_INK
        ) +
        scale_y_continuous(
          name = "Samples Analysed",
          sec.axis = sec_axis(~ . / max_total, name = "Sample Positivity (%)")
        ) +
        labs(
          title = sprintf("Samples Analysed and Test Positivity - %s", lab_title),
          x = NULL,
          caption = "Samples analysed and test positivity by analysis date"
        ) +
        dashboard_plot_theme()
      p <- apply_shared_x_scale(p, lab_x_limits)
      save_dashboard_svg(p, out_path)

      lab_loc <- lookup_lab_location(lab_name, lab_location_df)
      if ("health_zone" %in% names(lab_df) && !is.na(lab_df$health_zone[1]) && nzchar(lab_df$health_zone[1])) {
        lab_loc$health_zone <- lab_df$health_zone[1]
      }
      if ("province" %in% names(lab_df) && !is.na(lab_df$province[1]) && nzchar(lab_df$province[1])) {
        lab_loc$province <- lab_df$province[1]
      }

      plot_title <- sprintf("Samples Analysed and Test Positivity - %s", lab_title)
      plot_caption <- "Samples analysed and test positivity by analysis date"
      manifest$plots$lab_plots[[lab_name]] <- list(
        file = relative_to_outputs(out_path),
        title = plot_title,
        caption = plot_caption,
        lab_code = lab_name,
        lab_name_long = lab_title,
        health_zone = lab_loc$health_zone,
        province = lab_loc$province
      )

      message(sprintf(
        "  Wrote: %s (%d days)%s",
        sub(paste0("^", DATA_ROOT, "/?"), "", out_path),
        nrow(lab_df),
        if (nzchar(lab_loc$health_zone)) paste0(" [", lab_loc$health_zone, "]") else ""
      ))
    }
  }
}

message("")
message("F. WRITING MANIFEST")

manifest$indexes <- build_location_indexes(manifest, healthzone_plot_files, lab_location_df)

# Written to outputs/manifest.json (stable, un-dated) rather than inside this
# run's own dashboard_plots/ -- see MANIFEST_PATH comment above.
writeLines(
  jsonlite::toJSON(manifest, pretty = TRUE, auto_unbox = TRUE),
  MANIFEST_PATH,
  useBytes = TRUE
)
message("  Wrote: ", relative_to_outputs(MANIFEST_PATH), " (date: ", manifest$date, ")")

# --------------------------------- Cleanup ------------------------------------
dashboard_plots_manifest <- manifest
dashboard_plots_complete <- TRUE

keep_objects <- c(keep_objects, "dashboard_plots_manifest", "dashboard_plots_complete")

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
