# -------------------------  Check if Initialised  -----------------------------
# Adapted for BDBV2026-Contact_Tracing_Analysis: no local 1-colname-standard.R
# (see note in 2-aggregate-linelist.R). 2-aggregate-linelist.R itself resolves
# the latest synced ctsafe snapshot, so we only need to chain to that.
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

if (!exists("dhis2_linelist_aggregated")) {
  source("pipelines/epi_linelist_pipeline/2-aggregate-linelist.R")
} else if (!dhis2_linelist_aggregated) {
  source("pipelines/epi_linelist_pipeline/2-aggregate-linelist.R")
}

# ------------------------------  Aesthetics  ----------------------------------
COLOR_INK <- "#2a2a27"
COLOR_MUTED <- "#9c968b"
COLOR_BORDER <- "#e7e3db"
COLOR_AXIS <- "#d8d3c9"
COLOR_INCOMPLETE <- "#9c968b"
INCOMPLETE_LEGEND_LABEL <- "Reporting likely incomplete"
INCOMPLETE_CAPTION_NOTE <- "Grey bars: dates within the last week (reporting likely incomplete)."
DEFAULT_INCOMPLETE_DAYS <- 7L

SERIES_DEFS <- data.frame(
  id = c("d_pos", "d_neg", "d_inv", "d_blk"),
  label = c("Confirmed", "Negative", "Invalid Result", "Unclassified"),
  color = c("#b23b2e", "#5b86b3", "#d08163", "#9c968b"),
  stringsAsFactors = FALSE
)

CHART_W <- 720
MARGIN_LEFT <- 56
MARGIN_RIGHT <- 12
MARGIN_TOP <- 14
PLOT_H <- 330
X_LABEL_HEIGHT <- 54
LEGEND_HEIGHT <- 34
CAPTION_HEIGHT <- 18
FONT_AXIS <- 13
FONT_TICK <- 12
FONT_LEGEND <- 14
FONT_CAPTION <- 13
LEGEND_SWATCH <- 12
MARGIN_RIGHT_POSITIVITY <- 48L
COLOR_POSITIVITY <- "#5b86b3"
POSITIVITY_POINT_R <- 3.5
POSITIVITY_CI_W <- 1.5

# ------------------------------  Functions  ----------------------------------

load_incomplete_reporting_days <- function() {
  for (path in c(NOWCASTING_METADATA_JSON, PIPELINE_CONFIGS_JSON)) {
    if (!file.exists(path)) {
      next
    }
    cfg <- tryCatch(
      jsonlite::fromJSON(path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(cfg)) {
      next
    }
    nowcasting_cfg <- if (!is.null(cfg$epi_linelist)) {
      cfg$epi_linelist$nowcasting
    } else {
      cfg$nowcasting
    }
    test_days <- nowcasting_cfg$test_days
    if (!is.null(test_days) && !is.na(suppressWarnings(as.integer(test_days)))) {
      out <- as.integer(test_days)
      if (out > 0L) {
        return(out)
      }
    }
  }
  DEFAULT_INCOMPLETE_DAYS
}

incomplete_reporting_dates <- function(dates, n_days = load_incomplete_reporting_days()) {
  dates <- as.character(dates)
  if (!length(dates) || n_days <= 0L) {
    return(character())
  }
  cutoff <- format(Sys.Date() - as.integer(n_days), "%Y-%m-%d")
  dates[dates >= cutoff]
}

plot_title <- function(province) {
  sprintf("Daily Cases by Symptom Onset - %s", province)
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

plot_filename <- function(slug) {
  paste0("daily_onset_", slug, ".svg")
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

earliest_positive_date <- function(rows) {
  positive_dates <- rows$date[rows$d_pos > 0]
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

load_positivity_data <- function() {
  if (!file.exists(POSITIVITY_AGGREGATED_CSV)) {
    return(NULL)
  }
  raw <- read.csv(POSITIVITY_AGGREGATED_CSV, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(raw)) {
    return(NULL)
  }
  names(raw) <- tolower(trimws(names(raw)))
  date_col_candidates <- c("lab_analysis_date", "date_of_symptom_onset")
  required <- c("province", "daily_positivity_mean", "daily_positivity_lower", "daily_positivity_upper")
  has_date_col <- any(date_col_candidates %in% names(raw))
  if (!has_date_col || length(setdiff(required, names(raw)))) {
    return(NULL)
  }
  raw$province <- trimws(as.character(raw$province))
  raw
}

province_positivity_rows <- function(positivity_data, province) {
  if (is.null(positivity_data)) {
    return(NULL)
  }
  date_col_candidates <- c("lab_analysis_date", "date_of_symptom_onset")
  date_col <- date_col_candidates[date_col_candidates %in% names(positivity_data)][1]
  if (is.na(date_col)) {
    return(NULL)
  }
  prov_df <- positivity_data[positivity_data$province == province, , drop = FALSE]
  if (!nrow(prov_df)) {
    return(NULL)
  }
  by_date <- list()
  for (i in seq_len(nrow(prov_df))) {
    date <- parse_date(prov_df[[date_col]][[i]])
    if (is.null(date)) {
      next
    }
    by_date[[date]] <- list(
      mean  = suppressWarnings(as.numeric(prov_df$daily_positivity_mean[[i]])),
      lower = suppressWarnings(as.numeric(prov_df$daily_positivity_lower[[i]])),
      upper = suppressWarnings(as.numeric(prov_df$daily_positivity_upper[[i]]))
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

load_province_plot_entries <- function() {
  if (exists("dhis2_linelist_province_aggregated", inherits = TRUE)) {
    raw <- get("dhis2_linelist_province_aggregated", inherits = TRUE)
  } else if (file.exists(PROVINCE_AGGREGATED_CSV)) {
    raw <- read.csv(PROVINCE_AGGREGATED_CSV, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop(
      "Province aggregate not found. Run 2-aggregate-linelist.R or provide ",
      PROVINCE_AGGREGATED_CSV,
      call. = FALSE
    )
  }
  
  if (!nrow(raw)) {
    stop("province_aggregated.csv has no rows", call. = FALSE)
  }
  
  names(raw) <- tolower(trimws(names(raw)))
  required <- c("province", "date_of_symptom_onset", "total_positive")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      "province_aggregated.csv missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  raw$province <- trimws(as.character(raw$province))
  raw <- raw[nzchar(raw$province), , drop = FALSE]
  
  confirmed_series <- SERIES_DEFS[SERIES_DEFS$id == "d_pos", , drop = FALSE]
  entries <- list()
  
  for (province in sort(unique(raw$province))) {
    prov_df <- raw[raw$province == province, , drop = FALSE]
    by_date <- list()
    
    for (i in seq_len(nrow(prov_df))) {
      date <- parse_date(prov_df$date_of_symptom_onset[[i]])
      if (is.null(date)) {
        next
      }
      count <- to_int(prov_df$total_positive[[i]])
      by_date[[date]] <- to_int(by_date[[date]]) + count
    }
    
    if (!length(by_date)) {
      next
    }
    
    rows <- data.frame(
      date = names(by_date),
      d_pos = unname(unlist(by_date)),
      stringsAsFactors = FALSE
    )
    rows <- rows[order(rows$date), , drop = FALSE]
    rows <- complete_date_series(rows, confirmed_series$id)
    
    entries[[length(entries) + 1L]] <- list(
      slug = slugify(province),
      province = province,
      series = confirmed_series,
      rows = rows
    )
  }
  
  if (!length(entries)) {
    stop("No plottable province rows in province_aggregated.csv", call. = FALSE)
  }
  
  entries
}

build_chart_svg <- function(
    province,
    rows,
    series,
    incomplete_dates = incomplete_reporting_dates(rows$date),
    positivity_rows = NULL
) {
  has_positivity <- !is.null(positivity_rows) && nrow(positivity_rows) > 0L
  margin_right <- if (has_positivity) MARGIN_RIGHT_POSITIVITY else MARGIN_RIGHT
  plot_w <- CHART_W - MARGIN_LEFT - margin_right
  plot_top <- MARGIN_TOP
  axis_y <- plot_top + PLOT_H
  legend_y <- axis_y + X_LABEL_HEIGHT + 10
  svg_h <- legend_y + LEGEND_HEIGHT + CAPTION_HEIGHT

  dates <- rows$date
  n <- length(dates)
  incomplete_dates <- intersect(as.character(dates), as.character(incomplete_dates))
  show_incomplete_legend <- length(incomplete_dates) > 0L
  totals <- apply(rows[, series$id, drop = FALSE], 1L, sum)
  max_total <- max(max(totals), 1L)

  gap <- if (n > 1L) 1 else 0
  bar_w <- max(2.0, (plot_w - gap * (n - 1L)) / n)

  parts <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" role="img" aria-label="%s">',
      CHART_W, svg_h, plot_title(province)
    ),
    "<style>",
    "text { font-family: -apple-system, system-ui, 'Segoe UI', Helvetica, Arial, sans-serif; }",
    "</style>",
    sprintf('<rect width="%d" height="%d" fill="transparent"/>', CHART_W, svg_h)
  )

  # Left y-axis labels and gridlines (case counts)
  for (t in 0:4) {
    val <- round(max_total * t / 4)
    y <- plot_top + PLOT_H - (PLOT_H * t / 4)
    parts <- c(
      parts,
      sprintf(
        '<text x="%d" y="%.1f" text-anchor="end" fill="%s" font-size="%d">%d</text>',
        MARGIN_LEFT - 4L, y + 3, COLOR_MUTED, FONT_AXIS, val
      )
    )
    if (t > 0L) {
      parts <- c(
        parts,
        sprintf(
          '<line x1="%d" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="0.5"/>',
          MARGIN_LEFT, y, MARGIN_LEFT + plot_w, y, COLOR_BORDER
        )
      )
    }
  }

  # X-axis baseline
  parts <- c(
    parts,
    sprintf(
      '<line x1="%d" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1"/>',
      MARGIN_LEFT, axis_y, MARGIN_LEFT + plot_w, axis_y, COLOR_AXIS
    )
  )

  # Case count bars
  for (i in seq_len(n)) {
    x <- MARGIN_LEFT + (i - 1L) * (bar_w + gap)
    y_top <- axis_y
    row <- rows[i, , drop = FALSE]
    bar_incomplete <- dates[[i]] %in% incomplete_dates
    for (j in seq_len(nrow(series))) {
      key <- series$id[j]
      color <- if (bar_incomplete) COLOR_INCOMPLETE else series$color[j]
      v <- to_int(row[[key]])
      if (v <= 0L) {
        next
      }
      h <- (v / max_total) * PLOT_H
      y_top <- y_top - h
      parts <- c(
        parts,
        sprintf(
          '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"/>',
          x, y_top, bar_w, h, color
        )
      )
    }
  }

  # Right y-axis labels and positivity overlay (0–1 scale)
  if (has_positivity) {
    for (t in 0:4) {
      val <- t / 4
      y <- axis_y - (PLOT_H * t / 4)
      label <- if (t == 4L) "1" else if (t == 0L) "0" else sprintf("%.2f", val)
      parts <- c(
        parts,
        sprintf(
          '<text x="%d" y="%.1f" text-anchor="start" fill="%s" font-size="%d">%s</text>',
          MARGIN_LEFT + plot_w + 4L, y + 3L, COLOR_MUTED, FONT_AXIS, label
        )
      )
    }

    # Collect valid positivity points in date order
    pos_pts <- list()
    for (i in seq_len(nrow(positivity_rows))) {
      pos_date <- positivity_rows$date[i]
      idx <- match(pos_date, dates)
      if (is.na(idx)) next
      x_center <- MARGIN_LEFT + (idx - 1L) * (bar_w + gap) + bar_w / 2
      pos_pts[[length(pos_pts) + 1L]] <- list(
        x       = x_center,
        y_mean  = axis_y - max(0, min(1, positivity_rows$pos_mean[i]))  * PLOT_H,
        y_lower = axis_y - max(0, min(1, positivity_rows$pos_lower[i])) * PLOT_H,
        y_upper = axis_y - max(0, min(1, positivity_rows$pos_upper[i])) * PLOT_H
      )
    }

    if (length(pos_pts) > 0L) {
      # CI ribbon: upper bound forward, lower bound backward
      upper_pts <- sapply(pos_pts, function(p) sprintf("%.1f,%.1f", p$x, p$y_upper))
      lower_pts <- sapply(rev(pos_pts), function(p) sprintf("%.1f,%.1f", p$x, p$y_lower))
      parts <- c(parts, sprintf(
        '<polygon points="%s" fill="%s" fill-opacity="0.35" stroke="none"/>',
        paste(c(upper_pts, lower_pts), collapse = " "), COLOR_POSITIVITY
      ))

      # Connecting line through means
      mean_pts <- sapply(pos_pts, function(p) sprintf("%.1f,%.1f", p$x, p$y_mean))
      parts <- c(parts, sprintf(
        '<polyline points="%s" fill="none" stroke="%s" stroke-width="1.5"/>',
        paste(mean_pts, collapse = " "), COLOR_POSITIVITY
      ))

      # Mean circles
      for (p in pos_pts) {
        parts <- c(parts, sprintf(
          '<circle cx="%.1f" cy="%.1f" r="%.1f" fill="%s"/>',
          p$x, p$y_mean, POSITIVITY_POINT_R, COLOR_POSITIVITY
        ))
      }
    }
  }

  # X-axis date labels
  label_step <- max(1L, (n + 6L) %/% 7L)
  for (i in seq_len(n)) {
    if (i %% label_step != 0L && i != n) {
      next
    }
    x <- MARGIN_LEFT + (i - 1L) * (bar_w + gap) + bar_w / 2
    label <- substr(dates[[i]], 6L, 10L)
    x_label_y <- axis_y + 8
    parts <- c(
      parts,
      sprintf(
        paste0(
          '<text x="%.1f" y="%.1f" text-anchor="end" fill="%s" font-size="%d" ',
          'transform="rotate(-45 %.1f %.1f)">%s</text>'
        ),
        x, x_label_y, COLOR_MUTED, FONT_TICK, x, x_label_y, label
      )
    )
  }

  # Legend
  x_cursor <- MARGIN_LEFT
  for (j in seq_len(nrow(series))) {
    label <- series$label[j]
    color <- series$color[j]
    parts <- c(
      parts,
      sprintf(
        '<rect x="%.1f" y="%.1f" width="%d" height="%d" fill="%s" rx="2"/>',
        x_cursor, legend_y - 10, LEGEND_SWATCH, LEGEND_SWATCH, color
      ),
      sprintf(
        '<text x="%.1f" y="%.1f" fill="%s" font-size="%d">%s</text>',
        x_cursor + LEGEND_SWATCH + 4, legend_y, COLOR_INK, FONT_LEGEND, label
      )
    )
    x_cursor <- x_cursor + LEGEND_SWATCH + 4 + nchar(label) * 8 + 20
  }

  if (show_incomplete_legend) {
    parts <- c(
      parts,
      sprintf(
        '<rect x="%.1f" y="%.1f" width="%d" height="%d" fill="%s" rx="2"/>',
        x_cursor, legend_y - 10, LEGEND_SWATCH, LEGEND_SWATCH, COLOR_INCOMPLETE
      ),
      sprintf(
        '<text x="%.1f" y="%.1f" fill="%s" font-size="%d">%s</text>',
        x_cursor + LEGEND_SWATCH + 4, legend_y, COLOR_INK, FONT_LEGEND,
        INCOMPLETE_LEGEND_LABEL
      )
    )
    x_cursor <- x_cursor + LEGEND_SWATCH + 4 + nchar(INCOMPLETE_LEGEND_LABEL) * 8 + 20
  }

  if (has_positivity) {
    parts <- c(
      parts,
      sprintf(
        '<circle cx="%.1f" cy="%.1f" r="%.1f" fill="%s"/>',
        x_cursor + LEGEND_SWATCH / 2, legend_y - 5, POSITIVITY_POINT_R + 0.5, COLOR_POSITIVITY
      ),
      sprintf(
        '<text x="%.1f" y="%.1f" fill="%s" font-size="%d">%s</text>',
        x_cursor + LEGEND_SWATCH + 4, legend_y, COLOR_INK, FONT_LEGEND, "5-day Rolling Test Positivity"
      )
    )
  }

  caption <- if (show_incomplete_legend) {
    sprintf(
      "Confirmed cases by date of symptom onset. %s",
      INCOMPLETE_CAPTION_NOTE
    )
  } else {
    "Confirmed cases by date of symptom onset"
  }

  parts <- c(
    parts,
    sprintf(
      '<text x="%d" y="%d" fill="%s" font-size="%d">%s</text>',
      MARGIN_LEFT, svg_h - 10L, COLOR_MUTED, FONT_CAPTION, caption
    ),
    "</svg>"
  )
  paste(parts, collapse = "\n")
}

build_plots <- function() {
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  incomplete_days <- load_incomplete_reporting_days()
  positivity_data <- load_positivity_data()

  manifest <- list(
    source_csv = PROVINCE_AGGREGATED_CSV,
    series = lapply(seq_len(nrow(SERIES_DEFS)), function(i) {
      list(
        id = SERIES_DEFS$id[i],
        label = SERIES_DEFS$label[i],
        color = SERIES_DEFS$color[i]
      )
    }),
    incomplete_styling = list(
      days = incomplete_days,
      color = COLOR_INCOMPLETE,
      label = INCOMPLETE_LEGEND_LABEL
    ),
    plots = list()
  )

  for (entry in load_province_plot_entries()) {
    filename <- plot_filename(entry$slug)
    out_path <- file.path(OUTPUT_DIR, filename)
    incomplete_dates <- incomplete_reporting_dates(entry$rows$date, incomplete_days)
    pos_rows <- province_positivity_rows(positivity_data, entry$province)
    # Extend the bar date range to cover any later positivity dates
    if (!is.null(pos_rows) && nrow(pos_rows) > 0L) {
      last_bar <- max(entry$rows$date)
      last_pos <- max(pos_rows$date)
      if (last_pos > last_bar) {
        extra_dates <- format(
          seq(as.Date(last_bar) + 1L, as.Date(last_pos), by = "day"),
          "%Y-%m-%d"
        )
        extra_rows <- data.frame(date = extra_dates, d_pos = 0L,
                                 stringsAsFactors = FALSE)
        entry$rows <- rbind(entry$rows, extra_rows)
      }
    }
    svg <- build_chart_svg(
      entry$province,
      entry$rows,
      entry$series,
      incomplete_dates = incomplete_dates,
      positivity_rows = pos_rows
    )
    writeLines(svg, out_path, useBytes = TRUE)
    caption <- if (length(incomplete_dates)) {
      sprintf(
        "Confirmed cases by date of symptom onset. %s",
        INCOMPLETE_CAPTION_NOTE
      )
    } else {
      "Confirmed cases by date of symptom onset"
    }
    manifest$plots[[entry$province]] <- list(
      file = filename,
      title = plot_title(entry$province),
      caption = caption
    )
    message(sprintf(
      "  wrote %s (%d days)",
      sub(paste0("^", DATA_ROOT, "/?"), "", out_path),
      nrow(entry$rows)
    ))
  }
  
  manifest_path <- file.path(OUTPUT_DIR, "manifest.json")
  writeLines(
    jsonlite::toJSON(manifest, pretty = TRUE, auto_unbox = TRUE),
    manifest_path,
    useBytes = TRUE
  )
  message(sprintf(
    "  wrote %s",
    sub(paste0("^", DATA_ROOT, "/?"), "", manifest_path)
  ))
  manifest
}

run_dashboard_plots <- function() {
  message(sprintf("INPUT = %s", PROVINCE_AGGREGATED_CSV))
  message(sprintf("OUTPUT = %s", OUTPUT_DIR))
  manifest <- build_plots()
  message(sprintf("done: %d province plot(s)", length(manifest$plots)))
  invisible(manifest)
}


# ------------------------------  Aesthetics  ----------------------------------
CODE_ROOT <- code_root
DATA_ROOT <- data_root
EPI_LATEST_DIR <- resolve_epi_latest_dir()
OUTPUT_DIR <- file.path(DATA_ROOT, "outputs/dashboard_plots")
PROVINCE_AGGREGATED_CSV <- file.path(EPI_LATEST_DIR, "province_aggregated.csv")
POSITIVITY_AGGREGATED_CSV <- file.path(EPI_LATEST_DIR, "positivity_aggregated.csv")
PIPELINE_CONFIGS_JSON <- file.path(
  CODE_ROOT, "pipelines", "pipeline_configs.json"
)
NOWCASTING_METADATA_JSON <- file.path(EPI_LATEST_DIR, "dhis2_nowcasting_run_metadata.json")


if (sys.nframe() == 0L) {
  quit(status = { run_dashboard_plots(); 0L })
} else {
  dashboard_plots_manifest <- run_dashboard_plots()
  dashboard_plots_built <- TRUE
  if (exists("keep_objects", inherits = TRUE)) {
    keep_objects <<- c(get("keep_objects", inherits = TRUE),
                       "dashboard_plots_manifest", "dashboard_plots_built")
  }
}
