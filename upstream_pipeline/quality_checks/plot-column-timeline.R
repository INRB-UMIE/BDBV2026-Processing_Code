#!/usr/bin/env Rscript
# plot-column-timeline.R
#
# One Gantt-style chart per dataset (epi linelist, contact tracing) showing
# which columns each DHIS2 export had, over time. Drawn from the column history
# written by 1-raw-export-columns.R, so it needs no raw files and holds no
# patient data -- only column names. The chart is drawn by qc-plot-functions.R
# (qcp_column_timeline), which the Shiny Data Quality tab uses too.
#
#   Rscript pipelines/quality_checks/plot-column-timeline.R
#
# Also run at the end of 1-raw-export-columns.R (a plotting failure there is
# logged, never fatal).
#
# READING THE CHART
#   one row per column, as cleaning names it:
#     green   renamed by us (data/maps/*_column_renames.csv, or the cleaning
#             script's built-in replacements) -- the row shows OUR name
#     orange  no rename: cleaning makes a name up -- the row shows the raw name
#     bold    in the ctsafe export shared downstream (for the org-unit
#             hierarchy: its province / zone / area parts are)
#   a dot per export that had the column; a line joins consecutive exports.
#   A gap in the line: the column was missing from one or more exports.
#   A break with a new grey label above: DHIS2 changed the raw column name.
#   The grey label is the raw DHIS2 name (green rows only; on orange rows it
#   would repeat the row name).
#
# Outputs, beside the history in data/processed/quality_checks/:
#   column_timeline_epi_linelist.png / .pdf
#   column_timeline_contact_tracing.png / .pdf

# -------------------------  Check if Initialised  -----------------------------
if (!exists("setup_complete") || !setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}
suppressPackageStartupMessages(library(ggplot2))
source(file.path(repo_root, "pipelines", "quality_checks", "qc-plot-functions.R"), local = FALSE)

qct_dir   <- file.path(repo_root, pipeline_configs$quality_checks$output_subdir)
qct_cols  <- qcp_read(qct_dir, "raw_export_columns.csv")
qct_files <- qcp_read(qct_dir, "raw_export_files.csv")
if (is.null(qct_cols) || is.null(qct_files))
  stop("run quality_checks/1-raw-export-columns.R first", call. = FALSE)
qct_safe <- qcp_safe_columns(repo_root)

for (qct_ds in c("epi_linelist", "contact_tracing")) {
  qct_title <- c(epi_linelist = "Epi linelist: DHIS2 columns over time",
                 contact_tracing = "Contact tracing: DHIS2 columns over time")[[qct_ds]]
  qct_chart <- qcp_column_timeline(qct_cols, qct_files, qct_safe, qct_ds, qct_title)
  if (is.null(qct_chart)) { message("  ", qct_ds, ": no exports in the history, no chart"); next }
  for (ext in c("png", "pdf"))
    # one colour per y label works but warns "not officially supported"
    suppressWarnings(ggsave(file.path(qct_dir, paste0("column_timeline_", qct_ds, ".", ext)),
                            qct_chart$plot, width = 16, height = qct_chart$height, dpi = 150,
                            limitsize = FALSE))
  message("  ", qct_ds, ": column timeline written (", qct_chart$n_columns, " columns, ",
          qct_chart$n_exports, " exports)")
}

rm(list = intersect(ls(), c("qct_dir", "qct_cols", "qct_files", "qct_safe", "qct_ds",
                            "qct_title", "qct_chart")))
