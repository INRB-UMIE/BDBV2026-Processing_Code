#!/usr/bin/env Rscript
# plot-metrics.R
#
# Saves the charts of the growing metrics history (3-metrics-history.R). Run at
# the end of 3-metrics-history.R; a drawing failure there is logged, never
# fatal. The charts themselves are drawn by qc-plot-functions.R, which the
# Shiny Data Quality tab uses too.
#
#   Rscript pipelines/quality_checks/plot-metrics.R
#
# Outputs, in data/processed/quality_checks/ (counts and shares only):
#   metrics_<dataset>.png / .pdf          lines: each category's share over
#       time (all rows), rows first; red rings = a share jumped; GeneXpert on
#       a log10 scale; health_zone / province: largest 8, rest "(other)"
#   metrics_<dataset>_stacked.png / .pdf  the same as bands stacked to 100%
#       (GeneXpert over tested rows only)
#   metrics_<dataset>_counts.png / .pdf   the same bands in rows: the row
#       trajectory divided up by category
#   metrics_epi_alert_type_to_sample_status.png / .pdf
#       Sankey for the latest linelist export: type of alert -> status at
#       sampling (a "death" alert sampled "alive"?)

# -------------------------  Check if Initialised  -----------------------------
if (!exists("setup_complete") || !setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}
suppressPackageStartupMessages(library(ggplot2))
source(file.path(repo_root, "pipelines", "quality_checks", "qc-plot-functions.R"), local = FALSE)

qp_dir  <- file.path(repo_root, pipeline_configs$quality_checks$output_subdir)
qp_hist <- qcp_read(qp_dir, "metrics_history.csv")
qp_find <- qcp_read(qp_dir, "metrics_findings.csv")
if (is.null(qp_hist))
  stop("metrics_history.csv not found: run quality_checks/3-metrics-history.R first", call. = FALSE)
qp_hist <- qcp_metrics_types(qp_hist)

qp_save <- function(p, name, width = 14, height = NULL) {
  if (is.null(height)) height <- 2.6 * ceiling(attr(p, "n_panels") / 2) + 1
  for (ext in c("png", "pdf"))
    ggsave(file.path(qp_dir, paste0(name, ".", ext)), p, width = width, height = height,
           dpi = 150, limitsize = FALSE)
}

for (ds in intersect(names(QCP_TITLES), unique(qp_hist$dataset))) {
  pr <- qcp_metrics_prepare(qp_hist, qp_find, ds)
  if (is.null(pr)) next
  qp_save(qcp_metrics_lines(pr), paste0("metrics_", ds))
  qp_save(qcp_metrics_stacked(pr, "share"), paste0("metrics_", ds, "_stacked"))
  qp_save(qcp_metrics_stacked(pr, "n"), paste0("metrics_", ds, "_counts"))
  message("  metrics charts: ", ds, " (lines, stacked shares, stacked counts)")
}

qp_sk <- qcp_sankey(qp_hist)
if (!is.null(qp_sk)) {
  qp_save(qp_sk, "metrics_epi_alert_type_to_sample_status", width = 10, height = 6.5)
  message("  metrics chart: alert type -> sample status Sankey")
}

rm(list = intersect(ls(), c("qp_dir", "qp_hist", "qp_find", "qp_save", "qp_sk", "pr")))
