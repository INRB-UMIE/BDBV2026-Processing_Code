#!/usr/bin/env Rscript
# render-report.R -- build a run's report inputs and render its report.
#
#   Rscript pipelines/report/render-report.R
#       the latest export pair (latest.json): build the inputs bundle from the
#       full data, then render HTML. What the server does (nightly, and from
#       the app's Reports tab).
#   Rscript pipelines/report/render-report.R --epi LINELIST_26102026 --ct CONTACT_TRACING_26102026
#       a given pair, same steps.
#   Rscript pipelines/report/render-report.R --pdf-missing
#       CI: render a PDF for every inputs bundle that has none yet, from the
#       bundle alone (no processed exports needed). Writes the new PDFs' paths
#       to $GITHUB_OUTPUT as `files` when that is set.
#   --format pdf     render PDF instead of HTML (needs LaTeX)
#   --no-build       render from an existing bundle, do not rebuild it
#
# Output: data/processed/reports/pipeline_run_report_<EPI>_<CT>.html / .pdf
# Inputs: data/processed/reports/inputs/<EPI>__<CT>/ (build-report-inputs.R)
#
# Defines render_run_report() too, for shiny/global.R's run_reports_bg() and the
# full pipeline's last step; sourcing this file with RENDER_REPORT_NO_MAIN set
# defines the functions without running anything.

render_report_dir <- function(repo_root) file.path(repo_root, "data", "processed", "reports")

render_run_report <- function(repo_root, epi_run, ct_run, format = c("html", "pdf"),
                              build = TRUE) {
  format <- match.arg(format)
  if (build) {
    source(file.path(repo_root, "pipelines", "report", "build-report-inputs.R"), local = TRUE)
    message("  building report inputs for ", epi_run, " / ", ct_run)
    build_report_inputs(repo_root, epi_run, ct_run)
  }
  out_dir <- render_report_dir(repo_root)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- paste0("pipeline_run_report_", epi_run, "_", ct_run, ".", format)
  tmp <- tempfile("report-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  rmarkdown::render(
    file.path(repo_root, "pipelines", "report", "pipeline_run_report.Rmd"),
    output_format = if (format == "html") "html_document" else "pdf_document",
    params = list(repo_root = repo_root, epi_run = epi_run, ct_run = ct_run),
    output_file = out_file, output_dir = out_dir, intermediates_dir = tmp,
    knit_root_dir = tmp, quiet = TRUE, envir = new.env(parent = globalenv()))
  message("  report written: ", file.path(out_dir, out_file))
  invisible(file.path(out_dir, out_file))
}

render_latest_pair <- function(repo_root) {
  lj <- jsonlite::fromJSON(file.path(repo_root, "data", "processed", "latest.json"))
  epi <- lj$epi_linelist$folder; ct <- lj$contact_tracing$folder
  if (is.null(epi) || is.null(ct)) stop("latest.json has no epi_linelist / contact_tracing folder",
                                        call. = FALSE)
  c(epi = epi, ct = ct)
}

if (!nzchar(Sys.getenv("RENDER_REPORT_NO_MAIN"))) local({
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(name) { i <- match(paste0("--", name), args)
    if (is.na(i) || i == length(args)) NA_character_ else args[[i + 1L]] }
  flag <- function(name) paste0("--", name) %in% args
  repo_root <- normalizePath(".")
  if (!file.exists(file.path(repo_root, "pipelines", "report", "pipeline_run_report.Rmd")))
    stop("run from the repo root", call. = FALSE)

  if (flag("pdf-missing")) {
    inputs <- list.dirs(file.path(render_report_dir(repo_root), "inputs"),
                        recursive = FALSE, full.names = FALSE)
    inputs <- inputs[grepl("^.+__.+$", inputs)]
    made <- character(0)
    for (b in inputs) {
      parts <- strsplit(b, "__", fixed = TRUE)[[1]]
      pdf <- file.path(render_report_dir(repo_root),
                       paste0("pipeline_run_report_", parts[1], "_", parts[2], ".pdf"))
      if (file.exists(pdf)) next
      ok <- tryCatch({ render_run_report(repo_root, parts[1], parts[2], "pdf", build = FALSE); TRUE },
                     error = function(e) { message("!!! PDF failed for ", b, ": ",
                                                   conditionMessage(e)); FALSE })
      if (ok) made <- c(made, sub(paste0("^", repo_root, "/"), "", pdf))
    }
    message(length(made), " PDF(s) rendered")
    gh <- Sys.getenv("GITHUB_OUTPUT")
    if (nzchar(gh)) cat("files<<PDF_EOF\n", paste(made, collapse = "\n"), "\nPDF_EOF\n",
                        "has_compiled=", if (length(made)) "true" else "false", "\n",
                        file = gh, append = TRUE, sep = "")
  } else {
    pair <- if (!is.na(opt("epi")) && !is.na(opt("ct"))) c(epi = opt("epi"), ct = opt("ct"))
            else render_latest_pair(repo_root)
    fmt <- if (identical(opt("format"), "pdf")) "pdf" else "html"
    render_run_report(repo_root, pair[["epi"]], pair[["ct"]], fmt, build = !flag("no-build"))
  }
})
