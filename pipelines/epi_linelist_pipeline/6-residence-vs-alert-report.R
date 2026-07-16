#!/usr/bin/env Rscript
# Build HTML report for residence vs alert health-area maps.

# Self-contained bootstrap (mirrors resolve_code_root() in 0-setup.R), used
# only by the standalone-execution block at the bottom of this file to locate
# this repo (BDBV2026-Processing_Code) before 0-setup.R has been sourced.
resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
  } else {
    normalizePath(getwd(), mustWork = FALSE)
  }
  from_script <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
  if (dir.exists(file.path(from_script, "pipelines"))) {
    return(from_script)
  }
  cwd <- normalizePath(getwd(), mustWork = FALSE)
  if (dir.exists(file.path(cwd, "pipelines"))) {
    return(cwd)
  }
  stop("Could not locate BDBV2026-Processing_Code repo root (expected a pipelines/ directory).", call. = FALSE)
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

png_data_uri <- function(path) {
  if (!file.exists(path)) {
    return("")
  }
  b64 <- base64enc::base64encode(readBin(path, "raw", n = file.info(path)$size))
  paste0("data:image/png;base64,", b64)
}

build_key_findings_html <- function(province_results) {
  blocks <- vapply(
    province_results,
    function(res) {
      if (is.null(res)) {
        return("")
      }
      areas <- res$residence_only_areas
      if (is.null(areas) || !length(areas)) {
        items <- "<li><em>None identified.</em></li>"
      } else {
        items <- paste0(
          "<li>",
          vapply(areas, html_escape, character(1)),
          "</li>",
          collapse = ""
        )
      }
      sprintf(
        "<section class=\"province-findings\"><h4>%s</h4><ul>%s</ul></section>",
        html_escape(res$province),
        items
      )
    },
    character(1)
  )
  paste0(
    "<section id=\"key-findings\"><h3>Key findings</h3>",
    "<p>Health areas matched to the national health-area shapefile with line-list residence identified but no confirmed cases alerted:</p>",
    paste(blocks, collapse = ""),
    "</section>"
  )
}

build_report_html <- function(
    report_dir,
    report_stem,
    linelist_meta,
    province_results,
    maps_dir) {
  title <- "Residence vs Site of Alert Health Area Maps"
  subtitle <- paste0(
    "DHIS2 MVE line list update: ",
    linelist_meta$label,
    " (",
    linelist_meta$basename,
    ")"
  )

  description <- paste(
    "This report summarises maps of permanent residence health areas versus",
    "the health area where an alert was registered, for line-list rows in",
    "Ituri and Nord-Kivu. Pair maps draw lines between residence and alert",
    "locations when both can be matched to the national health-area shapefile.",
    "Ratio maps compare the number of line-list rows with residence in a health",
    "area to the number of confirmed cases alerted in that health area."
  )

  assumptions <- c(
    paste0(
      "Input line list: ",
      linelist_meta$basename,
      " (export date ",
      linelist_meta$label,
      ")."
    ),
    "Infectious pairs exclude rows where residence health area equals alert health area, or where residence health area equals alert health zone.",
    "Death alerts (type_of_alert = death) are excluded from the pair maps.",
    "Health-area names are matched to the RDC health-area shapefile using normalised text keys; unmatched areas are omitted from pair lines.",
    "Confirmed cases are identified from final_mve_case_classification = confirmed_case.",
    "Residence-to-confirmed ratio = (lines with residence in health area) / (confirmed cases with alert in health area). Purple shading marks health areas with residence lines but no confirmed alerts.",
    "Maps are illustrative for outbreak situational awareness; they do not imply confirmed transmission routes."
  )

  key_findings_html <- build_key_findings_html(province_results)

  plot_blocks <- vapply(
    province_results,
    function(res) {
      if (is.null(res) || is.null(res$plots)) {
        return("")
      }
      pairs_src <- html_escape(png_data_uri(res$plots$pairs_png))
      ratio_src <- html_escape(png_data_uri(res$plots$ratio_png))
      sprintf(
        paste0(
          "<section class=\"province\">",
          "<h3>%s</h3>",
          "<p class=\"stats\">Infectious-pair rows: %s | Pairs mapped: %s | ",
          "Health areas with ratio: %s | Residence only: %s / %s</p>",
          "<figure><img src=\"%s\" alt=\"%s pairs map\"><figcaption>%s: residence vs alert pairs</figcaption></figure>",
          "<figure><img src=\"%s\" alt=\"%s ratio map\"><figcaption>%s: residence-to-confirmed ratio</figcaption></figure>",
          "</section>"
        ),
        html_escape(res$province),
        res$infectious_pair_rows,
        res$infectious_pairs_mapped,
        res$health_areas_with_ratio,
        res$health_areas_residence_only,
        res$health_areas_total,
        pairs_src,
        html_escape(res$province),
        html_escape(res$province),
        ratio_src,
        html_escape(res$province),
        html_escape(res$province)
      )
    },
    character(1)
  )

  assumptions_html <- paste0(
    "<li>",
    vapply(assumptions, html_escape, character(1)),
    "</li>",
    collapse = ""
  )

  html <- paste0(
    "<!DOCTYPE html>",
    "<html lang=\"en\"><head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", html_escape(title), " — ", html_escape(linelist_meta$coded), "</title>",
    "<style>",
    "body { font-family: Georgia, 'Times New Roman', serif; color: #222; max-width: 920px; margin: 2rem auto; padding: 0 1.25rem 3rem; line-height: 1.5; }",
    "h1 { font-size: 1.75rem; margin-bottom: 0.25rem; }",
    "h2 { font-size: 1.15rem; font-weight: normal; color: #555; margin-top: 0; }",
    "h3 { font-size: 1.2rem; margin-top: 2rem; border-bottom: 1px solid #ddd; padding-bottom: 0.35rem; }",
    "h4 { font-size: 1.05rem; margin-top: 1.25rem; margin-bottom: 0.35rem; }",
    "section { margin-top: 2rem; }",
    "figure { margin: 1.5rem 0; }",
    "figure img { width: 100%; height: auto; border: 1px solid #ddd; }",
    "figcaption { font-size: 0.9rem; color: #666; margin-top: 0.4rem; }",
    ".stats { font-size: 0.95rem; color: #444; }",
    "ul { padding-left: 1.25rem; }",
    ".province-findings ul { columns: 2; column-gap: 2rem; }",
    "footer { margin-top: 2.5rem; font-size: 0.85rem; color: #777; border-top: 1px solid #eee; padding-top: 1rem; }",
    "</style>",
    "</head><body>",
    "<h1>", html_escape(title), "</h1>",
    "<h2>", html_escape(subtitle), "</h2>",
    "<section id=\"description\"><h3>Description</h3><p>", html_escape(description), "</p></section>",
    key_findings_html,
    "<section id=\"results\"><h3>Results</h3>", paste(plot_blocks, collapse = ""), "</section>",
    "<section id=\"assumptions\"><h3>Assumptions and caveats</h3><ul>", assumptions_html, "</ul></section>",
    "<footer>Generated ", html_escape(format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    " · Map images embedded for standalone sharing.</footer>",
    "</body></html>"
  )

  out_html <- file.path(report_dir, paste0(report_stem, ".html"))
  writeLines(html, out_html, useBytes = TRUE)
  out_html
}

write_residence_vs_alert_report <- function(
    data_root,
    linelist_meta,
    province_results,
    maps_dir = file.path(data_root, "outputs", linelist_meta$basename, "residence_vs_alert_health_area_maps")) {
  report_dir <- file.path(data_root, "outputs", linelist_meta$basename, "residence_vs_alert_reports")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  report_stem <- paste0("ResidenceAlertReport_", linelist_meta$coded)

  html_path <- build_report_html(
    report_dir = report_dir,
    report_stem = report_stem,
    linelist_meta = linelist_meta,
    province_results = province_results,
    maps_dir = maps_dir
  )

  message("Report HTML: ", html_path)

  invisible(list(
    html = html_path,
    stem = report_stem
  ))
}

if (sys.nframe() == 0L) {
  code_root <- resolve_repo_root()
  setup_path <- file.path(code_root, "pipelines", "0-setup.R")
  if (!file.exists(setup_path)) {
    stop("Setup script not found: ", setup_path, call. = FALSE)
  }
  source(setup_path, local = FALSE)
  # 0-setup.R re-resolves and overwrites code_root/data_root globally — the
  # code_root value above was only needed to locate 0-setup.R in the first place.

  # Adapted for BDBV2026-Contact_Tracing_Analysis: there's no raw workbook to
  # resolve a date from here (resolve_latest_dhis2_linelist() was upstream-only,
  # reading data/raw/dhis2_linelist_raw/, which doesn't exist in this repo).
  # Derive the run label from the latest synced date folder instead.
  epi_dir <- resolve_epi_latest_dir()
  linelist_meta <- parse_dhis2_linelist_date(epi_dir)
  maps_dir <- file.path(data_root, "outputs", linelist_meta$basename, "residence_vs_alert_health_area_maps")

  province_results <- list()
  for (prov_key in c("ituri", "nord kivu")) {
    slug <- gsub(" ", "-", prov_key)
    pairs_png <- file.path(
      maps_dir, paste0(slug, "_residence_vs_alert_health_area_map.png")
    )
    ratio_png <- file.path(
      maps_dir, paste0(slug, "_residence_confirmed_ratio_map.png")
    )
    if (!file.exists(pairs_png) || !file.exists(ratio_png)) {
      next
    }
    province_results[[length(province_results) + 1L]] <- list(
      province = if (prov_key == "ituri") "Ituri" else "Nord-Kivu",
      infectious_pair_rows = NA_integer_,
      infectious_pairs_mapped = NA_integer_,
      health_areas_with_ratio = NA_integer_,
      health_areas_residence_only = NA_integer_,
      health_areas_total = NA_integer_,
      residence_only_areas = character(),
      plots = list(pairs_png = pairs_png, ratio_png = ratio_png)
    )
  }

  write_residence_vs_alert_report(
    data_root = data_root,
    linelist_meta = linelist_meta,
    province_results = province_results,
    maps_dir = maps_dir
  )
}
