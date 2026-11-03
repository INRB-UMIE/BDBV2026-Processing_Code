# build-report-inputs.R
#
# Everything the run report shows that has to be computed from the full data,
# computed ONCE, where the full data is (the server), and saved as a small
# bundle of AGGREGATES:
#
#   data/processed/reports/inputs/<EPI_RUN>__<CT_RUN>/
#
# The report (pipeline_run_report.Rmd) reads only this bundle and
# data/processed/quality_checks/ -- never the processed exports themselves. That
# is what lets the same report be rendered as HTML on the server and as PDF in
# CI: CI has the git mirror of data/processed/, which lacks the big *_names.csv
# files (the contact tracing ones are over GitHub's 100 MB limit, so the mirror
# skips them), but has this bundle.
#
# NO ROW-LEVEL DATA and NO NAMES go in the bundle: counts, shares, column
# names, place names that failed to match, and the processing logs (which the
# old report already printed in full). PII columns are dropped on read, using
# pipeline_configs.json's pii_columns, before anything is counted.
#
# Defines build_report_inputs(repo_root, epi_run, ct_run); sourced by
# render-report.R. Base R + jsonlite only.

rpi_read <- function(path, cols = NULL) {
  if (!file.exists(path)) return(NULL)
  if (is.null(cols))
    return(utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                           check.names = FALSE, colClasses = "character"))
  hdr <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  cc <- ifelse(hdr %in% cols, "character", "NULL")
  d <- utils::read.csv(path, colClasses = cc, na.strings = "", check.names = FALSE)
  for (cl in setdiff(cols, names(d))) d[[cl]] <- rep(NA_character_, nrow(d))
  d
}

rpi_write <- function(d, dir, name) {
  if (is.null(d)) return(invisible())
  utils::write.csv(d, file.path(dir, name), row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

rpi_blank <- function(x) is.na(x) | !nzchar(trimws(x))

# The processing logs print, in full, lists of values typed into the exports:
# unmatched place names, unmatched job titles, unparseable alert IDs and so on.
# Those are free text and can hold a person's name (a name typed into the alert
# ID field was found this way on 2026-10-27). Every such list -- any line of
# the form "... (N distinct ...): a, b, c" / "(N row(s) ...): ..." /
# "(N rows affected): ..." -- keeps its label and counts but loses the values,
# so the report shows WHERE to look; the full list stays in the log on the
# server. Mirrored by tests/test_run_report.py, which runs RPI_CENSOR_RE over
# real log lines.
RPI_CENSOR_RE  <- "^(.*?\\([0-9,]+ (?:distinct|row\\(s\\)|rows affected).*?\\):)\\s*\\S.*$"
RPI_CENSOR_TAG <- "\\1 [censored -- see this export's processing log on the server]"
rpi_censor_log <- function(lines) {
  if (is.null(lines)) return(NULL)
  sub(RPI_CENSOR_RE, RPI_CENSOR_TAG, lines, perl = TRUE)
}

build_report_inputs <- function(repo_root, epi_run, ct_run, top_n = 25) {
  cfg <- jsonlite::fromJSON(file.path(repo_root, "pipelines", "pipeline_configs.json"),
                            simplifyVector = TRUE)
  pii <- unique(c(cfg$pii_columns$epi_linelist, cfg$pii_columns$contact_tracing))
  if (!length(pii))
    stop("pipeline_configs.json has no pii_columns; refusing to build report inputs.",
         call. = FALSE)

  proc <- file.path(repo_root, "data", "processed")
  epi_dir <- file.path(proc, "dhis2_linelist_processed", epi_run)
  ct_dir  <- file.path(proc, "contact_tracing_processed", ct_run)
  out <- file.path(proc, "reports", "inputs", paste0(epi_run, "__", ct_run))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  # ---- logs ------------------------------------------------------------------
  find_log <- function(name) {
    root <- file.path(repo_root, "logs")
    if (!dir.exists(root)) return(NA_character_)
    runs <- sort(list.dirs(root, recursive = FALSE, full.names = FALSE), decreasing = TRUE)
    runs <- c(runs[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}", runs)], "legacy")
    hits <- file.path(root, runs, name)
    hits <- hits[file.exists(hits)]
    if (length(hits)) hits[[1]] else NA_character_
  }
  read_log <- function(stem) {
    p <- find_log(paste0(stem, "_processing_log.txt"))
    if (is.na(p)) NULL else readLines(p, warn = FALSE, encoding = "UTF-8")
  }
  # Censored on read: everything below, and the bundle, sees only these.
  epi_log <- rpi_censor_log(read_log(epi_run)); ct_log <- rpi_censor_log(read_log(ct_run))
  if (!is.null(epi_log)) writeLines(epi_log, file.path(out, "log_epi.txt"), useBytes = TRUE)
  if (!is.null(ct_log))  writeLines(ct_log,  file.path(out, "log_ct.txt"),  useBytes = TRUE)
  has_err <- function(log) !is.null(log) && any(grepl("^!!! ERROR", log))

  # ---- the processed exports, PII dropped on read -----------------------------
  read_np <- function(path) {
    d <- rpi_read(path)
    if (is.null(d)) NULL else d[, setdiff(names(d), pii), drop = FALSE]
  }
  epi <- read_np(file.path(epi_dir, "dhis2_processed_linelist_names.csv"))
  ct  <- read_np(file.path(ct_dir,  "contact_tracing_processed_names.csv"))

  rpi_write(data.frame(
    dataset = c("epi_linelist", "contact_tracing"), run = c(epi_run, ct_run),
    rows = c(if (is.null(epi)) NA else nrow(epi), if (is.null(ct)) NA else nrow(ct)),
    columns = c(if (is.null(epi)) NA else ncol(epi), if (is.null(ct)) NA else ncol(ct)),
    file = c(file.path("data/processed/dhis2_linelist_processed", epi_run,
                       "dhis2_processed_linelist_names.csv"),
             file.path("data/processed/contact_tracing_processed", ct_run,
                       "contact_tracing_processed_names.csv")),
    stringsAsFactors = FALSE), out, "overview.csv")

  # ---- pipeline steps --------------------------------------------------------
  # The same evidence the old report used: the output exists, the cleaning log
  # has no "!!! ERROR", and the dedup outputs are no older than the export
  # (5 min slack: within one run the writes are minutes apart).
  latest <- tryCatch(jsonlite::fromJSON(file.path(proc, "latest.json")), error = function(e) NULL)
  fresh <- function(a, b) file.exists(a) && file.exists(b) &&
    as.numeric(difftime(file.mtime(a), file.mtime(b), units = "secs")) >= -300
  ct_src <- file.path(ct_dir, "contact_tracing_processed_names.csv")
  ct_date <- regmatches(ct_run, regexpr("[0-9]{8}$", ct_run))
  ct_dedup_logs <- c(find_log(paste0(ct_run, "_ct_dedup_log.txt")),
                     if (length(ct_date)) find_log(paste0("CT-COMBINED_", ct_date, "_ct_dedup_log.txt")))
  ct_dedup_logs <- ct_dedup_logs[!is.na(ct_dedup_logs)]
  pm <- file.path(proc, "person_matching")
  rpi_write(data.frame(
    step = c("Setup", "Epi Clean", "Epi Dedup", "CT Clean", "CT IDs", "CT Dedup",
             "Name Rarity", "Person Links", "Link Ledger", "Registry"),
    ok = c(TRUE,
           !is.null(epi) && !has_err(epi_log),
           fresh(file.path(epi_dir, "duplicate_candidates.csv"),
                 file.path(epi_dir, "dhis2_processed_linelist_names.csv")),
           !is.null(ct) && !has_err(ct_log),
           !is.null(ct) && all(c("dhis2_contact_id", "contact_id_assigned") %in% names(ct)),
           length(ct_dedup_logs) > 0 &&
             fresh(ct_dedup_logs[which.max(file.mtime(ct_dedup_logs))], ct_src),
           !is.null(latest$name_rarity),
           !is.null(latest$person_candidates),
           file.exists(file.path(pm, "link_ledger.csv")),
           file.exists(file.path(pm, "person_registry.csv"))),
    stringsAsFactors = FALSE), out, "steps.csv")
  errs <- c(grep("^!!! ERROR", epi_log %||% character(0), value = TRUE),
            grep("^!!! ERROR", ct_log %||% character(0), value = TRUE))
  writeLines(errs, file.path(out, "errors.txt"), useBytes = TRUE)

  # ---- unmatched place names --------------------------------------------------
  unmatched <- function(d, ds, pairs) {
    if (is.null(d)) return(list(rates = NULL, values = NULL))
    rates <- list(); vals <- list()
    for (flag in names(pairs)) {
      if (!flag %in% names(d)) next
      v <- pairs[[flag]]
      f <- d[[flag]][!is.na(d[[flag]])]
      un <- d[[flag]] %in% c("TRUE", "true", TRUE)
      raw <- if (v %in% names(d)) d[[v]][un & !rpi_blank(d[[v]])] else character(0)
      rates[[flag]] <- data.frame(dataset = ds, column = v, rows = length(f),
                                  unmatched = sum(f %in% c("TRUE", "true")),
                                  distinct_unmatched = length(unique(raw)),
                                  stringsAsFactors = FALSE)
      if (length(raw)) {
        t <- sort(table(raw), decreasing = TRUE)
        if (v == "permanent_residence_health_area") t <- utils::head(t, top_n)
        vals[[flag]] <- data.frame(dataset = ds, column = v, value = names(t),
                                   rows = as.integer(t), stringsAsFactors = FALSE)
      }
    }
    list(rates = do.call(rbind, rates), values = do.call(rbind, vals))
  }
  ue <- unmatched(epi, "epi_linelist", c(health_zone_unmatched = "health_zone",
                                         health_area_unmatched = "health_area",
                                         permanent_residence_health_area_unmatched =
                                           "permanent_residence_health_area"))
  # Permanent residence is typed free text (near-addresses, sometimes names):
  # its match RATE is reported, its values never are. Health zone and health
  # area come from the DHIS2 org-unit hierarchy, so their names are kept.
  if (!is.null(ue$values))
    ue$values <- ue$values[ue$values$column != "permanent_residence_health_area", , drop = FALSE]
  uc <- unmatched(ct, "contact_tracing", c(health_zone_unmatched = "health_zone",
                                           health_area_unmatched = "health_area"))
  rpi_write(rbind(ue$rates, uc$rates), out, "geo_match.csv")
  rpi_write(rbind(ue$values, uc$values), out, "geo_unmatched_values.csv")

  # ---- epi breakdowns ---------------------------------------------------------
  if (!is.null(epi)) {
    p <- epi$province; p[rpi_blank(p)] <- "(blank)"
    cls <- epi$final_mve_case_classification
    by_p <- data.frame(province = sort(unique(p)), stringsAsFactors = FALSE)
    by_p$alerts <- vapply(by_p$province, function(x) sum(p == x), integer(1))
    by_p$confirmed <- vapply(by_p$province, function(x)
      sum(p == x & cls %in% "confirmed_case"), integer(1))
    by_p$deaths <- vapply(by_p$province, function(x)
      sum(p == x & epi$type_of_alert %in% "death"), integer(1))
    rpi_write(by_p[order(-by_p$alerts), ], out, "epi_by_province.csv")
  }

  # ---- contact tracing --------------------------------------------------------
  if (!is.null(ct)) {
    nb <- function(x) x[!rpi_blank(x)]
    rpi_write(data.frame(
      measure = c("source cases", "contacts", "case-contact pairs", "follow-up rows"),
      value = c(length(unique(nb(ct$source_case_alert_id))), length(unique(nb(ct$contact_id))),
                nrow(unique(ct[, c("source_case_alert_id", "contact_id")])), nrow(ct)),
      stringsAsFactors = FALSE), out, "ct_summary.csv")
  }
  meta_p <- file.path(proc, "contact_tracing_processed", "combined", "combined_meta.json")
  cts_p  <- file.path(proc, "contact_tracing_processed", "combined",
                      "contact_tracing_combined_contacts.csv")
  if (file.exists(meta_p) && file.exists(cts_p)) {
    meta <- jsonlite::fromJSON(meta_p)
    cts <- rpi_read(cts_p, c("contact_status", "source_case_changed"))
    st <- table(cts$contact_status)
    rpi_write(data.frame(
      measure = c("latest export", "window start", "rows", "contacts",
                  paste0("status: ", names(st)), "source case changed",
                  "exact duplicate rows removed"),
      value = c(meta$latest_export %||% "", meta$window_start %||% "", meta$n_rows,
                meta$n_contacts, as.integer(st),
                sum(toupper(cts$source_case_changed) == "TRUE", na.rm = TRUE),
                meta$n_duplicate_rows_removed %||% NA),
      stringsAsFactors = FALSE), out, "ct_combined.csv")
  }

  # ---- person matching --------------------------------------------------------
  led <- rpi_read(file.path(pm, "link_ledger.csv"), c("link_status", "decided_by"))
  if (!is.null(led)) {
    t <- as.data.frame(table(link_status = led$link_status, decided_by = led$decided_by),
                       stringsAsFactors = FALSE)
    rpi_write(t[t$Freq > 0, ], out, "pm_ledger.csv")
  }
  reg <- rpi_read(file.path(pm, "person_registry.csv"),
                  c("person_id", "identifier_type", "status"))
  if (!is.null(reg)) {
    per <- table(reg$person_id)
    linked <- names(per)[per > 1]
    rpi_write(data.frame(
      measure = c("persons", "identifiers", "persons with more than one identifier",
                  "identifiers in persons with more than one",
                  paste0("identifiers: ", names(table(reg$identifier_type))),
                  "identifiers with an undecided link"),
      value = c(length(per), nrow(reg), length(linked), sum(reg$person_id %in% linked),
                as.integer(table(reg$identifier_type)), sum(reg$status == "undecided")),
      stringsAsFactors = FALSE), out, "pm_registry.csv")
  }

  jsonlite::write_json(list(epi_run = epi_run, ct_run = ct_run,
                            built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                            top_n = top_n),
                       file.path(out, "meta.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(out)
}

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
