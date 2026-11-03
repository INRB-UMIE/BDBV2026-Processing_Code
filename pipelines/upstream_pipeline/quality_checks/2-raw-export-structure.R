#!/usr/bin/env Rscript
# 2-raw-export-structure.R
#
# Stage 2 quality checks: the STRUCTURE of each dated raw DHIS2 export,
# compared with the previous one -- row counts, blank cells, column types,
# how recent its dates are, category values the translation tables do not
# know, duplicates. See pipelines/quality_checks/README.md.
#
#   Rscript pipelines/quality_checks/2-raw-export-structure.R
#
# Reads NO raw files: everything comes from the per-export profile that
# 1-raw-export-columns.R keeps (raw_export_files.csv, raw_export_columns.csv),
# so it is cheap and recomputed in full every run. 1-raw-export-columns.R runs
# it at its end. Every finding is a warning (or info): none stops the pipeline.
#
# Output: data/processed/quality_checks/raw_export_structure_findings.csv
# (same columns as raw_export_column_findings.csv). Counts, shares, dates,
# column names and the commonest untranslated category codes -- no row data.
#
# The rules are mirrored in tests/reference/quality_checks.py
# (structure_findings), tested by tests/test_quality_checks.py. Change both.

# -------------------------  Check if Initialised  -----------------------------
if (!exists("setup_complete") || !setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}

qs_cfg  <- pipeline_configs$quality_checks
QS      <- qs_cfg$structure
qs_dir  <- file.path(repo_root, qs_cfg$output_subdir)
qs_crit <- jsonlite::fromJSON(file.path(repo_root, "pipelines", "pipeline_configs.json"),
                              simplifyVector = FALSE)$quality_checks$critical_columns

QS_SEVERITY <- c(row_change = "warning", rows_dropped = "warning",
                 blank_jump = "warning", column_now_empty = "warning",
                 column_now_filled = "info", type_change = "warning",
                 stale_export = "warning", window_gap = "warning",
                 unknown_category_values = "warning", mostly_untranslated = "warning",
                 duplicate_ids = "warning", duplicate_rows = "warning")

qs_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, colClasses = "character", fileEncoding = "UTF-8",
                  na.strings = character(0), check.names = FALSE)
}
qs_num <- function(x) suppressWarnings(as.numeric(x))
qs_json <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(stats::setNames(integer(0), character(0)))
  v <- jsonlite::fromJSON(s, simplifyVector = FALSE)
  stats::setNames(as.integer(unlist(v)), names(v))
}
qs_fmt_vals <- function(v) paste0("'", names(v), "' x", v, collapse = ", ")
qs_pct <- function(x) sprintf("%.0f%%", 100 * x)

qs_files <- qs_read(file.path(qs_dir, "raw_export_files.csv"))
qs_cols  <- qs_read(file.path(qs_dir, "raw_export_columns.csv"))
if (is.null(qs_files) || is.null(qs_cols))
  stop("run quality_checks/1-raw-export-columns.R first", call. = FALSE)
# Histories written before stage 2 lack these columns: their checks are skipped.
for (cl in c("n_duplicate_rows", "n_duplicate_ids"))
  if (!cl %in% names(qs_files)) qs_files[[cl]] <- ""
for (cl in c("min_date", "max_date", "n_unknown", "unknown_values"))
  if (!cl %in% names(qs_cols)) qs_cols[[cl]] <- ""
qs_cols$key <- ifelse(qs_cols$mapping %in% c("explicit", "builtin"),
                      qs_cols$standard_name, qs_cols$raw_name)

# Untranslated values were counted against the translation tables as they were
# when each export was read. Re-apply the CURRENT tables, so adding a missing
# code to data/maps/ clears its warnings straight away, with no rebuild: a
# stored value the table now knows is dropped and its count subtracted. Exact
# whenever a column had at most 10 distinct untranslated values (all stored);
# otherwise the count is reduced by what is known.
for (ds in names(QS$value_maps)) {
  vm <- QS$value_maps[[ds]]
  for (std in names(vm)) {
    map <- load_map_csv(vm[[std]])
    rows <- which(qs_cols$dataset == ds & qs_cols$standard_name == std &
                  nzchar(qs_cols$unknown_values))
    for (r in rows) {
      v <- qs_json(qs_cols$unknown_values[r])
      known <- names(v) %in% names(map)
      if (!any(known)) next
      qs_cols$n_unknown[r] <- as.character(max(0, qs_num(qs_cols$n_unknown[r]) - sum(v[known])))
      keep <- v[!known]
      qs_cols$unknown_values[r] <- if (length(keep)) as.character(jsonlite::toJSON(
        as.list(keep), auto_unbox = TRUE)) else ""
    }
  }
}

# Known events: the hand-kept file plus those marked "expected" in the app.
qs_ev_cols <- c("dataset", "export_date", "check", "column", "note")
qs_events <- do.call(rbind, lapply(c(qs_cfg$known_events, qs_cfg$known_events_app), function(f) {
  e <- qs_read(file.path(repo_root, f))
  if (is.null(e)) NULL else e[, qs_ev_cols, drop = FALSE]
}))
if (is.null(qs_events))
  qs_events <- data.frame(dataset = character(0), export_date = character(0),
                          check = character(0), column = character(0),
                          note = character(0), stringsAsFactors = FALSE)

qs_out <- list()

for (ds in c("epi_linelist", "contact_tracing")) {
  fh <- qs_files[qs_files$dataset == ds & qs_files$read_ok == "TRUE", , drop = FALSE]
  fh <- fh[order(fh$export_date, fh$file), , drop = FALSE]
  crit_std <- unlist(lapply(qs_crit[[ds]], function(cr) unlist(cr$standard_names)))
  date_col <- QS$date_column[[ds]]
  free     <- unlist(QS$free_text)
  filled   <- function(b) b <= 1 - QS$fill_min_share

  prev <- NULL; pc <- NULL
  for (k in seq_len(nrow(fh))) {
    cur <- fh[k, ]
    cc <- qs_cols[qs_cols$dataset == ds & qs_cols$file == cur$file, , drop = FALSE]
    cc <- cc[order(as.integer(cc$position)), , drop = FALSE]
    cc <- cc[!duplicated(cc$key), , drop = FALSE]
    ed <- as.Date(cur$export_date)
    n_cur <- qs_num(cur$n_rows)
    add <- function(check, column, detail) {
      qs_out[[length(qs_out) + 1L]] <<- data.frame(
        dataset = ds, file = cur$file, export_date = cur$export_date,
        previous_file = if (is.null(prev)) "" else prev$file,
        check = check, severity = unname(QS_SEVERITY[check]), column = column,
        detail = detail, contents = "", stringsAsFactors = FALSE)
    }

    # Stale: the newest date is well before the date in the file name.
    dc <- cc[cc$standard_name == date_col & nzchar(cc$max_date), , drop = FALSE]
    if (nrow(dc)) {
      age <- as.numeric(ed - as.Date(dc$max_date[1]))
      if (!is.na(age) && age > QS$stale_days[[ds]])
        add("stale_export", dc$raw_name[1],
            sprintf("newest %s is %s; %d days before the export date",
                    date_col, dc$max_date[1], as.integer(age)))
    }

    # Category values the translation table does not know.
    for (i in seq_len(nrow(cc))) {
      if (cc$standard_name[i] %in% free || !nzchar(cc$n_unknown[i])) next
      nonblank <- n_cur * (1 - qs_num(cc$pct_blank[i]))
      nu <- qs_num(cc$n_unknown[i])
      if (is.na(nonblank) || nonblank <= 0 || is.na(nu)) next
      unk <- qs_json(cc$unknown_values[i])
      if (nu / nonblank >= QS$untranslated_share) {
        add("mostly_untranslated", cc$raw_name[i],
            sprintf("%s of values not in the translation table, e.g. %s",
                    qs_pct(nu / nonblank), qs_fmt_vals(utils::head(unk, 5))))
        next
      }
      before <- if (!is.null(pc)) {
        j <- match(cc$key[i], pc$key)
        if (is.na(j)) character(0) else names(qs_json(pc$unknown_values[j]))
      } else character(0)
      new <- unk[!names(unk) %in% before]
      if (length(new) && sum(new) / nonblank >= QS$unknown_new_min_share)
        add("unknown_category_values", cc$raw_name[i],
            paste0("new values not in the translation table: ", qs_fmt_vals(new)))
    }

    if (is.null(prev)) { prev <- cur; pc <- cc; next }
    n_prev <- qs_num(prev$n_rows)

    # Rows.
    if (!is.na(n_prev) && n_prev > 0 && !is.na(n_cur)) {
      change <- (n_cur - n_prev) / n_prev
      if (abs(change) > QS$row_change) {
        add("row_change", "", sprintf("%s -> %s rows (%+.0f%%)",
                                      format(n_prev, big.mark = ","),
                                      format(n_cur, big.mark = ","), 100 * change))
      } else if (ds == "epi_linelist" && n_cur < n_prev) {
        add("rows_dropped", "", sprintf("%s -> %s rows", format(n_prev, big.mark = ","),
                                        format(n_cur, big.mark = ",")))
      }
    }

    # Per column, present in both exports.
    m <- match(cc$key, pc$key)
    for (i in which(!is.na(m))) {
      j <- m[i]
      pb <- qs_num(pc$pct_blank[j]); cb <- qs_num(cc$pct_blank[i])
      if (!is.na(pb) && !is.na(cb)) {
        if (filled(pb) && cb >= 1) {
          add("column_now_empty", cc$raw_name[i],
              sprintf("was %s filled, now empty", qs_pct(1 - pb)))
        } else if (pb >= 1 && filled(cb)) {
          add("column_now_filled", cc$raw_name[i],
              sprintf("was empty, now %s filled", qs_pct(1 - cb)))
        } else if (pb < 1 && cb < 1) {
          lim <- if (cc$standard_name[i] %in% crit_std) QS$blank_jump_critical else QS$blank_jump
          if (cb - pb > lim)
            add("blank_jump", cc$raw_name[i],
                sprintf("blank %s -> %s%s", qs_pct(pb), qs_pct(cb),
                        if (cc$standard_name[i] %in% crit_std) " (critical column)" else ""))
        }
      }
      pt <- pc$type[j]; ct <- cc$type[i]
      if (nzchar(pt) && nzchar(ct) && pt != "blank" && ct != "blank" && pt != ct)
        add("type_change", cc$raw_name[i], sprintf("%s -> %s", pt, ct))
    }

    # Contact tracing: a gap between consecutive registration windows.
    if (ds == "contact_tracing" && nrow(dc)) {
      pdc <- pc[pc$standard_name == date_col & nzchar(pc$max_date), , drop = FALSE]
      if (nrow(pdc) && nzchar(dc$min_date[1])) {
        gap <- as.numeric(as.Date(dc$min_date[1]) - as.Date(pdc$max_date[1]))
        if (!is.na(gap) && gap > QS$window_gap_days)
          add("window_gap", dc$raw_name[1],
              sprintf("previous export's latest %s %s, this one's earliest %s",
                      date_col, pdc$max_date[1], dc$min_date[1]))
      }
    }

    # Duplicates: only when they increase.
    if (ds == "epi_linelist") {
      a <- qs_num(prev$n_duplicate_ids); b <- qs_num(cur$n_duplicate_ids)
      if (!is.na(a) && !is.na(b) && b > a)
        add("duplicate_ids", "", sprintf("alert_id on more than one row: %d -> %d", as.integer(a), as.integer(b)))
    }
    a <- qs_num(prev$n_duplicate_rows); b <- qs_num(cur$n_duplicate_rows)
    if (!is.na(a) && !is.na(b) && !is.na(n_prev) && !is.na(n_cur) && n_prev > 0 && n_cur > 0 &&
        b / n_cur - a / n_prev > 0.01)
      add("duplicate_rows", "", sprintf("exact duplicate rows %s -> %s",
                                        qs_pct(a / n_prev), qs_pct(b / n_cur)))

    prev <- cur; pc <- cc
  }
}

qs_findings <- if (length(qs_out)) do.call(rbind, qs_out) else
  data.frame(dataset = character(0), file = character(0), export_date = character(0),
             previous_file = character(0), check = character(0), severity = character(0),
             column = character(0), detail = character(0), contents = character(0),
             stringsAsFactors = FALSE)

# Known events: same rule as the column checks.
qs_findings$status <- rep("new", nrow(qs_findings))
qs_findings$event_note <- rep("", nrow(qs_findings))
for (k in seq_len(nrow(qs_events))) {
  ev <- qs_events[k, ]
  hit <- qs_findings$dataset == ev$dataset & qs_findings$export_date == ev$export_date &
         (!nzchar(ev$check)  | qs_findings$check  == ev$check) &
         (!nzchar(ev$column) | qs_findings$column == ev$column)
  qs_findings$status[hit] <- "expected"
  qs_findings$event_note[hit] <- ev$note
}

qs_path <- file.path(qs_dir, "raw_export_structure_findings.csv")
qs_tmp <- paste0(qs_path, ".tmp")
utils::write.csv(qs_findings, qs_tmp, row.names = FALSE, fileEncoding = "UTF-8", na = "")
if (!file.rename(qs_tmp, qs_path)) { unlink(qs_tmp); stop("Could not write ", qs_path, call. = FALSE) }

# Summary: the latest export of each dataset, in full.
for (ds in c("epi_linelist", "contact_tracing")) {
  fh <- qs_files[qs_files$dataset == ds & qs_files$read_ok == "TRUE", , drop = FALSE]
  if (!nrow(fh)) next
  latest <- fh$file[order(fh$export_date, fh$file)][nrow(fh)]
  lf <- qs_findings[qs_findings$dataset == ds & qs_findings$file == latest &
                    qs_findings$status == "new", , drop = FALSE]
  message("  ", ds, ": structure, latest export ", latest, ": ",
          if (!nrow(lf)) "no findings" else paste0(nrow(lf), " finding(s)"))
  for (i in seq_len(nrow(lf)))
    message("    [", lf$severity[i], "] ", lf$check[i],
            if (nzchar(lf$column[i])) paste0(": ", lf$column[i]), " -- ", lf$detail[i])
}

rm(list = intersect(ls(), c("vm", "map", "rows", "keep", "known", "qs_cfg", "QS", "qs_dir", "qs_crit", "QS_SEVERITY", "qs_read",
                            "qs_num", "qs_json", "qs_fmt_vals", "qs_pct", "qs_files",
                            "qs_cols", "qs_events", "qs_out", "qs_findings", "qs_path",
                            "qs_tmp")))
