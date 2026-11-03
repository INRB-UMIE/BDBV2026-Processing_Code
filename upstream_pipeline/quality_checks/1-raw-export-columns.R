#!/usr/bin/env Rscript
# 1-raw-export-columns.R
#
# Column checks on every dated raw DHIS2 export (epi linelist + contact tracing):
# which columns arrived, whether any went missing, were renamed or are new, and
# whether every critical column is there. See pipelines/quality_checks/README.md.
#
#   Rscript pipelines/quality_checks/1-raw-export-columns.R
#   QC_REBUILD=1  re-read every export, not just new or changed ones (needed
#                 once after the column profile below changes -- e.g. the
#                 stage 2 fields: dates, untranslated values, duplicates)
#   QC_NO_STOP=1  record critical findings but do not stop the pipeline
#
# INCREMENTAL. Each export is read once and each column's name and contents
# profile stored, with the file's MD5, in raw_export_files.csv /
# raw_export_columns.csv. Later runs read only exports that are new or whose MD5
# changed. An export whose raw file is no longer on disk keeps its history (raw
# files are moving to S3). The findings are then recomputed from the stored
# history in full, every run: that is cheap, and it means a rule change or a new
# known_events.csv row applies to the past too.
#
# CONTENTS. A rename is checked against what the columns hold, not just their
# names: each column's profile is its type (date / number / text / blank), its
# share of blank cells and -- only for small category columns, see
# qc_safe_values() -- its distinct values. A rename whose contents disagree is
# flagged, and two category columns holding the same values are paired as a
# rename even when their names are unalike.
#
# NO ROW-LEVEL DATA is written. The whole export is read to profile it, but the
# outputs hold only column names, types, blank shares, counts, and the values of
# category columns that pass qc_safe_values() (never names, phones, addresses).
#
# The comparison rules are mirrored in tests/reference/quality_checks.py, the
# executable specification tested by tests/test_quality_checks.py. Change both.

# -------------------------  Check if Initialised  -----------------------------
if (!exists("setup_complete") || !setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}

.qc_log <- character(0)
qclog <- function(...) {
  line <- paste0(...)
  message(line)
  .qc_log <<- c(.qc_log, line)
}

qc_cfg <- pipeline_configs$quality_checks
if (is.null(qc_cfg)) stop("pipeline_configs.json has no quality_checks block", call. = FALSE)

QC_OUT_DIR      <- file.path(repo_root, qc_cfg$output_subdir)
QC_EVENTS_PATH  <- file.path(repo_root, qc_cfg$known_events)
QC_SIM_MIN      <- as.numeric(qc_cfg$columns$rename_similarity_min)
QC_CONTAIN_MIN  <- as.integer(qc_cfg$columns$rename_contains_min_chars)
QC_CONTENTS     <- qc_cfg$columns$contents
QC_STRUCTURE    <- qc_cfg$structure
QC_CRITICAL     <- jsonlite::fromJSON(file.path(repo_root, "pipelines", "pipeline_configs.json"),
                                      simplifyVector = FALSE)$quality_checks$critical_columns
QC_REBUILD      <- identical(Sys.getenv("QC_REBUILD"), "1")
QC_NO_STOP      <- identical(Sys.getenv("QC_NO_STOP"), "1")

QC_FILES_PATH    <- file.path(QC_OUT_DIR, "raw_export_files.csv")
QC_COLUMNS_PATH  <- file.path(QC_OUT_DIR, "raw_export_columns.csv")
QC_FINDINGS_PATH <- file.path(QC_OUT_DIR, "raw_export_column_findings.csv")

QC_FILES_COLS    <- c("dataset", "file", "export_date", "md5", "n_rows", "n_columns",
                      "n_blank_headers", "n_duplicate_rows", "n_duplicate_ids",
                      "read_ok", "error", "checked_at")
QC_COLUMNS_COLS  <- c("dataset", "file", "export_date", "position",
                      "raw_name", "standard_name", "mapping",
                      "type", "pct_blank", "n_distinct", "values",
                      "min_date", "max_date", "n_unknown", "unknown_values")
QC_FINDINGS_COLS <- c("dataset", "file", "export_date", "previous_file", "check",
                      "severity", "column", "detail", "contents", "status", "event_note")

# The datasets checked: where their raw exports are, which cleaning script
# names their columns, and which rename map it uses.
QC_DATASETS <- list(
  epi_linelist = list(
    raw_dir = file.path(repo_root, pipeline_configs$epi_linelist$data$raw_subdir),
    script  = file.path(repo_root, "pipelines", "epi_linelist_pipeline", "1-colname-standard.R"),
    map     = "dhis2_epi_column_renames"),
  contact_tracing = list(
    raw_dir = file.path(repo_root, pipeline_configs$contact_tracing$data$raw_subdir),
    script  = file.path(repo_root, "pipelines", "contact_tracing_pipeline", "1-colname-standard.R"),
    map     = "contact_tracing_column_renames")
)


# ------------------------------  Functions  -----------------------------------

# Pull named top-level function definitions out of a cleaning script WITHOUT
# running the script (sourcing it would process every export). The QC step then
# names columns with exactly the code cleaning uses, so the two cannot drift.
qc_functions_from <- function(script, fns) {
  env <- new.env(parent = globalenv())
  for (e in parse(script, keep.source = FALSE, encoding = "UTF-8")) {
    if (is.call(e) && length(e) == 3L &&
        (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% fns &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      eval(e, env)
    }
  }
  missing <- setdiff(fns, ls(env))
  if (length(missing))
    stop(basename(script), " no longer defines: ", paste(missing, collapse = ", "),
         " (quality_checks/1-raw-export-columns.R relies on them)", call. = FALSE)
  env
}

# Export date from the file name: <anything>_DDMMYYYY.<ext>. NA when absent or
# not a real date (the undated early LINELIST_02 ... _07 files).
qc_export_date <- function(file) {
  m <- regmatches(file, regexec("_([0-9]{2})([0-9]{2})([0-9]{4})\\.(xlsx|xls|csv)$",
                                file, ignore.case = TRUE))[[1]]
  if (!length(m)) return(as.Date(NA))
  suppressWarnings(as.Date(sprintf("%s-%s-%s", m[4], m[3], m[2])))
}

# The whole first sheet, read the way cleaning reads it: readxl (which skips
# leading blank rows, as several exports have one) or read.csv.
# .name_repair = "minimal" so duplicate and blank headers are seen as they are,
# not renamed to "...7". guess_max covers every row, so a column blank in its
# first thousand rows is not guessed as logical and its later values lost.
qc_read_export <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                    na.strings = c("", "NA"))
  } else {
    suppressWarnings(readxl::read_excel(path, sheet = 1, .name_repair = "minimal",
                                        guess_max = 1e6))
  }
}

# Values of a column as trimmed text, "" for blank. Dates as YYYY-MM-DD.
qc_as_text <- function(x) {
  v <- if (inherits(x, c("POSIXct", "POSIXt", "Date"))) format(as.Date(x)) else as.character(x)
  v[is.na(v)] <- ""
  trimws(v)
}

# date / number / text / blank. Typed cells (readxl dates and numbers) keep
# their type; text cells count as dates or numbers when >= 90% of the non-blank
# ones look like one (CSV exports arrive as text).
qc_column_type <- function(x, v) {
  nb <- v[nzchar(v)]
  if (!length(nb)) return("blank")
  if (inherits(x, c("POSIXct", "POSIXt", "Date"))) return("date")
  if (is.numeric(x)) return("number")
  if (mean(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}|^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", nb)) >= 0.9)
    return("date")
  if (mean(!is.na(suppressWarnings(as.numeric(nb)))) >= 0.9) return("number")
  "text"
}

# The column's distinct values, sorted, ONLY when storing them cannot reveal a
# person: at most values_max_distinct values, each seen at least
# values_min_count times, all at most values_max_chars long, and the column's
# raw name does not suggest personal data (values_exclude_regex). Otherwise
# none. Category columns (sex, classification, result, zone) pass; names,
# phones, addresses, IDs and free text do not.
qc_safe_values <- function(raw_name, nb, cfg = QC_CONTENTS) {
  if (grepl(cfg$values_exclude_regex, raw_name, perl = TRUE)) return(character(0))
  if (!length(nb)) return(character(0))
  counts <- table(nb)
  if (length(counts) > cfg$values_max_distinct) return(character(0))
  if (min(counts) < cfg$values_min_count) return(character(0))
  if (max(nchar(names(counts))) > cfg$values_max_chars) return(character(0))
  sort(names(counts))
}

qc_values_json <- function(v) if (length(v)) as.character(jsonlite::toJSON(v)) else ""
qc_values_parse <- function(s) if (is.na(s) || !nzchar(s)) character(0) else
  as.character(jsonlite::fromJSON(s))

qc_jaccard <- function(a, b) {
  u <- union(a, b)
  if (!length(u)) return(0)
  length(intersect(a, b)) / length(u)
}

# Do two columns hold the same kind of data? `a`, `b`: one row each of the
# columns table (type, pct_blank, values as JSON).
#   unknown    a profile is missing, or both columns are entirely blank
#   differ     one blank and not the other, different types, blank shares more
#              than blank_tolerance apart, or stored values overlapping less
#              than values_jaccard_match
#   match      same type and blank share, and stored values overlap enough
#   consistent same type and blank share, no stored values to compare
qc_contents_verdict <- function(a, b, cfg = QC_CONTENTS) {
  ta <- a$type; tb <- b$type
  if (is.na(ta) || is.na(tb) || !nzchar(ta) || !nzchar(tb) ||
      (ta == "blank" && tb == "blank")) return("unknown")
  if (ta != tb) return("differ")
  if (abs(as.numeric(a$pct_blank) - as.numeric(b$pct_blank)) > cfg$blank_tolerance)
    return("differ")
  va <- qc_values_parse(a$values); vb <- qc_values_parse(b$values)
  if (length(va) && length(vb))
    return(if (qc_jaccard(va, vb) >= cfg$values_jaccard_match) "match" else "differ")
  "consistent"
}

# Earliest and latest date in a date column, as YYYY-MM-DD. Only plausible
# dates count -- from structure.date_plausible_from to one day after the
# export -- so one typo (2062, 1926) cannot hide a stale export.
qc_date_range <- function(nb, type, export_date,
                          from = as.Date(QC_STRUCTURE$date_plausible_from)) {
  if (type != "date" || !length(nb)) return(c("", ""))
  d <- suppressWarnings(as.Date(substr(nb, 1, 10), format = "%Y-%m-%d"))
  dmy <- suppressWarnings(as.Date(sub("^([0-9]{1,2}/[0-9]{1,2}/[0-9]{4}).*", "\\1", nb),
                                  format = "%d/%m/%Y"))
  d[is.na(d)] <- dmy[is.na(d)]
  d <- d[!is.na(d) & d >= from & d <= export_date + 1]
  if (!length(d)) return(c("", ""))
  c(format(min(d)), format(max(d)))
}

# Values of a translated category column that its data/maps/ table does not
# know -- cleaning keeps them untranslated, so they fall outside every
# category. Matched exactly as translate_mapped_values() matches (trimmed raw
# value against the map's keys). Returns the number of such cells and the
# commonest such values with their counts (at most 10, each at most
# values_max_chars long; longer ones are counted but not stored).
qc_unknown_values <- function(nb, map, cfg = QC_CONTENTS) {
  if (is.null(map) || !length(nb)) return(list(n = "", json = ""))
  bad <- nb[!nb %in% names(map)]
  if (!length(bad)) return(list(n = "0", json = ""))
  tab <- sort(table(bad), decreasing = TRUE)
  tab <- tab[nchar(names(tab)) <= cfg$values_max_chars]
  tab <- utils::head(tab, 10)
  list(n = as.character(length(bad)),
       json = if (length(tab)) as.character(jsonlite::toJSON(as.list(
         stats::setNames(as.integer(tab), names(tab))), auto_unbox = TRUE)) else "")
}

# A rename the map copes with is info, unless the contents disagree.
qc_bump <- function(severity, contents)
  if (identical(contents, "differ") && severity == "info") "warning" else severity

# The name standardize_column_name() makes up when no built-in replacement
# matches: its own last steps, repeated. A standard name that differs from this
# came from the cleaning script's hard-coded replacements, i.e. a deliberate
# rename (mapping "builtin"), not a made-up one (mapping "fallback").
qc_auto_name <- function(raw, fns) {
  vapply(raw, function(x) {
    x <- fns$strip_column_prefixes(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- gsub("'", "", x, fixed = TRUE)
    x <- gsub("[^[:alnum: ]]", " ", x)
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    gsub("^_|_$", "", x)
  }, character(1), USE.NAMES = FALSE)
}

# Standard name and mapping for raw column names, from the CURRENT rename map
# and cleaning code. Used when an export is read and again, every run, over
# the whole history: a rename-map edit then shows up straight away without
# re-reading old exports.
qc_name_columns <- function(nm, fns, rename_map) {
  explicit <- vapply(nm, fns$find_explicit_rename, character(1),
                     mapping = rename_map, USE.NAMES = FALSE)
  std <- ifelse(is.na(explicit),
                vapply(nm, fns$standardize_column_name, character(1), USE.NAMES = FALSE),
                explicit)
  list(standard_name = std,
       mapping = ifelse(!is.na(explicit), "explicit",
                        ifelse(std != qc_auto_name(nm, fns), "builtin", "fallback")))
}

# Comparison key for "is this a renamed column?": prefixes stripped the way
# cleaning strips them, accents dropped, lower case, letters and digits only.
qc_key <- function(raw, fns) {
  k <- vapply(raw, fns$strip_column_prefixes, character(1), USE.NAMES = FALSE)
  k <- iconv(k, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  gsub("[^a-z0-9]", "", tolower(k))
}

# 1 - Levenshtein distance / length of the longer key. 1 = identical.
qc_similarity <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) return(0)
  1 - as.numeric(utils::adist(a, b)) / max(nchar(a), nchar(b))
}

# One key wholly inside the other, the shorter at least `min_chars` long: a
# label that was extended or shortened ("Date d'enregistrement" -> "Date
# d'enregistrement du contact", CT 21 Oct 2026), which edit distance misses.
qc_contains <- function(a, b, min_chars) {
  short <- if (nchar(a) <= nchar(b)) a else b
  long  <- if (nchar(a) <= nchar(b)) b else a
  nchar(short) >= min_chars && grepl(short, long, fixed = TRUE)
}

qc_read_csv <- function(path, cols) {
  if (!file.exists(path)) {
    return(as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                         stringsAsFactors = FALSE))
  }
  d <- utils::read.csv(path, colClasses = "character", fileEncoding = "UTF-8",
                       na.strings = character(0), check.names = FALSE)
  for (c in setdiff(cols, names(d))) d[[c]] <- rep("", nrow(d))
  d[, cols, drop = FALSE]
}

# Written to a temporary name and renamed, so a reader never sees half a file.
qc_write_csv <- function(d, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  utils::write.csv(d, tmp, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  if (!file.rename(tmp, path)) { unlink(tmp); stop("Could not write ", path, call. = FALSE) }
  invisible(path)
}

# Is each critical column present in one export? `cols` is that export's rows
# of the columns table. A critical entry is satisfied by any column whose
# standard name is in its standard_names, or whose raw name matches raw_regex.
qc_critical_absent <- function(cols, critical) {
  absent <- character(0)
  for (cr in critical) {
    std_ok <- length(cr$standard_names) && any(cols$standard_name %in% cr$standard_names)
    raw_ok <- is.character(cr$raw_regex) && length(cr$raw_regex) == 1L &&
              !is.na(cr$raw_regex) && nzchar(cr$raw_regex) &&
              any(grepl(cr$raw_regex, cols$raw_name, perl = TRUE))
    if (!std_ok && !raw_ok) absent <- c(absent, cr$label)
  }
  absent
}

# Compare one export's columns with the previous export's. Returns a data
# frame of findings (check, severity, column, detail, contents). Order:
#   1. a gone column and an added one with the SAME standard name: renamed,
#      and the rename map already copes (info; warning if contents differ)
#   2. remaining gone/added pairs by name similarity >= QC_SIM_MIN, or one
#      name containing the other (shorter >= QC_CONTAIN_MIN characters), best
#      pair first, one-to-one: likely renamed (warning)
#   2c. remaining pairs whose contents "match" with stored values overlapping
#      >= contents_only_jaccard and sharing >= contents_only_min_shared values,
#      best first, one-to-one: likely renamed on contents alone (warning)
#   3. remaining gone: column_missing (warning)
#   4. remaining added: new_column (info) if the rename map knows it, else
#      new_column_unmapped (warning)
# Every rename finding carries the contents verdict (qc_contents_verdict).
qc_compare_columns <- function(prev, cur, sim_min, contain_min, cfg = QC_CONTENTS) {
  out <- list()
  add <- function(check, severity, column, detail, contents = "")
    out[[length(out) + 1L]] <<- data.frame(check = check, severity = severity,
                                           column = column, detail = detail,
                                           contents = contents, stringsAsFactors = FALSE)
  prev <- prev[!duplicated(prev$raw_name), , drop = FALSE]
  cur  <- cur[!duplicated(cur$raw_name), , drop = FALSE]
  gone  <- prev[!prev$raw_name %in% cur$raw_name, , drop = FALSE]
  added <- cur[!cur$raw_name %in% prev$raw_name, , drop = FALSE]
  gone_used <- rep(FALSE, nrow(gone)); added_used <- rep(FALSE, nrow(added))

  # 1. same standard name
  for (i in seq_len(nrow(gone))) {
    j <- which(!added_used & added$standard_name == gone$standard_name[i])[1]
    if (!is.na(j)) {
      gone_used[i] <- TRUE; added_used[j] <- TRUE
      v <- qc_contents_verdict(gone[i, ], added[j, ], cfg)
      add("renamed_mapped", qc_bump("info", v), added$raw_name[j],
          paste0("was '", gone$raw_name[i], "'; both become '", added$standard_name[j], "'"), v)
    }
  }

  # 2. similar names
  gi <- which(!gone_used); ai <- which(!added_used)
  if (length(gi) && length(ai)) {
    pairs <- expand.grid(g = gi, a = ai)
    pairs$sim <- mapply(function(g, a) qc_similarity(gone$key[g], added$key[a]),
                        pairs$g, pairs$a)
    pairs$contains <- mapply(function(g, a) qc_contains(gone$key[g], added$key[a], contain_min),
                             pairs$g, pairs$a)
    pairs <- pairs[pairs$sim >= sim_min | pairs$contains, , drop = FALSE]
    pairs <- pairs[order(-pairs$sim, as.integer(gone$position[pairs$g]),
                         as.integer(added$position[pairs$a])), , drop = FALSE]
    for (k in seq_len(nrow(pairs))) {
      g <- pairs$g[k]; a <- pairs$a[k]
      if (gone_used[g] || added_used[a]) next
      gone_used[g] <- TRUE; added_used[a] <- TRUE
      v <- qc_contents_verdict(gone[g, ], added[a, ], cfg)
      add("likely_renamed", "warning", added$raw_name[a],
          sprintf("was '%s' (%s); cleaning names it '%s', previously '%s'",
                  gone$raw_name[g],
                  if (pairs$sim[k] >= sim_min) sprintf("similarity %.2f", pairs$sim[k])
                  else "one name contains the other",
                  added$standard_name[a], gone$standard_name[g]), v)
    }
  }

  # 2c. contents alone
  gi <- which(!gone_used); ai <- which(!added_used)
  if (length(gi) && length(ai)) {
    pairs <- expand.grid(g = gi, a = ai)
    pairs$ok <- FALSE; pairs$jac <- 0
    for (k in seq_len(nrow(pairs))) {
      g <- pairs$g[k]; a <- pairs$a[k]
      vg <- qc_values_parse(gone$values[g]); va <- qc_values_parse(added$values[a])
      if (!length(vg) || !length(va)) next
      if (qc_contents_verdict(gone[g, ], added[a, ], cfg) != "match") next
      pairs$jac[k] <- qc_jaccard(vg, va)
      pairs$ok[k] <- pairs$jac[k] >= cfg$contents_only_jaccard &&
                     length(intersect(vg, va)) >= cfg$contents_only_min_shared
    }
    pairs <- pairs[pairs$ok, , drop = FALSE]
    pairs <- pairs[order(-pairs$jac, as.integer(gone$position[pairs$g]),
                         as.integer(added$position[pairs$a])), , drop = FALSE]
    for (k in seq_len(nrow(pairs))) {
      g <- pairs$g[k]; a <- pairs$a[k]
      if (gone_used[g] || added_used[a]) next
      gone_used[g] <- TRUE; added_used[a] <- TRUE
      add("likely_renamed", "warning", added$raw_name[a],
          sprintf("was '%s' (names differ, but both hold the same values: overlap %.2f); cleaning names it '%s', previously '%s'",
                  gone$raw_name[g], pairs$jac[k], added$standard_name[a], gone$standard_name[g]),
          "match")
    }
  }

  # 3. gone
  for (i in which(!gone_used))
    add("column_missing", "warning", gone$raw_name[i],
        paste0("was column ", gone$position[i], " ('", gone$standard_name[i], "')"))

  # 4. added
  for (j in which(!added_used)) {
    if (added$mapping[j] != "fallback") {
      add("new_column", "info", added$raw_name[j],
          paste0("column ", added$position[j], "; in the rename map as '", added$standard_name[j], "'"))
    } else {
      add("new_column_unmapped", "warning", added$raw_name[j],
          paste0("column ", added$position[j], "; not in the rename map, cleaning will call it '",
                 added$standard_name[j], "'"))
    }
  }

  if (!length(out)) return(data.frame(check = character(0), severity = character(0),
                                      column = character(0), detail = character(0),
                                      contents = character(0), stringsAsFactors = FALSE))
  do.call(rbind, out)
}

# Mark findings that a known event explains. An event matches on dataset and
# export date, and on check and column when it gives them (blank = any).
qc_apply_events <- function(findings, events) {
  findings$status <- rep("new", nrow(findings))
  findings$event_note <- rep("", nrow(findings))
  for (k in seq_len(nrow(events))) {
    ev <- events[k, ]
    hit <- findings$dataset == ev$dataset & findings$export_date == ev$export_date &
           (!nzchar(ev$check)  | findings$check  == ev$check) &
           (!nzchar(ev$column) | findings$column == ev$column)
    findings$status[hit] <- "expected"
    findings$event_note[hit] <- ev$note
  }
  findings
}


# ------------------------  1. Read new exports  --------------------------------
qclog("QUALITY CHECKS: raw export columns", if (QC_REBUILD) " (full rebuild)")

files_hist <- qc_read_csv(QC_FILES_PATH, QC_FILES_COLS)
cols_hist  <- qc_read_csv(QC_COLUMNS_PATH, QC_COLUMNS_COLS)
now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

for (ds in names(QC_DATASETS)) {
  spec <- QC_DATASETS[[ds]]
  fns  <- qc_functions_from(spec$script, c("normalize_key", "trim_names",
                                           "find_explicit_rename", "strip_column_prefixes",
                                           "standardize_column_name"))
  rename_map <- load_map_csv(spec$map)
  # The value translation tables cleaning applies, by standard column name.
  vmap_names <- unlist(QC_STRUCTURE$value_maps[[ds]])
  vmaps <- lapply(vmap_names, load_map_csv)

  raw <- list.files(spec$raw_dir, pattern = "\\.(xlsx|xls|csv)$", ignore.case = TRUE)
  dates <- do.call(c, lapply(raw, qc_export_date))
  if (any(is.na(dates)))
    qclog("  ", ds, ": skipped (no date in file name): ", paste(raw[is.na(dates)], collapse = ", "))
  raw <- raw[!is.na(dates)]; dates <- dates[!is.na(dates)]

  n_read <- 0L
  for (k in seq_along(raw)) {
    f <- raw[k]; path <- file.path(spec$raw_dir, f)
    md5 <- unname(tools::md5sum(path))
    old <- files_hist$dataset == ds & files_hist$file == f
    if (!QC_REBUILD && any(old) && identical(files_hist$md5[old][1], md5)) next
    if (any(old) && !QC_REBUILD) qclog("  ", ds, ": ", f, " has changed since it was checked; re-reading")

    df <- tryCatch(qc_read_export(path), error = function(e) e)
    ok <- !inherits(df, "error")
    cols <- NULL; n_blank <- 0L; n_dup_rows <- NA_integer_; n_dup_ids <- NA_integer_
    if (ok) {
      nm <- fns$trim_names(names(df))
      pos <- seq_along(nm)
      blank <- is.na(nm) | !nzchar(nm)
      n_blank <- sum(blank)
      keep <- which(!blank)
      if (length(keep)) {
        nm <- nm[keep]
        named <- qc_name_columns(nm, fns, rename_map)
        prof <- lapply(seq_along(keep), function(i) {
          x <- df[[keep[i]]]
          v <- qc_as_text(x)
          nb <- v[nzchar(v)]
          ty <- qc_column_type(x, v)
          rng <- qc_date_range(nb, ty, dates[k])
          unk <- qc_unknown_values(nb, vmaps[[named$standard_name[i]]])
          list(type = ty,
               pct_blank = if (length(v)) sprintf("%.4f", 1 - length(nb) / length(v)) else "",
               n_distinct = as.character(length(unique(nb))),
               values = qc_values_json(qc_safe_values(nm[i], nb)),
               min_date = rng[1], max_date = rng[2],
               n_unknown = unk$n, unknown_values = unk$json)
        })
        # Exact duplicate rows, and (epi) the same alert_id on two rows.
        n_dup_rows <- sum(duplicated(as.data.frame(df)[, keep, drop = FALSE]))
        id_col <- keep[named$standard_name == "alert_id"][1]
        n_dup_ids <- if (ds == "epi_linelist" && !is.na(id_col)) {
          ids <- trimws(as.character(df[[id_col]])); ids <- ids[!is.na(ids) & nzchar(ids)]
          sum(duplicated(ids))
        } else NA_integer_
        cols <- data.frame(dataset = ds, file = f, export_date = format(dates[k]),
                           position = as.character(pos[keep]), raw_name = nm,
                           standard_name = named$standard_name,
                           mapping = named$mapping,
                           type = vapply(prof, `[[`, "", "type"),
                           pct_blank = vapply(prof, `[[`, "", "pct_blank"),
                           n_distinct = vapply(prof, `[[`, "", "n_distinct"),
                           values = vapply(prof, `[[`, "", "values"),
                           min_date = vapply(prof, `[[`, "", "min_date"),
                           max_date = vapply(prof, `[[`, "", "max_date"),
                           n_unknown = vapply(prof, `[[`, "", "n_unknown"),
                           unknown_values = vapply(prof, `[[`, "", "unknown_values"),
                           stringsAsFactors = FALSE)
      }
    }
    files_hist <- files_hist[!old, , drop = FALSE]
    cols_hist  <- cols_hist[!(cols_hist$dataset == ds & cols_hist$file == f), , drop = FALSE]
    files_hist <- rbind(files_hist, data.frame(
      dataset = ds, file = f, export_date = format(dates[k]), md5 = md5,
      n_rows = if (ok) as.character(nrow(df)) else "",
      n_columns = as.character(if (is.null(cols)) 0L else nrow(cols)),
      n_blank_headers = as.character(n_blank),
      n_duplicate_rows = if (is.na(n_dup_rows)) "" else as.character(n_dup_rows),
      n_duplicate_ids  = if (is.na(n_dup_ids)) "" else as.character(n_dup_ids),
      read_ok = if (ok) "TRUE" else "FALSE",
      error = if (ok) "" else gsub("[\r\n]+", " ", conditionMessage(df)),
      checked_at = now, stringsAsFactors = FALSE))
    if (!is.null(cols)) cols_hist <- rbind(cols_hist, cols[, QC_COLUMNS_COLS])
    rm(df)
    n_read <- n_read + 1L
  }
  qclog("  ", ds, ": ", length(raw), " dated export(s) on disk, ", n_read, " read this run")

  # Re-name the whole history with the current rename map and cleaning code.
  in_ds <- cols_hist$dataset == ds
  if (any(in_ds)) {
    u <- unique(cols_hist$raw_name[in_ds])
    named <- qc_name_columns(u, fns, rename_map)
    m <- match(cols_hist$raw_name[in_ds], u)
    cols_hist$standard_name[in_ds] <- named$standard_name[m]
    cols_hist$mapping[in_ds]       <- named$mapping[m]
  }
}

files_hist <- files_hist[order(files_hist$dataset, files_hist$export_date, files_hist$file), , drop = FALSE]
cols_hist  <- cols_hist[order(cols_hist$dataset, cols_hist$export_date, cols_hist$file,
                              as.integer(cols_hist$position)), , drop = FALSE]
qc_write_csv(files_hist, QC_FILES_PATH)
qc_write_csv(cols_hist,  QC_COLUMNS_PATH)


# ----------------------  2. Findings from the whole history  -------------------
# Known events: the hand-kept file plus those marked "expected" in the app.
events <- rbind(
  qc_read_csv(QC_EVENTS_PATH, c("dataset", "export_date", "check", "column", "note")),
  qc_read_csv(file.path(repo_root, qc_cfg$known_events_app),
              c("dataset", "export_date", "check", "column", "note")))
findings <- list()
latest_by_ds <- list()

for (ds in names(QC_DATASETS)) {
  fns <- qc_functions_from(QC_DATASETS[[ds]]$script, "strip_column_prefixes")
  # Read unsimplified: as a data frame its ragged standard_names arrays and
  # absent raw_regex fields come back as list columns and NAs.
  critical <- QC_CRITICAL[[ds]]
  critical <- lapply(critical, function(cr) {
    cr$standard_names <- as.character(unlist(cr$standard_names)); cr })

  fh <- files_hist[files_hist$dataset == ds, , drop = FALSE]
  if (!nrow(fh)) next
  latest_by_ds[[ds]] <- fh$file[nrow(fh)]
  ch <- cols_hist[cols_hist$dataset == ds, , drop = FALSE]
  ch$key <- qc_key(ch$raw_name, fns)

  prev_file <- ""
  for (k in seq_len(nrow(fh))) {
    f <- fh$file[k]
    rec <- function(d) {
      if (!nrow(d)) return(NULL)
      if (is.null(d$contents)) d$contents <- rep("", nrow(d))
      cbind(data.frame(dataset = ds, file = f, export_date = fh$export_date[k],
                       previous_file = prev_file, stringsAsFactors = FALSE), d)
    }
    if (fh$read_ok[k] != "TRUE") {
      findings[[length(findings) + 1L]] <- rec(data.frame(
        check = "read_failed", severity = "critical", column = "",
        detail = fh$error[k], stringsAsFactors = FALSE))
      next   # the next export is compared with the last one that could be read
    }
    cur <- ch[ch$file == f, , drop = FALSE]

    absent <- qc_critical_absent(cur, critical)
    if (length(absent))
      findings[[length(findings) + 1L]] <- rec(data.frame(
        check = "critical_missing", severity = "critical", column = absent,
        detail = "critical column not found", stringsAsFactors = FALSE))

    dups <- unique(cur$raw_name[duplicated(cur$raw_name)])
    if (length(dups))
      findings[[length(findings) + 1L]] <- rec(data.frame(
        check = "duplicate_column", severity = "warning", column = dups,
        detail = "raw name appears more than once", stringsAsFactors = FALSE))

    if (nzchar(prev_file)) {
      prev <- ch[ch$file == prev_file, , drop = FALSE]
      findings[[length(findings) + 1L]] <- rec(qc_compare_columns(prev, cur, QC_SIM_MIN,
                                                                  QC_CONTAIN_MIN))
    }
    prev_file <- f
  }
}

findings <- Filter(Negate(is.null), findings)
findings <- if (length(findings)) do.call(rbind, findings) else
  data.frame(dataset = character(0), file = character(0), export_date = character(0),
             previous_file = character(0), check = character(0), severity = character(0),
             column = character(0), detail = character(0), contents = character(0),
             stringsAsFactors = FALSE)
findings <- qc_apply_events(findings, events)
findings <- findings[, QC_FINDINGS_COLS, drop = FALSE]
qc_write_csv(findings, QC_FINDINGS_PATH)

# Summary: per dataset, counts by severity over all exports, then the latest
# export's findings in full.
for (ds in names(latest_by_ds)) {
  fd <- findings[findings$dataset == ds & findings$status == "new", , drop = FALSE]
  tab <- table(factor(fd$severity, levels = c("critical", "warning", "info")))
  qclog("  ", ds, ": all exports: ", tab[["critical"]], " critical, ",
        tab[["warning"]], " warning, ", tab[["info"]], " info")
  lf <- fd[fd$file == latest_by_ds[[ds]], , drop = FALSE]
  qclog("  ", ds, ": latest export ", latest_by_ds[[ds]], ": ",
        if (!nrow(lf)) "no findings" else paste0(nrow(lf), " finding(s)"))
  for (i in seq_len(nrow(lf)))
    qclog("    [", lf$severity[i], "] ", lf$check[i], ": ", lf$column[i], " -- ", lf$detail[i],
          if (nzchar(lf$contents[i])) paste0(" [contents: ", lf$contents[i], "]"))
}
write_log_file(.qc_log, "quality_checks_columns_log.txt")

# Stage 2 structure checks (2-raw-export-structure.R): from the history just
# written, no files re-read. Warnings only, so never fatal here either.
tryCatch(source(file.path(repo_root, "pipelines", "quality_checks", "2-raw-export-structure.R"),
                local = FALSE),
         error = function(e) message("  structure checks not run: ", conditionMessage(e)))

# Column timeline charts (plot-column-timeline.R). Never fatal: a chart that
# fails to draw must not stop the pipeline.
tryCatch(source(file.path(repo_root, "pipelines", "quality_checks", "plot-column-timeline.R"),
                local = FALSE),
         error = function(e) message("  column timeline charts not drawn: ", conditionMessage(e)))


# --------------------  3. Stop on a critical latest export  --------------------
is_latest <- vapply(seq_len(nrow(findings)), function(i)
  identical(latest_by_ds[[findings$dataset[i]]], findings$file[i]), logical(1))
stopping <- findings[findings$severity == "critical" & findings$status == "new" &
                     is_latest, , drop = FALSE]
if (nrow(stopping) && !QC_NO_STOP) {
  stop("Quality checks: critical problem in the latest export -- ",
       paste0(stopping$file, ": ", stopping$check, " ", stopping$column, collapse = "; "),
       ". See ", QC_FINDINGS_PATH, ". If this is expected, add it to ",
       qc_cfg$known_events, "; to run anyway, set QC_NO_STOP=1.", call. = FALSE)
}

qc_columns_complete <- TRUE


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, "qc_columns_complete")
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
