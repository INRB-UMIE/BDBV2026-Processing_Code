#!/usr/bin/env Rscript
# 3-metrics-history.R
#
# Stage 3 quality checks: a GROWING history of counts and shares in the
# CLEANED data -- rows, IDs, case classification, lab result, sex, age group,
# outcome, province, health zone; for contact tracing follow-up result and
# contact status -- one set per export, so they can be plotted over time and
# sudden shifts flagged. See pipelines/quality_checks/README.md.
#
#   Rscript pipelines/quality_checks/3-metrics-history.R
#   QC_REBUILD=1   recompute every export, not just new or changed ones (the
#                  combined table's past snapshots are kept: they cannot be
#                  recomputed)
#
# THREE SOURCES, each its own `dataset` in the history:
#   epi_linelist              every processed linelist export
#   contact_tracing           every processed CT export -- its own window, so
#                             its shares shift as the window moves (27 Aug to
#                             20 Oct: ~4 weeks); kept because it is the only
#                             CT history that can be rebuilt
#   contact_tracing_combined  the all-time combined table, one snapshot per
#                             build (dated by its latest export). It is rebuilt
#                             in place, so its past cannot be recomputed: the
#                             history starts the first time this runs.
#
# subset "all" is every row (or contact); "new" is alert_ids (epi) / contact_ids
# (CT) not in the previous export -- for the combined table, contacts first seen
# in its latest export.
#
# INCREMENTAL: each source file's size and modification time are kept in
# metrics_sources.csv; only new or changed ones are recomputed.
#
# Outputs (counts and shares only, no row-level data), in
# data/processed/quality_checks/:
#   metrics_history.csv    dataset, source, export_date, subset, measure,
#                          category, n, denominator, share
#   metrics_sources.csv    which source files the history was computed from
#   metrics_findings.csv   shares that jumped (same columns as the other
#                          findings files)
#
# The share-jump rule is mirrored in tests/reference/quality_checks.py
# (share_findings). Change both.

# -------------------------  Check if Initialised  -----------------------------
if (!exists("setup_complete") || !setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}

qm_cfg <- pipeline_configs$quality_checks
QM     <- qm_cfg$metrics
qm_dir <- file.path(repo_root, qm_cfg$output_subdir)
QM_HIST_PATH <- file.path(qm_dir, "metrics_history.csv")
QM_SRC_PATH  <- file.path(qm_dir, "metrics_sources.csv")
QM_FIND_PATH <- file.path(qm_dir, "metrics_findings.csv")
QM_COLS      <- c("dataset", "source", "export_date", "subset", "measure",
                  "category", "n", "denominator", "share")
QM_REBUILD   <- identical(Sys.getenv("QC_REBUILD"), "1")

QM_EPI_DIR <- file.path(repo_root, pipeline_configs$epi_linelist$data$processed_subdir)
QM_CT_DIR  <- file.path(repo_root, pipeline_configs$contact_tracing$data$processed_subdir)


# ------------------------------  Functions  -----------------------------------

qm_read <- function(path, cols = NULL) {
  if (!file.exists(path)) return(NULL)
  if (is.null(cols))
    return(utils::read.csv(path, colClasses = "character", fileEncoding = "UTF-8",
                           na.strings = character(0), check.names = FALSE))
  hdr <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  cc <- ifelse(hdr %in% cols, "character", "NULL")
  # No fileEncoding here: read the processed files exactly as the pipeline's
  # own steps do (2b-combine-exports.R's ctc_read_cols()).
  d <- utils::read.csv(path, colClasses = cc, na.strings = character(0),
                       check.names = FALSE)
  for (cl in setdiff(cols, names(d))) d[[cl]] <- rep(NA_character_, nrow(d))
  d[, cols, drop = FALSE]
}

qm_write <- function(d, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  utils::write.csv(d, tmp, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  if (!file.rename(tmp, path)) { unlink(tmp); stop("Could not write ", path, call. = FALSE) }
}

# Blank and NA both become "(blank)", so a rise in missing values shows.
qm_cat <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "(blank)"
  x
}

# Age in years -> band. Bands from pipeline_configs (metrics.age_bands: the
# lower bound of each). Not a number -> "(blank)"; < 0 or > 120 -> "(invalid)".
qm_age_group <- function(age) {
  a <- suppressWarnings(as.numeric(age))
  lo <- as.numeric(unlist(QM$age_bands))
  labs <- c(paste0(lo[-length(lo)], "-", lo[-1] - 1), paste0(lo[length(lo)], "+"))
  out <- as.character(cut(a, c(lo, Inf), labels = labs, right = FALSE))
  out[is.na(a)] <- "(blank)"
  out[!is.na(a) & (a < 0 | a > 120)] <- "(invalid)"
  out
}

qm_date_of <- function(stem) {
  m <- regmatches(stem, regexpr("[0-9]{8}$", stem))
  if (!length(m)) return(as.Date(NA))
  as.Date(m, format = "%d%m%Y")
}

# One measure's categories as rows.
qm_shares <- function(x, measure) {
  if (!length(x)) return(NULL)
  t <- table(x)
  data.frame(measure = measure, category = names(t), n = as.integer(t),
             denominator = length(x), share = as.numeric(t) / length(x),
             stringsAsFactors = FALSE)
}
qm_scalar <- function(measure, value)
  data.frame(measure = measure, category = "", n = as.integer(value),
             denominator = NA_integer_, share = NA_real_, stringsAsFactors = FALSE)

# ---- every column (added 2026-10-30) ----------------------------------------
# On top of the hand-picked measures below, one measure per column of the
# processed file, named "col.<column>":
#   - a small category column: the share of each value, plus "(blank)";
#   - anything else (IDs, dates, numbers, free text, personal data): only
#     "(filled)" vs "(blank)", so a column going empty or filling up shows.
# Values are stored under the SAME privacy rule as stage 1's column contents
# (quality_checks.columns.contents): at most values_max_distinct values, each
# at most values_max_chars long; values seen fewer than values_min_count times
# are pooled as "(rare values)". Never for a pii_columns column or one whose
# name matches QM_COL_EXCLUDE (names, phones, addresses, places finer than a
# health area, form fillers, IDs). Subset "all" only.
# Not drawn in the nightly PNGs (too many panels); every one is selectable in
# the app's Data Quality -> Metrics tab.
QM_COL_PREFIX  <- "col."
QM_COL_EXCLUDE <- paste0("name|nom|phone|telephone|address|adresse|coord|chef|",
                         "contaminante|remplie|equipe|team|investigat|village|",
                         "quartier|uid|identif|numero|sample_number|alert_id|",
                         "contact_id|(^|_)id($|_)|specified|precis|comment|remarq")
qm_column_measures <- function(d, dataset) {
  if (is.null(d) || !nrow(d)) return(NULL)
  cc  <- qm_cfg$columns$contents
  pii <- unlist(pipeline_configs$pii_columns[[dataset]])
  out <- lapply(names(d), function(cl) {
    x <- trimws(as.character(d[[cl]])); x[is.na(x)] <- ""
    filled <- nzchar(x)
    vals <- table(x[filled])
    categorical <- !(cl %in% pii) &&
      !grepl(QM_COL_EXCLUDE, cl, ignore.case = TRUE, perl = TRUE) &&
      length(vals) >= 1 && length(vals) <= cc$values_max_distinct &&
      all(nchar(names(vals), type = "chars", allowNA = TRUE) <= cc$values_max_chars,
          na.rm = TRUE)
    cat <- if (categorical) {
      v <- ifelse(filled, x, "(blank)")
      v[v %in% names(vals)[vals < cc$values_min_count]] <- "(rare values)"
      v
    } else ifelse(filled, "(filled)", "(blank)")
    qm_shares(cat, paste0(QM_COL_PREFIX, cl))
  })
  do.call(rbind, out)
}

# A whole processed file, every column as text, old column names brought onto
# the current ones (data/maps/processed_column_aliases.csv). `need` columns
# missing from the file are added as NA, as qm_read() does.
qm_read_all <- function(path, dataset, need = character(0)) {
  if (!file.exists(path)) return(NULL)
  d <- utils::read.csv(path, colClasses = "character", na.strings = character(0),
                       check.names = FALSE)
  d <- apply_column_aliases(d, dataset)
  for (cl in setdiff(need, names(d))) d[[cl]] <- rep(NA_character_, nrow(d))
  d
}

# Tag a block of measures with where it came from.
qm_tag <- function(d, dataset, source, date, subset) {
  if (is.null(d) || !nrow(d)) return(NULL)
  cbind(data.frame(dataset = dataset, source = source, export_date = format(date),
                   subset = subset, stringsAsFactors = FALSE), d)
}

# ---- epi linelist: one processed export ------------------------------------
QM_EPI_COLS <- c("alert_id", "alert_id_assigned", "final_mve_case_classification",
                 "genexpert_result", "sex", "age", "type_of_alert",
                 "sample_collection_status", "province", "health_zone")

qm_epi_measures <- function(d) {
  rbind(
    qm_shares(qm_cat(d$final_mve_case_classification), "case_classification"),
    qm_shares(qm_cat(d$genexpert_result), "genexpert_result"),
    qm_shares(qm_cat(d$sex), "sex"),
    qm_shares(qm_age_group(d$age), "age_group"),
    qm_shares(qm_cat(d$type_of_alert), "type_of_alert"),
    qm_shares(qm_cat(d$sample_collection_status), "sample_collection_status"),
    # The two together: does an alert's type agree with the patient's status
    # at sampling (a "death" alert sampled "alive")? Drawn as a Sankey.
    qm_shares(paste(qm_cat(d$type_of_alert), qm_cat(d$sample_collection_status),
                    sep = " -> "), "alert_type_x_sample_status"),
    qm_shares(qm_cat(d$province), "province"),
    qm_shares(qm_cat(d$health_zone), "health_zone"))
}

qm_epi_export <- function(path, stem, date, prev_ids) {
  d <- qm_read_all(path, "epi_linelist", QM_EPI_COLS)
  ids <- trimws(d$alert_id)
  is_new <- nzchar(ids) & !is.na(ids) & !(ids %in% prev_ids)
  out <- rbind(
    qm_tag(rbind(qm_scalar("n_rows", nrow(d)),
                 qm_scalar("n_alert_ids", length(unique(ids[!is.na(ids) & nzchar(ids)]))),
                 qm_scalar("n_assigned_ids", sum(nzchar(qm_blank(d$alert_id_assigned)))),
                 qm_scalar("n_new_rows", sum(is_new)),
                 qm_epi_measures(d),
                 qm_column_measures(d, "epi_linelist")), "epi_linelist", stem, date, "all"),
    qm_tag(qm_epi_measures(d[is_new, , drop = FALSE]), "epi_linelist", stem, date, "new"))
  list(rows = out, ids = unique(ids[!is.na(ids) & nzchar(ids)]))
}
qm_blank <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }

# ---- contact tracing: per-row and per-contact measures ---------------------
QM_CT_COLS <- c("contact_id", "followup_result", "contact_sex", "contact_age",
                "health_zone")

qm_ct_measures <- function(d) {
  first <- d[!duplicated(d$contact_id), , drop = FALSE]   # one row per contact
  rbind(
    qm_shares(qm_cat(d$followup_result), "followup_result"),          # per follow-up row
    qm_shares(qm_cat(first$contact_sex), "contact_sex"),               # per contact
    qm_shares(qm_age_group(first$contact_age), "age_group"),
    qm_shares(qm_cat(first$health_zone), "health_zone"))
}

qm_ct_export <- function(path, stem, date, prev_ids) {
  d <- qm_read_all(path, "contact_tracing", QM_CT_COLS)
  d$contact_id <- trimws(d$contact_id)
  d <- d[!is.na(d$contact_id) & nzchar(d$contact_id), , drop = FALSE]
  is_new <- !(d$contact_id %in% prev_ids)
  ids <- unique(d$contact_id)
  out <- rbind(
    qm_tag(rbind(qm_scalar("n_rows", nrow(d)),
                 qm_scalar("n_contacts", length(ids)),
                 qm_scalar("n_new_contacts", length(unique(d$contact_id[is_new]))),
                 qm_ct_measures(d),
                 qm_column_measures(d, "contact_tracing")), "contact_tracing", stem, date, "all"),
    qm_tag(qm_ct_measures(d[is_new, , drop = FALSE]), "contact_tracing", stem, date, "new"))
  list(rows = out, ids = ids)
}

qm_ct_combined <- function(rows_path, contacts_path, latest, date) {
  cts <- qm_read(contacts_path, c("contact_id", "contact_status", "first_export",
                                  "source_case_changed"))
  d <- qm_read_all(rows_path, "contact_tracing", QM_CT_COLS)
  d$contact_id <- trimws(d$contact_id)
  new_ids <- cts$contact_id[cts$first_export == latest]
  is_new <- d$contact_id %in% new_ids
  rbind(
    qm_tag(rbind(qm_scalar("n_rows", nrow(d)),
                 qm_scalar("n_contacts", nrow(cts)),
                 qm_scalar("n_new_contacts", length(new_ids)),
                 qm_scalar("n_source_case_changed", sum(toupper(cts$source_case_changed) == "TRUE")),
                 qm_shares(qm_cat(cts$contact_status), "contact_status"),
                 qm_ct_measures(d),
                 qm_column_measures(d, "contact_tracing")),
           "contact_tracing_combined", latest, date, "all"),
    qm_tag(qm_ct_measures(d[is_new, , drop = FALSE]), "contact_tracing_combined",
           latest, date, "new"))
}

qm_signature <- function(paths)
  paste(file.size(paths), format(file.mtime(paths), "%Y-%m-%dT%H:%M:%S"),
        sep = "@", collapse = ";")


# ------------------------  1. Update the history  ------------------------------
message("QUALITY CHECKS: metrics history", if (QM_REBUILD) " (full rebuild)")

hist <- qm_read(QM_HIST_PATH)
srcs <- qm_read(QM_SRC_PATH)
# A rebuild recomputes every export -- but NOT the combined table's past
# snapshots, which cannot be recomputed (it is rebuilt in place). Those are
# kept; only its latest snapshot is recomputed (see below). Before 2026-10-30
# a rebuild silently dropped them.
if (QM_REBUILD) {
  if (!is.null(hist)) hist <- hist[hist$dataset == "contact_tracing_combined", , drop = FALSE]
  if (!is.null(srcs)) srcs <- srcs[srcs$dataset == "contact_tracing_combined", , drop = FALSE]
}
if (is.null(hist)) hist <- as.data.frame(setNames(replicate(length(QM_COLS), character(0),
                                                            simplify = FALSE), QM_COLS))
if (is.null(srcs)) srcs <- data.frame(dataset = character(0), source = character(0),
                                      signature = character(0), stringsAsFactors = FALSE)
new_rows <- list(); new_srcs <- list()
qm_done <- function(ds, src, sig)
  any(srcs$dataset == ds & srcs$source == src & srcs$signature == sig)
qm_store <- function(ds, src, sig, rows) {
  new_rows[[length(new_rows) + 1L]] <<- rows
  new_srcs[[length(new_srcs) + 1L]] <<- data.frame(dataset = ds, source = src,
                                                   signature = sig, stringsAsFactors = FALSE)
  hist <<- hist[!(hist$dataset == ds & hist$source == src), , drop = FALSE]
  srcs <<- srcs[!(srcs$dataset == ds & srcs$source == src), , drop = FALSE]
}

# Exports in date order. New or changed exports are computed; so is the one
# after a CHANGED export, because its "new" subset is measured against it.
qm_run_series <- function(ds, dir, pattern, file, fn) {
  stems <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  stems <- stems[grepl(pattern, stems) & file.exists(file.path(dir, stems, file))]
  # one export per date (both CONTACT-TRACING_ and CONTACT_TRACING_ occur):
  # keep the hyphenated one, as 2b-combine-exports.R does
  stems <- stems[order(grepl("^CONTACT_", stems), stems)]
  stems <- stems[!duplicated(sub(".*_", "", stems))]
  dates <- as.Date(vapply(stems, function(s) as.numeric(qm_date_of(s)), numeric(1)),
                   origin = "1970-01-01")
  o <- order(dates); stems <- stems[o]; dates <- dates[o]
  paths <- file.path(dir, stems, file)
  sigs  <- vapply(paths, qm_signature, character(1), USE.NAMES = FALSE)
  done  <- mapply(function(s, g) qm_done(ds, s, g), stems, sigs)
  known <- stems %in% srcs$source[srcs$dataset == ds]
  need  <- !done
  if (length(need) > 1)
    need[-1] <- need[-1] | (!done[-length(done)] & known[-length(known)])
  idcol <- if (ds == "epi_linelist") "alert_id" else "contact_id"
  last_k <- 0L; last_ids <- character(0)
  for (k in which(need)) {
    prev_ids <- if (k == 1) character(0) else if (last_k == k - 1) last_ids else
      unique(trimws(qm_read(paths[k - 1], idcol)[[idcol]]))
    r <- fn(paths[k], stems[k], dates[k], prev_ids)
    qm_store(ds, stems[k], sigs[k], r$rows)
    last_k <- k; last_ids <- r$ids
  }
  message("  ", ds, ": ", length(stems), " export(s), ", sum(need), " computed this run")
}

# Warnings are collected and summarised (distinct messages, with counts)
# rather than left as R's bare "There were 50 or more warnings".
qm_quietly <- function(label, expr) {
  w <- character(0)
  withCallingHandlers(expr, warning = function(cond) {
    w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning")
  })
  if (length(w)) {
    t <- sort(table(w), decreasing = TRUE)
    message("  ", label, ": ", length(w), " warning(s); distinct:")
    for (i in seq_len(min(5, length(t))))
      message("    [", t[[i]], "x] ", substr(names(t)[i], 1, 200))
  }
}
qm_quietly("epi_linelist", qm_run_series("epi_linelist", QM_EPI_DIR, "^LINELIST_[0-9]{8}$",
                                         "dhis2_processed_linelist_names.csv", qm_epi_export))
qm_quietly("contact_tracing", qm_run_series("contact_tracing", QM_CT_DIR,
                                            "^CONTACT[-_]TRACING_[0-9]{8}$",
                                            "contact_tracing_processed_names.csv", qm_ct_export))

# The combined table: one snapshot per build, never recomputed afterwards.
local({
  meta_p <- file.path(QM_CT_DIR, "combined", "combined_meta.json")
  rows_p <- file.path(QM_CT_DIR, "combined", "contact_tracing_combined_names.csv")
  cts_p  <- file.path(QM_CT_DIR, "combined", "contact_tracing_combined_contacts.csv")
  if (!all(file.exists(c(meta_p, rows_p, cts_p)))) {
    message("  contact_tracing_combined: not built yet, skipped"); return(invisible())
  }
  meta <- jsonlite::fromJSON(meta_p)
  sig <- paste(meta$built_at, qm_signature(c(rows_p, cts_p)), sep = "|")
  if (!QM_REBUILD && qm_done("contact_tracing_combined", meta$latest_export, sig)) {
    message("  contact_tracing_combined: ", meta$latest_export, " already recorded"); return(invisible())
  }
  qm_store("contact_tracing_combined", meta$latest_export, sig,
           qm_ct_combined(rows_p, cts_p, meta$latest_export, qm_date_of(meta$latest_export)))
  message("  contact_tracing_combined: recorded ", meta$latest_export)
})

if (length(new_rows)) {
  add <- do.call(rbind, new_rows)
  add[] <- lapply(add, as.character)
  hist <- rbind(hist, add[, QM_COLS])
  srcs <- rbind(srcs, do.call(rbind, new_srcs))
}
hist <- hist[order(hist$dataset, hist$export_date, hist$source, hist$subset,
                   hist$measure, hist$category), , drop = FALSE]
qm_write(hist, QM_HIST_PATH)
qm_write(srcs[order(srcs$dataset, srcs$source), , drop = FALSE], QM_SRC_PATH)


# ------------------------  2. Shares that jumped  -------------------------------
# For each dataset / subset / measure / category, compare the share with
#   - the previous export ("share_change"), and
#   - the pooled previous `baseline_exports` exports ("share_drift"),
# flagging when it moved by more than `share_jump` (absolute) AND a
# two-proportion z-test gives p < `p_value`. Small counts therefore rarely
# flag: a big swing in a handful of rows is not evidence of anything.
qm_ztest_p <- function(x1, n1, x2, n2) {
  if (n1 <= 0 || n2 <= 0) return(1)
  p <- (x1 + x2) / (n1 + n2)
  se <- sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
  if (!is.finite(se) || se == 0) return(if (x1 / n1 == x2 / n2) 1 else 0)
  2 * stats::pnorm(-abs(x1 / n1 - x2 / n2) / se)
}

# Only the subsets in metrics.alert_subsets ("all"): the "new" rows of one day
# differ from the next day's for real reasons (where the outbreak is, which
# teams reported), so flagging them buries the data problems. They are kept in
# the history for the plots.
# (Rows added this run hold NA, not "", where there is no denominator.)
h <- hist[!is.na(hist$denominator) & nzchar(hist$denominator) &
          hist$subset %in% unlist(QM$alert_subsets), , drop = FALSE]
h$n <- as.numeric(h$n); h$denominator <- as.numeric(h$denominator)
find <- list()
for (key in unique(paste(h$dataset, h$subset, h$measure, sep = "|"))) {
  g <- h[paste(h$dataset, h$subset, h$measure, sep = "|") == key, , drop = FALSE]
  exps <- unique(g[order(g$export_date, g$source), c("export_date", "source")])
  if (nrow(exps) < 2) next
  dens <- g$denominator[match(exps$source, g$source)]
  for (k in 2:nrow(exps)) {
    cur <- g[g$source == exps$source[k], , drop = FALSE]
    n_cur <- dens[k]
    prev_src <- exps$source[k - 1]
    base_src <- exps$source[max(1, k - QM$baseline_exports):(k - 1)]
    for (cat in unique(c(cur$category, g$category[g$source == prev_src]))) {
      x_cur <- sum(cur$n[cur$category == cat])
      for (cmp in c("share_change", "share_drift")) {
        if (cmp == "share_drift" && length(base_src) < 2) next
        srcs_b <- if (cmp == "share_change") prev_src else base_src
        x_b <- sum(g$n[g$source %in% srcs_b & g$category == cat])
        n_b <- sum(dens[match(srcs_b, exps$source)])
        if (n_cur <= 0 || n_b <= 0) next
        d <- x_cur / n_cur - x_b / n_b
        if (abs(d) > QM$share_jump && qm_ztest_p(x_cur, n_cur, x_b, n_b) < QM$p_value) {
          parts <- strsplit(key, "|", fixed = TRUE)[[1]]
          find[[length(find) + 1L]] <- data.frame(
            dataset = parts[1], file = exps$source[k], export_date = exps$export_date[k],
            previous_file = if (cmp == "share_change") prev_src else
              paste0(length(srcs_b), " previous exports"),
            check = cmp, severity = "warning",
            column = paste0(parts[3], ": ", cat),
            detail = sprintf("%s share %.1f%% -> %.1f%% (%+.1f points; n = %s)", parts[2],
                             100 * x_b / n_b, 100 * x_cur / n_cur, 100 * d,
                             format(n_cur, big.mark = ",")),
            contents = "", stringsAsFactors = FALSE)
        }
      }
    }
  }
}
findings_m <- if (length(find)) do.call(rbind, find) else
  data.frame(dataset = character(0), file = character(0), export_date = character(0),
             previous_file = character(0), check = character(0), severity = character(0),
             column = character(0), detail = character(0), contents = character(0),
             stringsAsFactors = FALSE)
# Known events: the hand-kept file plus those marked "expected" in the app.
ev <- do.call(rbind, lapply(c(qm_cfg$known_events, qm_cfg$known_events_app), function(f) {
  e <- qm_read(file.path(repo_root, f))
  if (is.null(e)) NULL else e[, c("dataset", "export_date", "check", "column", "note"), drop = FALSE]
}))
findings_m$status <- rep("new", nrow(findings_m)); findings_m$event_note <- rep("", nrow(findings_m))
if (!is.null(ev)) for (k in seq_len(nrow(ev))) {
  # an event on "contact_tracing" also covers the combined table
  hit <- (findings_m$dataset == ev$dataset[k] |
            findings_m$dataset == paste0(ev$dataset[k], "_combined")) &
    findings_m$export_date == ev$export_date[k] &
    (!nzchar(ev$check[k]) | findings_m$check == ev$check[k]) &
    (!nzchar(ev$column[k]) | findings_m$column == ev$column[k])
  findings_m$status[hit] <- "expected"; findings_m$event_note[hit] <- ev$note[k]
}
qm_write(findings_m, QM_FIND_PATH)
for (ds in unique(hist$dataset)) {
  latest <- max(hist$export_date[hist$dataset == ds])
  lf <- findings_m[findings_m$dataset == ds & findings_m$export_date == latest &
                   findings_m$status == "new", , drop = FALSE]
  message("  ", ds, ": shares, latest (", latest, "): ",
          if (!nrow(lf)) "no jumps" else paste0(nrow(lf), " jump(s)"))
  for (i in seq_len(nrow(lf)))
    message("    [", lf$check[i], "] ", lf$column[i], " -- ", lf$detail[i])
}

# Charts (plot-metrics.R). Never fatal.
tryCatch(source(file.path(repo_root, "pipelines", "quality_checks", "plot-metrics.R"),
                local = FALSE),
         error = function(e) message("  metrics charts not drawn: ", conditionMessage(e)))

rm(list = intersect(ls(), c("hist", "srcs", "new_rows", "new_srcs", "h", "find",
                            "findings_m", "ev", "g", "exps", "cur", "add")))
