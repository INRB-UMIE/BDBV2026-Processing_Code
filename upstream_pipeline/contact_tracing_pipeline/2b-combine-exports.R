#!/usr/bin/env Rscript
# 2b-combine-exports.R
#
# Combine every processed contact tracing export into ONE all-time dataset.
#
# WHY. From CONTACT-TRACING_27082026 onwards the DHIS2 export only holds about
# four weeks of contacts, by registration date. Up to 26 Aug each export held
# everything since 2 May. The windows overlap -- each starts before the
# previous export ends -- so the union of all exports is the full history, but
# no single export is. Anything that reads only the latest export (analytics,
# dedup, person matching) silently loses every contact older than the window.
#
# Run after 2-assign-contact-ids.R, so the rows carry their CT- / CI- / CX- IDs.
#
# Outputs, in data/processed/contact_tracing_processed/combined/:
#   contact_tracing_combined_names.csv     all rows, with names   (PII)
#   contact_tracing_combined_ctsafe.csv    same, ctsafe allowlist
#   contact_tracing_combined_contacts.csv  one row per contact_id  (no names)
#   combined_meta.json   (the log goes to logs/<run>/ct_combine_log.txt)
#
# ---------------------------------------------------------------------------
# THE UNIT OF REPLACEMENT IS THE CONTACT, NOT THE ROW
# ---------------------------------------------------------------------------
# The export has no DHIS2 event UID, so there is no row key to match a row in
# one export to the same row in the next. The obvious candidates do not work.
# Measured on CONTACT-TRACING_23092026 (12,050 rows):
#
#   contact_id + followup_date                          331 duplicate keys
#   contact_id + followup_date + source_case_alert_id   331 -- no better
#
# Adding source_case_alert_id changes nothing because a contact_id already
# encodes one person x one source case: across all 58 exports no contact_id
# ever carries two source cases within an export. Of the 331, 212 are exact
# duplicate rows and the rest are two follow-ups of one contact on the same
# day, so no field makes them unique.
#
# So each contact is taken WHOLE from the most recent export that contains it:
# every one of its follow-up rows, as that export has them. That needs no row
# key, it picks up later follow-up results automatically, and it is safe
# because the window is cut on registration date, which is per contact -- a
# contact is either entirely inside an export or entirely outside it.
#
# Keying on contact_id alone, not contact_id + source_case_alert_id, is
# deliberate: 119 contacts' source_case_alert_id changes between exports, almost
# all a source ID repaired or assigned (blank -> CI-000001, REC-AAA-... ->
# RDC-AAA-...) by pipeline rules newer than the older export's processing.
# Keying on the pair would split each of those into two contacts. The newest
# export's source ID wins; source_case_changed flags the contact.
#
# ---------------------------------------------------------------------------
# EVERY CONTACT IS KEPT, WITH A STATUS
# ---------------------------------------------------------------------------
#   current              in the latest export
#   aged_out             not in the latest export, registered before its
#                        window starts -- dropped off the rolling window, as
#                        expected
#   missing_from_latest  not in the latest export although its registration
#                        date is inside the window (or blank) -- deleted in
#                        DHIS2, or its contact_id changed
#
# ---------------------------------------------------------------------------
# WHAT THIS CANNOT RECOVER
# ---------------------------------------------------------------------------
# * Edits made in DHIS2 after a contact left the window (a late follow-up
#   result). Only a full export could supply those.
# * Rows with a blank contact_id in an export 2-assign-contact-ids.R never ran
#   on. Nothing identifies them across exports, so they are left out and
#   counted in the log. Reprocessing those exports (CT Cleanup tab, select
#   them) gives them CT- IDs and brings them in.
# * Cleaning differences. Each export is combined as it was processed. Older
#   folders were cleaned under older rules -- source IDs like "25" come from
#   there -- so reprocess them for consistency. The log lists exports without
#   the current step 2 columns.
#
# Exact duplicate rows (every column identical) are removed; the count per
# contact is in n_duplicate_rows_removed.
#
# Rebuilt in full when any input changed (compared by file size and mtime);
# otherwise skipped.
#
# Identification only: no export is modified.

# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

ctc_processed_dir <- file.path(repo_root, "data", "processed",
                               "contact_tracing_processed")
ctc_out_dir <- file.path(ctc_processed_dir, "combined")

# The columns this step adds to every row.
CTC_ADDED_COLUMNS <- c("ct_export", "contact_first_export", "contact_status")

# -------------------------  Helpers  ------------------------------------------

.ctc_log <- character(0)
ctclog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .ctc_log <<- c(.ctc_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

ctc_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

ctc_stem_date <- function(stem) {
  m <- regmatches(stem, regexpr("[0-9]{8}$", stem))
  if (!length(m) || !nzchar(m)) return(as.Date(NA))
  tryCatch(as.Date(m, format = "%d%m%Y"), error = function(e) as.Date(NA))
}

ctc_header <- function(path) {
  tryCatch(names(utils::read.csv(path, nrows = 0, check.names = FALSE)),
           error = function(e) NULL)
}

# Only the named columns, as character. colClasses "NULL" skips converting the
# rest, which is most of the cost of read.csv. Missing columns come back blank.
ctc_read_cols <- function(path, cols) {
  hdr <- ctc_header(path)
  if (is.null(hdr)) return(NULL)
  cc <- ifelse(hdr %in% cols, "character", "NULL")
  df <- utils::read.csv(path, colClasses = cc, na.strings = "",
                        check.names = FALSE)
  for (cl in setdiff(cols, names(df))) df[[cl]] <- rep(NA_character_, nrow(df))
  df[, cols, drop = FALSE]
}

ctc_blank <- function(x) is.na(x) | !nzchar(trimws(x))

# rbind over differing column sets: the union, in first-seen order.
ctc_bind <- function(parts) {
  cols <- unique(unlist(lapply(parts, names), use.names = FALSE))
  do.call(rbind, lapply(parts, function(p) {
    for (cl in setdiff(cols, names(p))) p[[cl]] <- rep(NA_character_, nrow(p))
    p[, cols, drop = FALSE]
  }))
}


# -------------------------  Find the exports  ---------------------------------

ctclog("=== Combining contact tracing exports ===")

ctc_stems <- list.dirs(ctc_processed_dir, full.names = FALSE, recursive = FALSE)
# CONTACT-TRACING_ and CONTACT_TRACING_ both occur in the raw file names. A
# misspelt folder (CONTCT-TRACING_17062026) does not match and is ignored.
ctc_stems <- ctc_stems[grepl("^CONTACT[-_]TRACING_[0-9]{8}$", ctc_stems)]
ctc_stems <- ctc_stems[file.exists(
  file.path(ctc_processed_dir, ctc_stems, "contact_tracing_processed_names.csv"))]

# One export per date. Where both spellings exist (09072026) they hold the same
# export; keep the hyphenated one.
ctc_stems <- ctc_stems[order(grepl("^CONTACT_", ctc_stems), ctc_stems)]
ctc_dupe  <- duplicated(sub(".*_", "", ctc_stems))
if (any(ctc_dupe)) {
  ctclog("  Same date twice, ignoring: ", paste(ctc_stems[ctc_dupe], collapse = ", "))
}
ctc_stems <- ctc_stems[!ctc_dupe]

ctc_dates <- as.Date(vapply(ctc_stems, function(s) as.numeric(ctc_stem_date(s)),
                            numeric(1)), origin = "1970-01-01")
ctc_stems <- ctc_stems[!is.na(ctc_dates)]
ctc_dates <- ctc_dates[!is.na(ctc_dates)]
ctc_ord   <- order(ctc_dates)                      # oldest first
ctc_stems <- ctc_stems[ctc_ord]
ctc_dates <- ctc_dates[ctc_ord]
if (!length(ctc_stems)) stop("No processed contact tracing exports found in ",
                             ctc_processed_dir, call. = FALSE)
ctc_latest <- ctc_stems[length(ctc_stems)]

# The file each export is read from: the full table, names included, because
# later matching needs names. (Every processed export has one; there is no
# longer a no-names copy to fall back to.)
ctc_src <- file.path(ctc_processed_dir, ctc_stems,
                     "contact_tracing_processed_names.csv")

ctclog("  ", length(ctc_stems), " export(s): ", ctc_stems[1], " to ", ctc_latest)


# -------------------------  Skip if nothing changed  --------------------------

ctc_meta_p <- file.path(ctc_out_dir, "combined_meta.json")
ctc_signature <- paste(ctc_stems, file.size(ctc_src),
                       format(file.mtime(ctc_src), "%Y-%m-%dT%H:%M:%S"),
                       sep = ":", collapse = ";")
ctc_prev_sig <- tryCatch(jsonlite::fromJSON(ctc_meta_p)$signature,
                         error = function(e) NULL)
ctc_outputs_exist <- all(file.exists(file.path(ctc_out_dir, c(
  "contact_tracing_combined_names.csv", "contact_tracing_combined_contacts.csv"))))
ctc_force <- exists("ct_combine_force") && isTRUE(ct_combine_force)

if (!ctc_force && ctc_outputs_exist && identical(ctc_prev_sig, ctc_signature)) {
  ctclog("  No export changed since the last build; combined dataset is current.")
} else {

# -------------------------  Pass 1: which export is each contact's latest -----
# Reads contact_id, source_case_alert_id and registration_date only.
ctc_seen   <- vector("list", length(ctc_stems))
ctc_checks <- data.frame(export = ctc_stems, rows = NA_integer_,
                         blank_contact_id = NA_integer_,
                         has_step2 = NA, max_registration = NA_character_,
                         stringsAsFactors = FALSE)

for (i in seq_along(ctc_stems)) {
  d <- ctc_read_cols(ctc_src[i], c("contact_id", "source_case_alert_id",
                                   "registration_date"))
  if (is.null(d)) { ctclog("  ", ctc_stems[i], ": unreadable, skipped."); next }
  d$contact_id <- trimws(d$contact_id)
  blank <- ctc_blank(d$contact_id)
  ctc_checks$rows[i] <- nrow(d)
  ctc_checks$blank_contact_id[i] <- sum(blank)
  ctc_checks$has_step2[i] <- "contact_id_assigned" %in% ctc_header(ctc_src[i])
  reg <- d$registration_date[!ctc_blank(d$registration_date)]
  ctc_checks$max_registration[i] <- if (length(reg)) max(reg) else NA_character_
  d <- d[!blank, , drop = FALSE]
  ctc_seen[[i]] <- unique(data.frame(contact_id = d$contact_id,
                                     source = ifelse(ctc_blank(d$source_case_alert_id),
                                                     "", d$source_case_alert_id),
                                     export = ctc_stems[i], ord = i,
                                     stringsAsFactors = FALSE))
}
ctc_seen <- do.call(rbind, ctc_seen[!vapply(ctc_seen, is.null, logical(1))])

# A folder dated earlier than the contacts it holds is mislabelled, and would
# be ranked as older than it is. Harmless when a correctly dated copy exists
# (the later one wins), but worth knowing.
ctc_mislabel <- !is.na(ctc_checks$max_registration) &
  ctc_checks$max_registration > format(ctc_dates, "%Y-%m-%d")
if (any(ctc_mislabel)) {
  ctclog("  WARNING: export(s) holding registrations after their own date -- ",
         "check the file name: ",
         paste0(ctc_stems[ctc_mislabel], " (to ",
                ctc_checks$max_registration[ctc_mislabel], ")", collapse = ", "))
}

ctc_legacy <- ctc_checks$blank_contact_id > 0 & !ctc_checks$has_step2
ctc_legacy[is.na(ctc_legacy)] <- FALSE
if (any(ctc_legacy)) {
  ctclog("  ", sum(ctc_checks$blank_contact_id[ctc_legacy]), " row(s) with a ",
         "blank contact_id in ", sum(ctc_legacy), " export(s) processed before ",
         "2-assign-contact-ids.R -- left out. Reprocess these exports to bring ",
         "them in: ", paste(ctc_stems[ctc_legacy], collapse = ", "))
}

# Per contact: first and last export, how many exports, source-case history.
ctc_seen <- ctc_seen[order(ctc_seen$contact_id, ctc_seen$ord), , drop = FALSE]
ctc_by   <- split(seq_len(nrow(ctc_seen)), ctc_seen$contact_id)
ctc_contacts <- data.frame(
  contact_id   = names(ctc_by),
  first_export = vapply(ctc_by, function(ix) ctc_seen$export[ix[1]], character(1)),
  last_export  = vapply(ctc_by, function(ix) ctc_seen$export[ix[length(ix)]], character(1)),
  n_exports    = vapply(ctc_by, function(ix) length(unique(ctc_seen$ord[ix])), integer(1)),
  source_case_history = vapply(ctc_by, function(ix) {
    s <- unique(ctc_seen$source[ix]); paste(s[nzchar(s)], collapse = " | ")
  }, character(1)),
  source_case_changed = vapply(ctc_by, function(ix)
    length(unique(ctc_seen$source[ix])) > 1, logical(1)),
  stringsAsFactors = FALSE, row.names = NULL)


# -------------------------  Pass 2: take each contact from its latest export --
ctc_parts <- list()
ctc_dup_ids <- character(0)   # one entry per exact duplicate row removed
for (i in seq_along(ctc_stems)) {
  ids <- ctc_contacts$contact_id[ctc_contacts$last_export == ctc_stems[i]]
  if (!length(ids)) next
  d <- utils::read.csv(ctc_src[i], colClasses = "character", na.strings = "",
                       check.names = FALSE)
  d <- d[trimws(d$contact_id) %in% ids, , drop = FALSE]
  # EXACT DUPLICATE ROWS are dropped: every column identical, names included,
  # so they carry nothing a copy does not. 1,901 of 87,819 rows across 930
  # contacts when measured. Checked within one export, before this step adds
  # its own columns. Two follow-ups of a contact on the same day that differ
  # in any field are NOT duplicates and are kept.
  dup <- duplicated(d)
  if (any(dup)) {
    ctc_dup_ids <- c(ctc_dup_ids, trimws(d$contact_id[dup]))
    d <- d[!dup, , drop = FALSE]
  }
  # Exports processed before a column was renamed still carry the old name.
  # Bring them onto the current one (data/maps/processed_column_aliases.csv),
  # so the combined file has one column rather than two half-empty ones.
  d <- apply_column_aliases(d, "contact_tracing")
  d$ct_export <- ctc_stems[i]
  ctc_parts[[length(ctc_parts) + 1L]] <- d
  ctclog("  ", ctc_stems[i], ": ", length(ids), " contact(s), ", nrow(d), " row(s)")
}
ctc_rows <- ctc_bind(ctc_parts)
ctc_rows$contact_id <- trimws(ctc_rows$contact_id)


# -------------------------  Status  -------------------------------------------
# The latest export's window starts at its earliest registration date.
ctc_latest_reg <- ctc_rows$registration_date[ctc_rows$ct_export == ctc_latest]
ctc_latest_reg <- ctc_latest_reg[!ctc_blank(ctc_latest_reg)]
ctc_window_start <- if (length(ctc_latest_reg)) min(ctc_latest_reg) else NA_character_

ctc_reg <- tapply(ctc_rows$registration_date, ctc_rows$contact_id, function(x) {
  x <- x[!ctc_blank(x)]; if (length(x)) min(x) else NA_character_
})
ctc_contacts$registration_date <- unname(ctc_reg[ctc_contacts$contact_id])
ctc_contacts$source_case_alert_id <- unname(tapply(
  ctc_rows$source_case_alert_id, ctc_rows$contact_id,
  function(x) { x <- unique(x[!ctc_blank(x)]); paste(x, collapse = " | ") }
)[ctc_contacts$contact_id])
ctc_contacts$n_rows <- as.integer(table(ctc_rows$contact_id)[ctc_contacts$contact_id])
ctc_dup_n <- table(ctc_dup_ids)
ctc_contacts$n_duplicate_rows_removed <- as.integer(ifelse(
  ctc_contacts$contact_id %in% names(ctc_dup_n),
  ctc_dup_n[ctc_contacts$contact_id], 0L))

ctc_contacts$contact_status <- ifelse(
  ctc_contacts$last_export == ctc_latest, "current",
  ifelse(!is.na(ctc_contacts$registration_date) & !is.na(ctc_window_start) &
           ctc_contacts$registration_date < ctc_window_start,
         "aged_out", "missing_from_latest"))

m <- match(ctc_rows$contact_id, ctc_contacts$contact_id)
ctc_rows$contact_first_export <- ctc_contacts$first_export[m]
ctc_rows$contact_status       <- ctc_contacts$contact_status[m]

ctc_contacts <- ctc_contacts[, c("contact_id", "source_case_alert_id",
                                 "registration_date", "contact_status",
                                 "first_export", "last_export", "n_exports",
                                 "n_rows", "n_duplicate_rows_removed",
                                 "source_case_changed",
                                 "source_case_history"), drop = FALSE]


# -------------------------  Write  --------------------------------------------
ctc_rows <- ctc_rows[, c(setdiff(names(ctc_rows), CTC_ADDED_COLUMNS),
                         CTC_ADDED_COLUMNS), drop = FALSE]

ctc_write_csv(ctc_rows, file.path(ctc_out_dir, "contact_tracing_combined_names.csv"))

# The no-names copy (contact_tracing_combined.csv) is no longer written; remove
# one left from an earlier build so it is not mistaken for current.
ctc_legacy_plain <- file.path(ctc_out_dir, "contact_tracing_combined.csv")
if (file.exists(ctc_legacy_plain)) {
  unlink(ctc_legacy_plain)
  ctclog("  Removed old no-names copy: ", basename(ctc_legacy_plain))
}

# ctsafe: the latest export's own allowlist, read back from its file so it
# cannot drift from what steps 1 and 2 publish, plus this step's columns.
ctc_safe_p <- file.path(ctc_processed_dir, ctc_latest,
                        "contact_tracing_processed_ctsafe.csv")
ctc_safe_cols <- ctc_header(ctc_safe_p)
if (!is.null(ctc_safe_cols)) {
  ctc_safe_cols <- unique(c(intersect(ctc_safe_cols, names(ctc_rows)),
                            CTC_ADDED_COLUMNS))
  ctc_write_csv(ctc_rows[, ctc_safe_cols, drop = FALSE],
                file.path(ctc_out_dir, "contact_tracing_combined_ctsafe.csv"))
} else {
  ctclog("  WARNING: no ctsafe file in ", ctc_latest,
         " -- combined ctsafe export not written.")
}

ctc_write_csv(ctc_contacts,
              file.path(ctc_out_dir, "contact_tracing_combined_contacts.csv"))


# -------------------------  Report  -------------------------------------------
ctc_status_n <- table(factor(ctc_contacts$contact_status,
                             levels = c("current", "aged_out", "missing_from_latest")))
ctclog("\n  Combined: ", nrow(ctc_rows), " row(s), ", nrow(ctc_contacts),
       " contact(s), ", length(unique(ctc_rows$source_case_alert_id[
         !ctc_blank(ctc_rows$source_case_alert_id)])), " source case(s)")
ctclog("  Latest export ", ctc_latest, ", window from ", ctc_window_start)
for (k in names(ctc_status_n)) ctclog("    ", format(k, width = 20), ctc_status_n[[k]])
ctclog("  Exact duplicate rows removed: ", length(ctc_dup_ids), " (",
       length(unique(ctc_dup_ids)), " contact(s))")
ctclog("  Source case changed between exports: ",
       sum(ctc_contacts$source_case_changed), " contact(s)")
ctclog("  NOTE: contact_tracing_combined_names.csv contains patient names. ",
       "Handle it like contact_tracing_processed_names.csv.")

writeLines(jsonlite::toJSON(list(
  signature = ctc_signature,
  exports = ctc_stems,
  latest_export = ctc_latest,
  window_start = ctc_window_start,
  n_rows = nrow(ctc_rows),
  n_contacts = nrow(ctc_contacts),
  n_duplicate_rows_removed = length(ctc_dup_ids),
  contact_status = as.list(ctc_status_n),
  built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
), auto_unbox = TRUE, pretty = TRUE), ctc_meta_p)

# Only on a rebuild, so a skipped run does not overwrite the log that
# describes the files actually on disk.
write_log_file(.ctc_log, "ct_combine_log.txt")

}  # end rebuild

ct_combine_complete <- TRUE


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, "ct_combine_complete")
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
