#!/usr/bin/env Rscript
# 2-assign-contact-ids.R
#
# Assign every ID this repo issues on the contact tracing side, in dependency
# order:
#
#   CT-  to rows the export left without a `contact_id` (from 2026-10-30,
#        only where there is no `contact_uid` to use instead -- see the
#        "Fill from contact_uid" block below)
#   CI-  to source cases that have a name but no alert_id
#   CX-  to source cases with no name either, keyed on the contact's exposure
#
# Run after pipelines/contact_tracing_pipeline/1-colname-standard.R, which owns
# the structural repair of `source_case_alert_id` and the corroborated fill from
# the linelist. This script only assigns IDs to what those could not resolve.
#
# THE ORDER IS THE POINT. CX- keys on `contact_id`, which CT- fills, so CI- and
# CX- used to sit in step 1 where a row blank on both source and contact ID had
# nothing to key on and was left unresolved. Running them here, after CT-,
# removes that dead end. Verified safe on the real data: the blank-contact_id
# and blank-source sets are disjoint (0 rows blank on both), and running CT-
# before the source is resolved causes 0 fingerprint collisions — 13 distinct
# CT- fingerprints either way.
#
# Rewrites, in the CT run folder:
#   contact_tracing_processed_names.csv    IDs filled + the companion columns
#   contact_tracing_processed_ctsafe.csv   same, restricted to CT_SAFE_COLUMNS
# and maintains, in the CT processed root:
#   assigned_contact_ids.csv   CT- (contains patient names)
#   assigned_case_ids.csv      CI- (contains patient names)
#   assigned_exposure_ids.csv  CX- (contact IDs and a date only)
#
# (This file replaces the unused 2-confirm_linelist_contact_tracing_cohesion.R.)
#
#
# ---------------------------------------------------------------------------
# WHY THE FINGERPRINT LOOKS LIKE THIS
# ---------------------------------------------------------------------------
# Measured on CONTACT-TRACING_01092026 and the 42 exports before it.
#
# 1. A `contact_id` identifies a PERSON x SOURCE CASE, not a person. `lokuli
#    ambe roger` holds two contact IDs — C-26-S00-CONT-00001234 under source
#    RDC-AAA-BBB-26-0001238-MVE and C-26-S00-CONT-00001235 under
#    RDC-AAA-BBB-26-0001240-MVE — because he was a contact of two different
#    cases. So `source_case_alert_id` belongs in the key: leaving it out would
#    merge two legitimately distinct contact records into one ID.
#
#    The cost is a coupling: repairing a source case's alert ID (which
#    1-colname-standard.R now does) changes the fingerprint of every contact
#    of that case. Deterministic once introduced, but it means the registry
#    must be rebuilt if the repair rules ever change — see the schema guard in
#    load_assigned_contact_id_registry().
#
# 2. CT is one row per FOLLOW-UP, not one row per person: 2,526 distinct
#    contact IDs across 20,632 rows, a median of 7 rows each and up to 26. So
#    the fingerprint must not include anything that varies between follow-ups
#    of the same contact.
#
#    Tracking every blank-contact_id row across all 43 exports gives 7,967
#    comparisons of the same follow-up row in consecutive files. These changed
#    zero times:
#
#      contact_name, contact_sex, contact_age, source_case_alert_id,
#      registration_date
#
#    Excluded because they do move: relationship_to_case, contact_type and
#    case_circumstances (556 changes each — they move together), health_zone
#    (268) and followup_result (197). health_area and province were also stable
#    but add nothing once the source case is in the key.
#
# 3. It discriminates. The 263 blank rows reduce to 13 distinct fingerprints,
#    none of which collides with a row that already has a contact_id.
#
# 4. No supersession machinery. Unlike the linelist — where 9 rows started
#    blank and were later given a real alert ID — ZERO blank contact IDs have
#    ever been filled in by a later export. The registry keeps the columns for
#    symmetry and in case that changes, but they are expected to stay empty.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("contact_tracing_cleaning_complete")) {
  source("pipelines/contact_tracing_pipeline/1-colname-standard.R", local = FALSE)
} else if (!contact_tracing_cleaning_complete) {
  source("pipelines/contact_tracing_pipeline/1-colname-standard.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

# CT- to mirror the epi pipeline's LL-. Deliberately unlike a DHIS2 contact ID
# (C-26-S00-CONT-00001236) so the two can never be confused.
ASSIGNED_CONTACT_PREFIX <- "CT-"
ASSIGNED_CONTACT_RE     <- "^CT-[0-9]{6}$"

# The fingerprint fields, in order. FROZEN once IDs are in circulation:
# changing this list invalidates every ID in the registry.
CONTACT_FINGERPRINT_FIELDS <- c("contact_name", "contact_sex", "contact_age",
                                "source_case_alert_id", "registration_date")

ASSIGNED_CONTACT_REGISTRY_COLUMNS <- c(
  "assigned_id",             # CT-nnnnnn, sequential, never reissued
  "fingerprint",             # the matching key (contains the contact's name)
  "first_assigned_run",      # export in which this ID was first handed out
  "num_ct_rows",             # rows carrying this assigned ID in counts_from_run
  "counts_from_run",         # which export num_ct_rows describes
  "superseded_by_dhis2_id",  # real contact_id, if DHIS2 ever supplies one
  "superseded_in_run",
  # RETIRED IDs. The CT- ID this one was replaced by, when it was issued in
  # error. Set by hand (or a one-off script), never by this step. 2026-10:
  # the 21 Oct export renamed "Date d'enregistrement", registration_date went
  # blank, and 174 rows got a second CT- ID; see
  # contact_tracing_pipeline/maintenance/check_duplicate_ct_ids_2026_10.py.
  # A retired ID is never handed out again (a fingerprint that matches it gets
  # its replacement), and 4-build-person-registry.R puts it in the same person
  # as its replacement, so history that cites it still resolves.
  "replaced_by"
)


# -------------------------  Helper Functions  ---------------------------------

ct_assign_blank_or_na <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# Deterministic accent folding. NOT iconv(to = "ASCII//TRANSLIT"), whose
# behaviour comes from the platform C library: glibc renders "Gisèle" as
# "Gisele" while macOS commonly yields "Gis?le". A fingerprint cannot tolerate
# that — the same contact would be issued a different ID on a different
# machine, which is exactly what this mechanism exists to prevent.
.CT_ASSIGN_FROM <- paste0(
  "àáâãäåèéêë", "ìíîïòóôõöù", "úûüýÿçñðþ",
  "ÀÁÂÃÄÅÈÉÊË", "ÌÍÎÏÒÓÔÕÖÙ", "ÚÛÜÝÇÑÐÞ"
)
.CT_ASSIGN_TO <- paste0(
  "aaaaaaeeee", "iiiiooooou", "uuuyycndt",
  "AAAAAAEEEE", "IIIIOOOOOU", "UUUYCNDT"
)

ct_assign_translit <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("ß", "ss", x, fixed = TRUE)
  x <- gsub("æ", "ae", x, fixed = TRUE)
  x <- gsub("Æ", "AE", x, fixed = TRUE)
  x <- gsub("œ", "oe", x, fixed = TRUE)
  x <- gsub("Œ", "OE", x, fixed = TRUE)
  x <- chartr(.CT_ASSIGN_FROM, .CT_ASSIGN_TO, x)
  gsub("[^ -~]", "", x)
}

# Fingerprint of one row. Order-insensitive on the name, since name order
# varies between exports.
contact_fingerprint <- function(df) {
  get <- function(nm) {
    if (nm %in% names(df)) as.character(df[[nm]]) else rep("", nrow(df))
  }
  nm <- ct_assign_translit(tolower(trimws(get("contact_name"))))
  nm <- gsub("[^a-z0-9 ]", " ", nm)
  nm <- vapply(strsplit(trimws(nm), "[[:space:]]+"), function(tk) {
    tk <- tk[nzchar(tk)]
    if (!length(tk)) return("")
    paste0(sort(tk), collapse = " ")
  }, character(1))

  tidy <- function(x) {
    out <- tolower(trimws(as.character(x)))
    out[is.na(out)] <- ""
    out
  }
  paste(nm, tidy(get("contact_sex")), tidy(get("contact_age")),
        tidy(get("source_case_alert_id")), tidy(get("registration_date")),
        sep = "|")
}

assigned_contact_registry_path <- function(processed_dir) {
  file.path(processed_dir, "assigned_contact_ids.csv")
}

# LINELIST/CONTACT-TRACING_DDMMYYYY -> "YYYYMMDD", so run stems compare
# chronologically as plain strings. Needed because file discovery is
# alphabetical, not chronological.
ct_assign_run_sortkey <- function(stem) {
  d <- regmatches(stem, regexpr("[0-9]{8}$", stem))
  if (!length(d) || !nzchar(d)) return("")
  paste0(substr(d, 5, 8), substr(d, 3, 4), substr(d, 1, 2))
}

load_assigned_contact_id_registry <- function(processed_dir) {
  path <- assigned_contact_registry_path(processed_dir)
  cols <- ASSIGNED_CONTACT_REGISTRY_COLUMNS
  if (!file.exists(path)) {
    return(as.data.frame(
      setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    ))
  }
  reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character")

  # Refuse a registry whose keys cannot match the current ones. Loading such a
  # file would quietly issue every contact a second ID rather than reusing the
  # one it already has.
  wrong_prefix <- "assigned_id" %in% names(reg) && nrow(reg) > 0 &&
    !all(grepl(ASSIGNED_CONTACT_RE, reg$assigned_id))
  missing_cols <- setdiff(c("assigned_id", "fingerprint"), names(reg))
  if (wrong_prefix || length(missing_cols) > 0) {
    stop("Assigned contact-ID registry uses a superseded schema.\n",
         if (length(missing_cols) > 0)
           paste0("  Missing column(s): ", paste(missing_cols, collapse = ", "),
                  ".\n") else "",
         if (wrong_prefix)
           paste0("  Its IDs do not match ", ASSIGNED_CONTACT_RE, " — e.g. ",
                  reg$assigned_id[1], ".\n") else "",
         "  Delete it and let this run rebuild it:\n    ", path,
         "\n  (Safe while no assigned contact ID has been used downstream. ",
         "Once they have been, neither CONTACT_FINGERPRINT_FIELDS nor the ",
         "CT- prefix may change.)",
         call. = FALSE)
  }

  for (cl in setdiff(cols, names(reg))) reg[[cl]] <- NA_character_
  reg[, cols, drop = FALSE]
}

save_assigned_contact_id_registry <- function(reg, processed_dir) {
  ord <- order(suppressWarnings(
    as.numeric(sub(ASSIGNED_CONTACT_PREFIX, "", reg$assigned_id))
  ))
  ct_write_csv(reg[ord, , drop = FALSE],
               assigned_contact_registry_path(processed_dir))
}

ct_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

# Assign IDs to rows whose contact_id is blank. Never touches a row that
# already has one.
# Rewrite the _new/_reused half of a row label to mean "this export introduced
# the ID", not "this pass of the script minted it".
#
# The distinction matters because step 2 is idempotent in its IDs but was not in
# its labels. Running it twice in one session left the IDs and the registries
# correct while relabelling all 263 assigned contact rows `_reused`, because the
# second pass found every fingerprint already registered. The row-level column
# therefore described the last pass rather than the data, and could not answer
# "which IDs did this export introduce" — only the registry could.
#
# `first_assigned_run` is the durable answer, so the label is derived from it.
# An ID absent from the registry keeps whatever label it came with, rather than
# silently reading as `_reused`: that should not happen, and quietly relabelling
# it would hide the fact that it did.
ct_relabel_by_first_run <- function(label, ids, reg, run_stem) {
  if (!length(label)) return(label)
  touched <- !is.na(label) & grepl("_(new|reused)$", label)
  if (!any(touched)) return(label)

  # Single bracket: `first` is an atomic named vector, and a miss must give NA
  # rather than the "subscript out of bounds" that [[ ]] raises.
  first <- stats::setNames(reg$first_assigned_run, reg$assigned_id)
  fr    <- unname(first[ids[touched]])

  relabelled <- paste0(sub("_(new|reused)$", "", label[touched]),
                       ifelse(!is.na(fr) & fr == run_stem, "_new", "_reused"))
  relabelled[is.na(fr)] <- label[touched][is.na(fr)]
  label[touched] <- relabelled
  label
}

assign_missing_contact_ids <- function(contact_id, fp, contact_name, reg,
                                       run_stem) {
  n <- length(contact_id)
  out <- as.character(contact_id)
  label <- rep(NA_character_, n)

  blank <- ct_assign_blank_or_na(out)
  if (!any(blank)) {
    return(list(value = out, registry = reg, label = label,
                n_new = 0L, n_reused = 0L, n_unassignable = 0L))
  }

  # Without a name there is nothing to recognise the row by next export.
  nameless <- blank & ct_assign_blank_or_na(contact_name)
  usable   <- blank & !nameless

  known <- stats::setNames(reg$assigned_id, reg$fingerprint)
  # A fingerprint that matches a retired ID gets its replacement (followed
  # through a chain of replacements, up to a sane depth).
  rb <- stats::setNames(reg$replaced_by, reg$assigned_id)
  rb <- rb[!is.na(rb) & nzchar(rb)]
  if (length(rb)) {
    for (step in 1:10) {
      nxt <- unname(rb[known])
      move <- !is.na(nxt)
      if (!any(move)) break
      known[move] <- nxt[move]
    }
  }
  next_num <- if (nrow(reg)) {
    max(suppressWarnings(
      as.numeric(sub(ASSIGNED_CONTACT_PREFIX, "", reg$assigned_id))
    ), na.rm = TRUE) + 1L
  } else {
    1L
  }
  if (!is.finite(next_num)) next_num <- 1L

  n_new <- 0L; n_reused <- 0L
  new_rows <- list()

  for (i in which(usable)) {
    # Single bracket: `known` is an atomic named vector, and
    # known[["a name that isn't there"]] raises "subscript out of bounds"
    # rather than returning NA — and a miss is the common case on a first run.
    hit <- unname(known[fp[i]])
    if (!is.na(hit)) {
      out[i] <- hit
      label[i] <- "assigned_contact_id_reused"
      n_reused <- n_reused + 1L
      next
    }
    new_id <- sprintf("%s%06d", ASSIGNED_CONTACT_PREFIX, next_num)
    next_num <- next_num + 1L
    known[fp[i]] <- new_id
    out[i] <- new_id
    label[i] <- "assigned_contact_id_new"
    n_new <- n_new + 1L
    new_rows[[length(new_rows) + 1L]] <- data.frame(
      assigned_id = new_id, fingerprint = fp[i], first_assigned_run = run_stem,
      num_ct_rows = NA_character_, counts_from_run = NA_character_,
      superseded_by_dhis2_id = NA_character_, superseded_in_run = NA_character_,
      replaced_by = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  label[nameless] <- "no_contact_id_and_no_name"

  if (length(new_rows)) reg <- rbind(reg, do.call(rbind, new_rows))
  list(value = out, registry = reg, label = label, n_new = n_new,
       n_reused = n_reused, n_unassignable = sum(nameless))
}

# How many rows of this export carry each assigned ID. Only overwrites when
# this export is at least as recent as whatever the entry already describes,
# so reprocessing an old file cannot roll the figures backwards.
update_assigned_contact_row_counts <- function(reg, final_ids, run_stem) {
  if (!nrow(reg)) return(reg)
  cnt_tab <- table(final_ids[grepl(ASSIGNED_CONTACT_RE, final_ids)])
  counts <- stats::setNames(as.integer(cnt_tab), names(cnt_tab))

  this_key <- ct_assign_run_sortkey(run_stem)
  stored_key <- vapply(reg$counts_from_run, function(s) {
    if (is.na(s) || !nzchar(s)) "" else ct_assign_run_sortkey(s)
  }, character(1), USE.NAMES = FALSE)

  for (k in seq_len(nrow(reg))) {
    if (nzchar(stored_key[k]) && this_key < stored_key[k]) next
    n <- unname(counts[reg$assigned_id[k]])
    reg$num_ct_rows[k] <- as.character(if (is.na(n)) 0L else n)
    reg$counts_from_run[k] <- run_stem
  }
  reg
}

# Note fingerprints that now carry a real DHIS2 contact_id. Recorded in the
# registry only, so the same fingerprint is never issued a second assigned ID.
record_superseded_contact_ids <- function(reg, fp, contact_ids, run_stem) {
  if (!nrow(reg)) return(reg)
  idx <- match(reg$fingerprint, fp)
  hit <- !is.na(idx) & is.na(reg$superseded_by_dhis2_id)
  if (!any(hit)) return(reg)
  real <- contact_ids[idx[hit]]
  ok <- !ct_assign_blank_or_na(real) & !grepl(ASSIGNED_CONTACT_RE, real)
  if (!any(ok)) return(reg)
  target <- which(hit)[ok]
  reg$superseded_by_dhis2_id[target] <- real[ok]
  reg$superseded_in_run[target] <- run_stem
  for (k in target) {
    message("  assigned contact ID superseded: ", reg$assigned_id[k], " -> ",
            reg$superseded_by_dhis2_id[k])
  }
  reg
}


# ============================================================================
# MOVED FROM 1-colname-standard.R
# ============================================================================
# The CI- and CX- assignment used to live in step 1, alongside the
# source_case_alert_id repair. It moved here because CX- keys on contact_id,
# which THIS script fills — so in step 1 a row blank on both source and contact
# ID had nothing to key on and was left unresolved. Running after the CT-
# assignment removes that dead end.
#
# Order within this script is therefore load-bearing:
#
#   1. CT-  fills blank contact_id
#   2. CI-  fills blank source_case_alert_id where the case has a name
#   3. CX-  fills the rest, keyed on the (now guaranteed) contact_id
#
# Verified safe on the real data: the blank-contact_id and blank-source sets are
# disjoint (0 rows blank on both), and running CT- before the source is resolved
# causes 0 fingerprint collisions — 13 distinct CT- fingerprints either way.
#
# Step 1 still owns the structural repair and the corroborated linelist fill,
# because CT-'s own fingerprint includes source_case_alert_id and wants the
# repaired value.

# normalize_key() lives in 1-colname-standard.R, which clears it on the way out.
ct_assign_normalize_key <- function(x) {
  x <- ct_assign_translit(tolower(trimws(as.character(x))))
  gsub("[^a-z0-9]", "", x)
}

# ==========  CI- IDs for source cases with no alert_id at all  ===============
#
# After the linelist fill above, the rows still without a source_case_alert_id
# get an ID of our own — "CI-" for cas index — mirroring the epi pipeline's LL-
# and 2-assign-contact-ids.R's CT-.
#
# READ THIS BEFORE TRUSTING THE IDs. The fingerprint is case_name ALONE, and
# that is materially weaker than the other two schemes:
#
#   LL- : name + sex + age + reporting_date + date_of_notification  (5 fields)
#   CT- : contact_name + sex + age + source_case + registration_date (5 fields)
#   CI- : case_name                                                  (1 field)
#
# A CT row carries nothing else about the source case — every other field
# (contact_sex, contact_age, registration_date, health_zone) describes the
# CONTACT. There is no case_sex, case_age or case reporting date to add.
#
# case_name is at least perfectly stable: zero changes across 9,844
# comparisons of the same contact's rows in consecutive exports. The weakness
# is discrimination, and it bites. Of the 12 names remaining after the linelist
# fill, three appear to be variants of another name in the same health zone:
#
#   LIKAI SABO (4 rows)   /  LIKAI TSABO (3 rows)    Mangala   JW 0.018
#   SIFA (1)              /  SIFA MBOYALE SIMON (1)  Boma M.   JW 0.150
#   TALIKO (3)            /  TALIKO TEKA FISTON (3)  Isiro     JW 0.590
#
# Exact-name assignment therefore mints a separate CI- ID for each half of
# those pairs. The TALIKO case is the worst: the long form is filled with a
# real linelist ID (RDC-AAA-BBB-26-0001242-MVE) while the short form gets a
# CI-, splitting one apparent person across two ID systems.
#
# This was a deliberate choice — exact matching now, merge later — so the
# registry is built to make that merge easy rather than to pretend the problem
# is absent:
#
#   * possible_variant_of         another entry whose name is a token subset or
#                                 superset of this one, or within JW 0.10, in
#                                 the same health zone. Advisory only.
#   * suggested_linelist_alert_id the linelist candidate that failed
#                                 corroboration (OKWERA), so the link survives.
#   * superseded_by_dhis2_id      set if DHIS2 later supplies a real alert ID.
#
# Merging two CI- IDs means retiring one via superseded_by_dhis2_id and
# repointing its rows; nothing here does that automatically.
ASSIGNED_CASE_PREFIX <- "CI-"
ASSIGNED_CASE_RE     <- "^CI-[0-9]{6}$"

# The CI- fingerprint joins its sorted tokens with a SPACE, unlike
# ct_name_key(), which concatenates them because it feeds Jaro-Winkler where
# spaces are noise. The difference is load-bearing: the variant check below
# compares token SETS, and on a concatenated key strsplit(x, " ") returns a
# single token, so a subset test can never fire. That silently hid both
# short-form pairs (SIFA / SIFA MBOYALE SIMON and TALIKO / TALIKO TEKA FISTON)
# the first time this ran.
ct_case_key <- function(x) {
  s <- ct_assign_translit(tolower(trimws(as.character(x))))
  s <- gsub("[^a-z0-9 ]", " ", s)
  vapply(strsplit(trimws(s), "[[:space:]]+"), function(tk) {
    tk <- tk[nzchar(tk)]
    if (!length(tk)) return("")
    paste(sort(tk), collapse = " ")
  }, character(1))
}

# Token-subset or this JW distance, in the same zone, marks a possible variant.
CASE_VARIANT_MAX_DIST <- 0.10

ASSIGNED_CASE_REGISTRY_COLUMNS <- c(
  "assigned_id",                  # CI-nnnnnn, sequential, never reissued
  "fingerprint",                  # normalised case_name — the whole key
  "health_zone",                  # no case name spans >1 zone in the data
  "first_assigned_run",
  "num_ct_rows",                  # rows carrying this ID in counts_from_run
  "counts_from_run",
  "possible_variant_of",          # advisory — see the note above
  "suggested_linelist_alert_id",   # candidate that failed corroboration
  "superseded_by_dhis2_id",
  "superseded_in_run"
)

assigned_case_registry_path <- function(processed_dir) {
  file.path(processed_dir, "assigned_case_ids.csv")
}

load_assigned_case_id_registry <- function(processed_dir) {
  path <- assigned_case_registry_path(processed_dir)
  cols <- ASSIGNED_CASE_REGISTRY_COLUMNS
  if (!file.exists(path)) {
    return(as.data.frame(
      setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    ))
  }
  reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character")
  wrong_prefix <- "assigned_id" %in% names(reg) && nrow(reg) > 0 &&
    !all(grepl(ASSIGNED_CASE_RE, reg$assigned_id))
  missing_cols <- setdiff(c("assigned_id", "fingerprint"), names(reg))
  if (wrong_prefix || length(missing_cols) > 0) {
    stop("Assigned case-ID registry uses a superseded schema.\n",
         if (length(missing_cols) > 0)
           paste0("  Missing column(s): ", paste(missing_cols, collapse = ", "),
                  ".\n") else "",
         if (wrong_prefix)
           paste0("  Its IDs do not match ", ASSIGNED_CASE_RE, " — e.g. ",
                  reg$assigned_id[1], ".\n") else "",
         "  Delete it and let this run rebuild it:\n    ", path,
         "\n  (Safe while no assigned case ID has been used downstream.)",
         call. = FALSE)
  }
  for (cl in setdiff(cols, names(reg))) reg[[cl]] <- NA_character_
  reg[, cols, drop = FALSE]
}

save_assigned_case_id_registry <- function(reg, processed_dir) {
  ord <- order(suppressWarnings(
    as.numeric(sub(ASSIGNED_CASE_PREFIX, "", reg$assigned_id))
  ))
  ct_write_csv(reg[ord, , drop = FALSE],
                      assigned_case_registry_path(processed_dir))
}

# Assign CI- IDs to rows still lacking a source_case_alert_id.
ct_assign_case_ids <- function(source_id, case_name, health_zone, suggested,
                               reg, run_stem) {
  n <- length(source_id)
  out <- as.character(source_id)
  action <- rep(NA_character_, n)

  blank <- ct_assign_blank_or_na(out)
  if (!any(blank)) {
    return(list(value = out, action = action, registry = reg,
                n_new = 0L, n_reused = 0L, n_unassignable = 0L))
  }

  nm_key <- ct_case_key(case_name)
  nameless <- blank & !nzchar(nm_key)
  action[nameless] <- "unresolved_no_case_name"
  usable <- blank & nzchar(nm_key)

  known <- stats::setNames(reg$assigned_id, reg$fingerprint)
  next_num <- if (nrow(reg)) {
    max(suppressWarnings(
      as.numeric(sub(ASSIGNED_CASE_PREFIX, "", reg$assigned_id))
    ), na.rm = TRUE) + 1L
  } else {
    1L
  }
  if (!is.finite(next_num)) next_num <- 1L

  n_new <- 0L; n_reused <- 0L
  new_rows <- list()
  for (i in which(usable)) {
    # Single bracket — known[["absent name"]] errors on an atomic vector.
    hit <- unname(known[nm_key[i]])
    if (!is.na(hit)) {
      out[i] <- hit
      action[i] <- "assigned_case_id_reused"
      n_reused <- n_reused + 1L
      next
    }
    new_id <- sprintf("%s%06d", ASSIGNED_CASE_PREFIX, next_num)
    next_num <- next_num + 1L
    known[nm_key[i]] <- new_id
    out[i] <- new_id
    action[i] <- "assigned_case_id_new"
    n_new <- n_new + 1L
    new_rows[[length(new_rows) + 1L]] <- data.frame(
      assigned_id = new_id, fingerprint = nm_key[i],
      health_zone = as.character(health_zone)[i],
      first_assigned_run = run_stem,
      num_ct_rows = NA_character_, counts_from_run = NA_character_,
      possible_variant_of = NA_character_,
      suggested_linelist_alert_id = as.character(suggested)[i],
      superseded_by_dhis2_id = NA_character_, superseded_in_run = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(new_rows)) reg <- rbind(reg, do.call(rbind, new_rows))

  list(value = out, action = action, registry = reg, n_new = n_new,
       n_reused = n_reused, n_unassignable = sum(nameless))
}

update_assigned_case_row_counts <- function(reg, final_ids, run_stem) {
  if (!nrow(reg)) return(reg)
  cnt_tab <- table(final_ids[grepl(ASSIGNED_CASE_RE, final_ids)])
  counts <- stats::setNames(as.integer(cnt_tab), names(cnt_tab))
  this_key <- ct_assign_run_sortkey(run_stem)
  stored_key <- vapply(reg$counts_from_run, function(s) {
    if (is.na(s) || !nzchar(s)) "" else ct_assign_run_sortkey(s)
  }, character(1), USE.NAMES = FALSE)
  for (k in seq_len(nrow(reg))) {
    if (nzchar(stored_key[k]) && this_key < stored_key[k]) next
    n <- unname(counts[reg$assigned_id[k]])
    reg$num_ct_rows[k] <- as.character(if (is.na(n)) 0L else n)
    reg$counts_from_run[k] <- run_stem
  }
  reg
}


# Populate possible_variant_of. Advisory only — nothing merges automatically.
#
# HOW TO READ THE RESULT. A single-token CI- name is a subset of every name
# containing that token, so it will list several candidates: OKWERA matches
# five Isiro linelist people whose names include "okwera". That is genuine
# ambiguity, not noise — any of them could be the person — but it is a
# different thing from a one-to-one variant:
#
#   exactly one hit    a probable merge (CI-000005 taliko -> the alert_id that
#                      TALIKO TEKA FISTON was filled with)
#   several hits       ambiguous; the name is too short to identify anyone
#   no hits            nothing similar in the same zone
#
# Note also that possible_variant_of is zone-restricted while
# suggested_linelist_alert_id is not, so the two can point elsewhere and both
# be right: OKWERA's suggested candidate is in Boma Mangbetu (which is why
# corroboration refused it) while its variant candidates are all in Isiro.
flag_possible_case_name_variants <- function(reg, corroboration = NULL) {
  if (!nrow(reg)) return(reg)

  # Candidates to compare against: the other CI- entries, PLUS every linelist
  # name. The linelist half is essential — TALIKO's counterpart is not another
  # CI- but TALIKO TEKA FISTON, which was filled with a real alert_id.
  cand_id <- reg$assigned_id
  cand_key <- reg$fingerprint
  cand_zone <- ct_assign_normalize_key(reg$health_zone)
  if (!is.null(corroboration) && length(corroboration$token_key_by_id)) {
    ll_id <- names(corroboration$token_key_by_id)
    cand_id <- c(cand_id, ll_id)
    cand_key <- c(cand_key, unname(corroboration$token_key_by_id))
    cand_zone <- c(cand_zone, ct_assign_normalize_key(unname(corroboration$zone_by_id[ll_id])))
  }
  cand_toks <- strsplit(cand_key, " ", fixed = TRUE)
  cand_flat <- gsub(" ", "", cand_key)

  reg_toks <- strsplit(reg$fingerprint, " ", fixed = TRUE)
  reg_zone <- ct_assign_normalize_key(reg$health_zone)
  reg_flat <- gsub(" ", "", reg$fingerprint)

  for (i in seq_len(nrow(reg))) {
    if (!nzchar(reg_zone[i])) { reg$possible_variant_of[i] <- NA_character_; next }
    same_zone <- which(cand_zone == reg_zone[i] & cand_id != reg$assigned_id[i])
    if (!length(same_zone)) { reg$possible_variant_of[i] <- NA_character_; next }

    ti <- setdiff(reg_toks[[i]], "")
    dists <- stringdist::stringdist(reg_flat[i], cand_flat[same_zone],
                                    method = "jw", p = 0.1)
    hits <- character(0)
    for (n in seq_along(same_zone)) {
      j <- same_zone[n]
      tj <- setdiff(cand_toks[[j]], "")
      # Two arms, with different jobs.
      #
      # Subset: catches a short form of a longer name (SIFA inside SIFA MBOYALE
      # SIMON, TALIKO inside TALIKO TEKA FISTON). Unrestricted.
      subset_rel <- length(ti) > 0 && length(tj) > 0 &&
        (all(ti %in% tj) || all(tj %in% ti))

      # Jaro-Winkler: catches a spelling variant (LIKAI SABO / LIKAI TSABO).
      # Restricted to names of two or more tokens on BOTH sides, because JW is
      # prefix-weighted and runs loose on short strings — unrestricted, the
      # single-token OKWERA matched five unrelated Isiro names at <= 0.10.
      close <- length(ti) >= 2L && length(tj) >= 2L &&
        !is.na(dists[n]) && dists[n] <= CASE_VARIANT_MAX_DIST

      if (subset_rel || close) hits <- c(hits, cand_id[j])
    }
    reg$possible_variant_of[i] <- if (length(hits)) {
      paste(sort(unique(hits)), collapse = ";")
    } else {
      NA_character_
    }
  }
  reg
}

# =====  CX- IDs where even the source case's NAME is missing  ================
#
# After the linelist fill and the CI- assignment, 20 rows of
# CONTACT-TRACING_01092026 still have nothing: no source_case_alert_id and no
# case_name. There is no case identity to work from at all, so these are keyed
# on the CONTACT's own exposure instead:
#
#     contact_id | last_contact_with_source_date
#
# A SEPARATE PREFIX ON PURPOSE. A CI- ID says "a named source case we could not
# find an alert_id for". A CX- ID says something much weaker: "the unknown
# exposure behind this contact". Sharing one namespace would let the second be
# mistaken for the first, so they are kept visibly distinct.
#
# WHY THESE TWO FIELDS AND NOT THE OTHER OBVIOUS ONES. Measured across 2,633
# comparisons of the same follow-up row in consecutive exports, restricted to
# these no-name rows:
#
#     contact_id                       0 changes
#     last_contact_with_source_date    0 changes
#     health_zone                      3 changes  (0.11%)
#     case_circumstances             290 changes  (11.0%)
#     relationship_to_case           290 changes  (11.0%)
#
# case_circumstances is also useless here even setting instability aside: all
# 20 rows say "household", so it discriminates nothing. And
# relationship_to_case describes the CONTACT's relation to the case, so using
# it to identify the case would split one source case across its own contacts —
# a sibling and a neighbour of the same person getting different source IDs.
#
# WHAT THIS DOES AND DOES NOT CLAIM. Because contact_id is in the key, this
# yields ONE CX- ID PER CONTACT — 15 from the 15 contacts involved. It does not
# assert that any two contacts share a source case. Grouping them by
# health_zone + last_contact_with_source_date instead would give 4 IDs and
# imply that, e.g., eight contacts in one zone who last saw someone on the
# same day were exposed to one person. That is plausible but unproven, so the deliberate
# choice here is to claim nothing: a CX- ID is a placeholder for one contact's
# unknown exposure, and counting distinct CX- IDs as index cases would
# over-count. The registry records the basis so a later merge is possible.
#
# Note this registry holds no patient names — only contact IDs and a date —
# unlike the LL-/CT-/CI- registries.
ASSIGNED_EXPOSURE_PREFIX <- "CX-"
ASSIGNED_EXPOSURE_RE     <- "^CX-[0-9]{6}$"

ASSIGNED_EXPOSURE_REGISTRY_COLUMNS <- c(
  "assigned_id",             # CX-nnnnnn, sequential, never reissued
  "fingerprint",             # contact_id | last_contact_with_source_date
  "health_zone",
  "first_assigned_run",
  "num_ct_rows",
  "counts_from_run",
  "possible_shared_exposure_with",  # advisory — see below
  "superseded_by_dhis2_id",
  "superseded_in_run"
)

assigned_exposure_registry_path <- function(processed_dir) {
  file.path(processed_dir, "assigned_exposure_ids.csv")
}

exposure_fingerprint <- function(contact_id, last_contact_date) {
  tidy <- function(x) {
    out <- tolower(trimws(as.character(x)))
    out[is.na(out)] <- ""
    out
  }
  paste(tidy(contact_id), tidy(last_contact_date), sep = "|")
}

load_assigned_exposure_id_registry <- function(processed_dir) {
  path <- assigned_exposure_registry_path(processed_dir)
  cols <- ASSIGNED_EXPOSURE_REGISTRY_COLUMNS
  if (!file.exists(path)) {
    return(as.data.frame(
      setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    ))
  }
  reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character")
  wrong_prefix <- "assigned_id" %in% names(reg) && nrow(reg) > 0 &&
    !all(grepl(ASSIGNED_EXPOSURE_RE, reg$assigned_id))
  missing_cols <- setdiff(c("assigned_id", "fingerprint"), names(reg))
  if (wrong_prefix || length(missing_cols) > 0) {
    stop("Assigned exposure-ID registry uses a superseded schema.\n",
         if (length(missing_cols) > 0)
           paste0("  Missing column(s): ", paste(missing_cols, collapse = ", "),
                  ".\n") else "",
         if (wrong_prefix)
           paste0("  Its IDs do not match ", ASSIGNED_EXPOSURE_RE, " — e.g. ",
                  reg$assigned_id[1], ".\n") else "",
         "  Delete it and let this run rebuild it:\n    ", path, call. = FALSE)
  }
  for (cl in setdiff(cols, names(reg))) reg[[cl]] <- NA_character_
  reg[, cols, drop = FALSE]
}

save_assigned_exposure_id_registry <- function(reg, processed_dir) {
  ord <- order(suppressWarnings(
    as.numeric(sub(ASSIGNED_EXPOSURE_PREFIX, "", reg$assigned_id))
  ))
  ct_write_csv(reg[ord, , drop = FALSE],
                      assigned_exposure_registry_path(processed_dir))
}

ct_assign_exposure_ids <- function(source_id, contact_id, last_contact_date,
                                   health_zone, reg, run_stem) {
  n <- length(source_id)
  out <- as.character(source_id)
  action <- rep(NA_character_, n)

  blank <- ct_assign_blank_or_na(out)
  if (!any(blank)) {
    return(list(value = out, action = action, registry = reg,
                n_new = 0L, n_reused = 0L, n_unassignable = 0L))
  }

  # contact_id is filled by 2-assign-contact-ids.R, which runs AFTER this
  # script. A row blank on both has nothing to key on yet, so it is left for
  # the next run rather than collapsed onto an empty fingerprint — which would
  # give every such row the same CX- ID. Zero rows in CONTACT-TRACING_01092026.
  no_contact <- blank & ct_assign_blank_or_na(contact_id)
  action[no_contact] <- "unresolved_no_case_name_and_no_contact_id"
  usable <- blank & !no_contact

  fp <- exposure_fingerprint(contact_id, last_contact_date)
  known <- stats::setNames(reg$assigned_id, reg$fingerprint)
  next_num <- if (nrow(reg)) {
    max(suppressWarnings(
      as.numeric(sub(ASSIGNED_EXPOSURE_PREFIX, "", reg$assigned_id))
    ), na.rm = TRUE) + 1L
  } else {
    1L
  }
  if (!is.finite(next_num)) next_num <- 1L

  n_new <- 0L; n_reused <- 0L
  new_rows <- list()
  for (i in which(usable)) {
    hit <- unname(known[fp[i]])   # single bracket: NA on a miss, not an error
    if (!is.na(hit)) {
      out[i] <- hit
      action[i] <- "assigned_exposure_id_reused"
      n_reused <- n_reused + 1L
      next
    }
    new_id <- sprintf("%s%06d", ASSIGNED_EXPOSURE_PREFIX, next_num)
    next_num <- next_num + 1L
    known[fp[i]] <- new_id
    out[i] <- new_id
    action[i] <- "assigned_exposure_id_new"
    n_new <- n_new + 1L
    new_rows[[length(new_rows) + 1L]] <- data.frame(
      assigned_id = new_id, fingerprint = fp[i],
      health_zone = as.character(health_zone)[i],
      first_assigned_run = run_stem,
      num_ct_rows = NA_character_, counts_from_run = NA_character_,
      possible_shared_exposure_with = NA_character_,
      superseded_by_dhis2_id = NA_character_, superseded_in_run = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(new_rows)) reg <- rbind(reg, do.call(rbind, new_rows))

  list(value = out, action = action, registry = reg, n_new = n_new,
       n_reused = n_reused, n_unassignable = sum(no_contact))
}

# Advisory only: CX- entries that plausibly share ONE source case.
#
# One CX- ID per contact is the deliberate choice — it claims nothing about
# grouping. But the grouping is often obvious in the data and should not be
# thrown away: CONTACT-TRACING_01092026 has eight CONSECUTIVE contact IDs
# in one health zone with the same last-contact date, which is a household
# registered in one sitting rather than eight independent exposures.
#
# Grouped on health_zone + last_contact_with_source_date, the two fields that
# would identify a shared exposure and are both stable (0 and 0.11% change).
# Nothing merges automatically; this exists so that counting distinct CX- IDs
# as index cases can be corrected knowingly rather than by accident.
flag_possible_shared_exposures <- function(reg) {
  if (!nrow(reg)) return(reg)
  # The fingerprint is "contact_id|last_contact_date", so the date is its tail.
  last_contact <- sub("^[^|]*\\|", "", reg$fingerprint)
  key <- paste(ct_assign_normalize_key(reg$health_zone), last_contact, sep = "|")
  for (i in seq_len(nrow(reg))) {
    peers <- reg$assigned_id[key == key[i] & reg$assigned_id != reg$assigned_id[i]]
    reg$possible_shared_exposure_with[i] <- if (length(peers)) {
      paste(sort(unique(peers)), collapse = ";")
    } else {
      NA_character_
    }
  }
  reg
}

update_assigned_exposure_row_counts <- function(reg, final_ids, run_stem) {
  if (!nrow(reg)) return(reg)
  cnt_tab <- table(final_ids[grepl(ASSIGNED_EXPOSURE_RE, final_ids)])
  counts <- stats::setNames(as.integer(cnt_tab), names(cnt_tab))
  this_key <- ct_assign_run_sortkey(run_stem)
  stored_key <- vapply(reg$counts_from_run, function(s) {
    if (is.na(s) || !nzchar(s)) "" else ct_assign_run_sortkey(s)
  }, character(1), USE.NAMES = FALSE)
  for (k in seq_len(nrow(reg))) {
    if (nzchar(stored_key[k]) && this_key < stored_key[k]) next
    n <- unname(counts[reg$assigned_id[k]])
    reg$num_ct_rows[k] <- as.character(if (is.na(n)) 0L else n)
    reg$counts_from_run[k] <- run_stem
  }
  reg
}



# -------------------------  Locate the run  -----------------------------------

ct_processed_dir <- file.path(repo_root, "data", "processed",
                              "contact_tracing_processed")

# ct_force_stems lets the Shiny app target specific runs, matching the
# convention 1-colname-standard.R uses.
ct_assign_stems <- if (exists("ct_force_stems") && length(ct_force_stems) > 0) {
  ct_force_stems
} else {
  basename(resolve_ct_latest_dir(ct_processed_dir))
}

message("=== Assigning contact IDs to rows with no contact_id ===")
message("Runs to process: ", paste(ct_assign_stems, collapse = ", "))

assigned_contact_registry <- load_assigned_contact_id_registry(ct_processed_dir)
message("Loaded ", nrow(assigned_contact_registry),
        " assigned contact ID(s) from ",
        assigned_contact_registry_path(ct_processed_dir))

assigned_case_registry <- load_assigned_case_id_registry(ct_processed_dir)
message("Loaded ", nrow(assigned_case_registry), " assigned case ID(s) from ",
        assigned_case_registry_path(ct_processed_dir))

assigned_exposure_registry <- load_assigned_exposure_id_registry(ct_processed_dir)
message("Loaded ", nrow(assigned_exposure_registry),
        " assigned exposure ID(s) from ",
        assigned_exposure_registry_path(ct_processed_dir))

# Linelist names, for the CI- variant check only — it has to be able to notice
# that TALIKO is a short form of TALIKO TEKA FISTON, whose long form was filled
# with a real alert_id in step 1 and so is not in any CI- registry.
# 1-colname-standard.R has its own copy of this for the repair; it clears it on
# the way out, so this is a small independent loader rather than a dependency.
ct_case_corroboration <- local({
  epi_dir <- file.path(repo_root, "data", "processed", "dhis2_linelist_processed")
  folder <- read_latest("epi_linelist")$folder
  if (length(folder) != 1 || is.na(folder) || !nzchar(folder)) return(NULL)
  path <- file.path(epi_dir, folder, "dhis2_processed_linelist_names.csv")
  if (!file.exists(path)) {
    message("NOTE: no processed linelist _names.csv — CI- variant flagging ",
            "will compare CI- entries against each other only.")
    return(NULL)
  }
  epi <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character")
  if (!all(c("alert_id", "name") %in% names(epi))) return(NULL)
  keep <- grepl("^RDC-[A-Z]{3}-[A-Z]{3}-26-[0-9]{6,7}-MVE$",
                toupper(trimws(epi$alert_id)))
  epi <- epi[keep, , drop = FALSE]
  epi <- epi[!duplicated(epi$alert_id), , drop = FALSE]
  aid <- toupper(trimws(epi$alert_id))
  zone <- if ("health_zone" %in% names(epi)) as.character(epi$health_zone) else rep("", nrow(epi))
  message("CI- variant check will also compare against ", nrow(epi),
          " linelist name(s)")
  list(token_key_by_id = stats::setNames(ct_case_key(epi$name), aid),
       zone_by_id = stats::setNames(zone, aid))
})


for (stem in ct_assign_stems) {
  run_dir   <- file.path(ct_processed_dir, stem)
  names_csv <- file.path(run_dir, "contact_tracing_processed_names.csv")

  if (!file.exists(names_csv)) {
    message("  Skipping ", stem,
            " — no contact_tracing_processed_names.csv (contact_name is ",
            "required to recognise a row across exports).")
    next
  }

  df <- utils::read.csv(names_csv, stringsAsFactors = FALSE, na.strings = "",
                        colClasses = "character")
  message("\n--- ", stem, " (", nrow(df), " rows) ---")

  if (!"contact_id" %in% names(df)) {
    message("  No contact_id column — skipping.")
    next
  }

  # dhis2_contact_id preserves what the export supplied, for every row
  # including blanks. contact_id becomes our canonical value.
  if (!"dhis2_contact_id" %in% names(df)) {
    df$dhis2_contact_id <- as.character(df$contact_id)
  }

  fp <- contact_fingerprint(df)

  assigned_contact_registry <- record_superseded_contact_ids(
    assigned_contact_registry, fp, df$contact_id, stem
  )

  # ---- Fill from contact_uid first (added 2026-10-30) ----------------------
  # Exports from 21 Oct 2026 carry `UID contact` (contact_uid): the source
  # system's own ID, never blank and 1:1 with contact_id where both exist
  # (checked on CONTACT_TRACING_30102026: 23,632 pairs, no exceptions). A row
  # with no contact_id takes its contact_uid rather than a made-up CT- ID.
  #
  # EXCEPT a contact already holding a CT- ID: it keeps it. Reviewer votes,
  # the link ledger and the person registry are keyed on the contact_id
  # value, so switching an existing CT- ID to a UID would orphan that work.
  # Decided per contact_uid, so every row of one contact gets the same ID.
  uid_fill <- rep(FALSE, nrow(df))
  uid_pending <- rep(FALSE, nrow(df))
  if ("contact_uid" %in% names(df)) {
    blank_cid <- ct_assign_blank_or_na(df$contact_id)
    has_uid   <- !ct_assign_blank_or_na(df$contact_uid)
    known_fp  <- assigned_contact_registry$fingerprint
    uid_has_ct <- unique(df$contact_uid[blank_cid & has_uid & fp %in% known_fp])
    uid_fill <- blank_cid & has_uid & !(df$contact_uid %in% uid_has_ct)
    df$contact_id[uid_fill] <- trimws(df$contact_uid[uid_fill])
    # Rows of a contact that already has a CT- ID, but whose own fingerprint
    # is not in the registry (a field changed): held back from CT- assignment,
    # which would mint them a second ID, and given the contact's existing one
    # below.
    uid_pending <- blank_cid & has_uid & df$contact_uid %in% uid_has_ct &
      !(fp %in% known_fp)
    df$contact_id[uid_pending] <- "__pending_contact_uid__"
    message("  Filled from contact_uid: ", sum(uid_fill), " row(s), ",
            length(unique(df$contact_uid[uid_fill])), " contact(s)",
            if (length(uid_has_ct))
              paste0(" | kept existing CT- IDs: ", length(uid_has_ct),
                     " contact(s)") else "")
  }

  res <- assign_missing_contact_ids(df$contact_id, fp, df$contact_name,
                                    assigned_contact_registry, stem)
  df$contact_id <- res$value
  assigned_contact_registry <- res$registry
  df$contact_id_assigned <- ct_relabel_by_first_run(
    res$label, df$contact_id, assigned_contact_registry, stem
  )
  df$contact_id_assigned[uid_fill] <- "filled_from_contact_uid"

  if (any(uid_pending)) {
    donor <- !uid_pending & !ct_assign_blank_or_na(df$contact_uid) &
      grepl(ASSIGNED_CONTACT_RE, df$contact_id)
    id_of  <- stats::setNames(df$contact_id[donor], df$contact_uid[donor])
    lab_of <- stats::setNames(df$contact_id_assigned[donor], df$contact_uid[donor])
    df$contact_id[uid_pending] <- unname(id_of[df$contact_uid[uid_pending]])
    df$contact_id_assigned[uid_pending] <- unname(lab_of[df$contact_uid[uid_pending]])
    message("  ", sum(uid_pending), " row(s) given their contact's existing ",
            "CT- ID via contact_uid (fingerprint had changed)")
  }

  message("  Newly assigned: ", res$n_new,
          " | reused from the registry: ", res$n_reused,
          " | unassignable (no contact name): ", res$n_unassignable)

  assigned_contact_registry <- update_assigned_contact_row_counts(
    assigned_contact_registry, df$contact_id, stem
  )

  # A contact_id shared by more than one row is expected here — CT is one row
  # per follow-up — so this looks only for rows that share an id with a
  # DIFFERENT fingerprint, which would mean two people collapsed onto one id.
  #
  # Reported through the log rather than as a column: it was FALSE on all
  # 20,632 rows of CONTACT-TRACING_01092026, so as a column it added width to
  # every export and told the reader nothing. The condition still matters, and
  # the warning below is loud when it holds.
  by_id <- split(fp, df$contact_id)
  bad_ids <- names(by_id)[vapply(by_id, function(v) length(unique(v)) > 1L,
                                 logical(1))]
  bad_ids <- bad_ids[!ct_assign_blank_or_na(bad_ids)]
  if (length(bad_ids)) {
    message("  WARNING: ", length(bad_ids), " contact_id(s) cover more than one ",
            "distinct person: ", paste(utils::head(bad_ids, 10), collapse = ", "))
  }

  # ==========================================================================
  # With contact_id now guaranteed, resolve blank source_case_alert_id
  # ==========================================================================
  if ("source_case_alert_id" %in% names(df)) {
    cn_vec <- if ("case_name" %in% names(df)) df$case_name else rep(NA_character_, nrow(df))
    hz_vec <- if ("health_zone" %in% names(df)) df$health_zone else rep(NA_character_, nrow(df))

    # Step 1 already ran the structural repair and the corroborated linelist
    # fill, and recorded the outcome per row.
    fill_note <- if ("source_case_alert_id_fill_note" %in% names(df)) {
      df$source_case_alert_id_fill_note
    } else {
      rep(NA_character_, nrow(df))
    }
    suggested <- if ("source_case_alert_id_suggested" %in% names(df)) {
      df$source_case_alert_id_suggested
    } else {
      rep(NA_character_, nrow(df))
    }

    message("\n  ASSIGNING CI- IDs TO REMAINING SOURCE CASES")
    cas <- ct_assign_case_ids(df$source_case_alert_id, cn_vec, hz_vec,
                              suggested, assigned_case_registry, stem)
    df$source_case_alert_id <- cas$value
    assigned_case_registry <- cas$registry
    message("    Newly assigned: ", cas$n_new, " | reused: ", cas$n_reused,
            " | no case name: ", cas$n_unassignable)

    # CX- keys on contact_id, which is why this whole block moved here from
    # step 1: by now every assignable row has one.
    message("  ASSIGNING CX- IDs WHERE THE SOURCE CASE HAS NO NAME EITHER")
    lc_vec <- if ("last_contact_with_source_date" %in% names(df)) {
      df$last_contact_with_source_date
    } else {
      rep(NA_character_, nrow(df))
    }
    exp_res <- ct_assign_exposure_ids(df$source_case_alert_id, df$contact_id,
                                      lc_vec, hz_vec,
                                      assigned_exposure_registry, stem)
    df$source_case_alert_id <- exp_res$value
    assigned_exposure_registry <- exp_res$registry
    message("    Newly assigned: ", exp_res$n_new, " | reused: ",
            exp_res$n_reused, " | still unresolved: ", exp_res$n_unassignable)
    if (exp_res$n_new > 0 || exp_res$n_reused > 0) {
      message("    One CX- ID per CONTACT, not per source case: these assert ",
              "only 'this contact's unknown exposure'. Counting distinct CX- ",
              "IDs as index cases would over-count.")
    }

    # One column carrying both halves of the story: WHAT the row ended up with
    # and WHY step 1's fill could not supply it, as
    # `assigned_case_id_new__name_not_in_linelist`.
    #
    # These were two columns, `_fill_note` (reason, from step 1) and
    # `_resolution` (outcome, from here). They covered the same 81 rows of
    # 20,632 and mapped nearly one-to-one, so they are folded together and
    # `_fill_note` is dropped from the output below. Step 1 still writes it —
    # this script reads it, and step 1 has to stay runnable on its own.
    case_action <- ct_relabel_by_first_run(cas$action, df$source_case_alert_id,
                                           assigned_case_registry, stem)
    exp_action  <- ct_relabel_by_first_run(exp_res$action,
                                           df$source_case_alert_id,
                                           assigned_exposure_registry, stem)

    # Outcomes are mutually exclusive on the real data (7 + 54 + 20 = 81, no
    # row in two categories), but applied in the order tried so that the
    # later stage wins if that ever stops holding. CX- must override the
    # `unresolved_no_case_name` that CI- leaves on a nameless source case.
    resolution <- ifelse(!is.na(fill_note) &
                           fill_note == "filled_from_linelist_name",
                         "filled_from_linelist_name", NA_character_)
    resolution[!is.na(case_action)] <- case_action[!is.na(case_action)]
    resolution[!is.na(exp_action)]  <- exp_action[!is.na(exp_action)]

    # `not_filled_` is noise once the outcome is stated alongside it.
    reason <- sub("^not_filled_", "", fill_note)
    has_reason <- !is.na(resolution) & !is.na(reason) &
      resolution != "filled_from_linelist_name"
    resolution[has_reason] <- paste0(resolution[has_reason], "__",
                                     reason[has_reason])
    df$source_case_alert_id_resolution <- resolution
    df$source_case_alert_id_fill_note <- NULL

    # Also redundant. It was non-blank on 13 rows: on 7 it simply repeated the
    # value that landed in source_case_alert_id, and on the other 6 it held a
    # candidate that was REJECTED (all six suggesting the `okwera` in Boma
    # Mangbetu for a contact in Isiro — the zone-mismatch refusal). Those six
    # are recorded in assigned_case_ids.csv as suggested_linelist_alert_id, so
    # the column duplicated the outcome or duplicated the registry.
    df$source_case_alert_id_suggested <- NULL

    assigned_case_registry <- update_assigned_case_row_counts(
      assigned_case_registry, df$source_case_alert_id, stem
    )
    assigned_case_registry <- flag_possible_case_name_variants(
      assigned_case_registry, ct_case_corroboration
    )
    assigned_exposure_registry <- update_assigned_exposure_row_counts(
      assigned_exposure_registry, df$source_case_alert_id, stem
    )
    assigned_exposure_registry <- flag_possible_shared_exposures(
      assigned_exposure_registry
    )

    message("  Resolution of rows the export left without a source case:")
    print(table(resolution, useNA = "no"))

    n_variant <- sum(!ct_assign_blank_or_na(
      assigned_case_registry$possible_variant_of))
    if (n_variant > 0) {
      message("  WARNING: ", n_variant, " CI- ID(s) look like a name variant of ",
              "another and may be the same person — the fingerprint is ",
              "case_name alone. See possible_variant_of in ",
              basename(assigned_case_registry_path(ct_processed_dir)), ".")
      vr <- assigned_case_registry[!ct_assign_blank_or_na(
        assigned_case_registry$possible_variant_of), , drop = FALSE]
      for (k in seq_len(nrow(vr))) {
        message("    ", vr$assigned_id[k], " (", vr$fingerprint[k], ", ",
                vr$health_zone[k], ") ~ ", vr$possible_variant_of[k])
      }
      message("    Nothing is merged automatically.")
    }
    n_shared <- sum(!ct_assign_blank_or_na(
      assigned_exposure_registry$possible_shared_exposure_with))
    if (n_shared > 0) {
      message("  ", n_shared, " CX- ID(s) share a health zone and last-contact ",
              "date with another and may be one source case — see ",
              "possible_shared_exposure_with in ",
              basename(assigned_exposure_registry_path(ct_processed_dir)), ".")
    }
  }

  # Each ID sits next to the raw value it was derived from and its companion
  # columns, source case first, then contact. Both families move together to
  # just after the geography block. Previously source_case_alert_id was column
  # 7 while its dhis2_ twin was column 25, so the two could not be read
  # against each other, while the contact family was already grouped.
  src_family <- intersect(c("source_case_alert_id",
                            "dhis2_source_case_alert_id",
                            "source_case_alert_id_valid",
                            "source_case_alert_id_issue",
                            "source_case_alert_id_resolution"), names(df))
  ct_family  <- intersect(c("contact_id", "dhis2_contact_id",
                            "contact_id_assigned", "contact_uid"), names(df))
  families <- c(src_family, ct_family)
  rest <- setdiff(names(df), families)

  # Anchor on the last geography column still present, so the families land
  # after it however that block is composed.
  anchor <- intersect(c("health_area_unmatched", "health_area",
                        "health_zone_unmatched", "health_zone",
                        "province", "country"), rest)
  at <- if (length(anchor)) match(anchor[1], rest) else 0L
  df <- df[, c(rest[seq_len(at)], families,
               rest[setdiff(seq_along(rest), seq_len(at))]), drop = FALSE]

  # Rewrite both outputs so the assigned IDs reach every consumer. The
  # ctsafe allowlist is read back from the file 1-colname-standard.R wrote,
  # rather than reconstructed, so it cannot drift out of step with it.
  # (There is no third, no-names copy any more: anything that must not see
  # PII drops pii_columns.contact_tracing from _names.csv on load.)
  ct_write_csv(df, names_csv)

  safe_csv <- file.path(run_dir, "contact_tracing_processed_ctsafe.csv")
  if (file.exists(safe_csv)) {
    safe_cols <- names(utils::read.csv(safe_csv, nrows = 1,
                                       stringsAsFactors = FALSE,
                                       check.names = FALSE))
    # dhis2_contact_id and contact_id_assigned join the ctsafe export on
    # purpose: contact_id may now hold a CT-nnnnnn value, so a consumer
    # parsing it needs to be able to tell the two apart. Additive, so existing
    # consumers are unaffected.
    safe_cols <- unique(c(safe_cols, "dhis2_contact_id", "contact_id_assigned"))
    ct_write_csv(df[, intersect(safe_cols, names(df)), drop = FALSE], safe_csv)
  }
  message("  Rewrote names / ctsafe CSVs in ", stem)
}

if (nrow(assigned_contact_registry) > 0) {
  save_assigned_contact_id_registry(assigned_contact_registry, ct_processed_dir)
  message("\nAssigned contact-ID registry saved: ",
          nrow(assigned_contact_registry), " entries -> ",
          assigned_contact_registry_path(ct_processed_dir))
  message("  NOTE: this file contains patient names. Handle it exactly like ",
          "contact_tracing_processed_names.csv.")
}

if (nrow(assigned_case_registry) > 0) {
  save_assigned_case_id_registry(assigned_case_registry, ct_processed_dir)
  message("Assigned case-ID registry saved: ", nrow(assigned_case_registry),
          " entries -> ", assigned_case_registry_path(ct_processed_dir))
  message("  NOTE: this file contains patient names.")
}

if (nrow(assigned_exposure_registry) > 0) {
  save_assigned_exposure_id_registry(assigned_exposure_registry, ct_processed_dir)
  message("Assigned exposure-ID registry saved: ",
          nrow(assigned_exposure_registry), " entries -> ",
          assigned_exposure_registry_path(ct_processed_dir))
  message("  (Holds no patient names — contact IDs and a date only.)")
}

message("\nNOTE: the structural repair of source_case_alert_id and the ",
        "corroborated linelist fill both happen in 1-colname-standard.R; ",
        "this script only assigns IDs to what they could not resolve.")

# ---- Refresh the in-memory copy downstream steps read -----------------------
# This script rewrote the CSVs on disk, but 4-build-id-lookup.R matches against
# `processed_contact_tracing` in memory, which 1-colname-standard.R left as the
# PRE-assignment data frame. Without this reload the ID lookup would still see
# blank contact_ids and miss every row we just assigned.
local({
  latest <- basename(resolve_ct_latest_dir(ct_processed_dir))
  src <- file.path(ct_processed_dir, latest,
                   "contact_tracing_processed_names.csv")
  if (file.exists(src)) {
    message("Refreshing processed_contact_tracing from ", basename(src),
            " (", latest, ") so downstream steps see the assigned IDs")
    assign("processed_contact_tracing",
           utils::read.csv(src, stringsAsFactors = FALSE, na.strings = ""),
           envir = .GlobalEnv)
  } else {
    message("WARNING: could not refresh processed_contact_tracing — ",
            "downstream steps may not see the assigned contact IDs.")
  }
})

contact_id_assignment_complete <- TRUE


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, c("assigned_contact_registry",
                                       "assigned_case_registry",
                                       "assigned_exposure_registry",
                                       "contact_id_assignment_complete"))
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
