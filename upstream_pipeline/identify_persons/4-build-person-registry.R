#!/usr/bin/env Rscript
# 4-build-person-registry.R
#
# Who is who: one row per IDENTIFIER, carrying the person it belongs to.
#
#   person_id        P-000001. Assigned once, never reused.
#   identifier_type  alert_id / source_case_alert_id / contact_id
#   original_id      the id as it appears in the source file
#   status           accepted / undecided
#   decided_by       harmoniser / reviewer / auto_rule / model / none
#
# Output: data/processed/person_matching/person_registry.csv
#         data/processed/person_matching/person_registry_history.csv
#         data/processed/person_matching/person_registry_with_candidates.csv
#           (a view that ALSO joins on undecided proposals, with a
#            human_reviewed TRUE/FALSE column -- see the section of that name)
#
# This is the table you join to. `SELECT person_id FROM registry WHERE
# original_id = <alert_id>` is the whole contract, and the reason the file
# covers EVERY identifier rather than only the interesting ones (see below).
#
# Specified in specs/2026-09-03-person-id-entity-resolution.md (§3
# `person_identifiers.csv`, §7 the id lifecycle) and built on the ledger from
# specs/2026-09-10-link-ledger-after-harmonisation.md.
#
# ---------------------------------------------------------------------------
# EVERY IDENTIFIER, NOT ONLY THE LINKED ONES
# ---------------------------------------------------------------------------
# Measured on LINELIST_01092026 x CONTACT-TRACING_01092026: 38,025 identifiers
# exist across the two exports and only 5,867 of them -- 15% -- appear in any
# candidate link at all. The other 32,158 are people nobody proposed a match
# for, and they are people all the same.
#
# A registry holding only the 5,867 would leave 85% of records with a null
# person_id on a join, so every consumer would have to implement "no row means
# it is its own person" for themselves. They will do it inconsistently, and
# `n_distinct(person_id)` -- the case count this whole exercise exists to fix
# -- would be wrong for anyone who forgot. So the unlinked identifiers are
# swept in as persons of one.
#
# ---------------------------------------------------------------------------
# WHY person_id IS NOT RECOMPUTED FROM SCRATCH
# ---------------------------------------------------------------------------
# The pipeline runs on a new export most days. If components were simply
# numbered in order each run, every person_id would change meaning whenever an
# earlier cluster grew or shrank, and any analysis that had cited one would be
# silently wrong rather than loudly broken.
#
# So ids are inherited: a component whose members already carried an id keeps
# it. Merges keep the lower-numbered id and record the other as superseded.
# Splits retire the id and issue new ones to BOTH halves -- letting the larger
# half keep it would be less churn, but it would silently change what an
# already-published id means, and for an outbreak with analyses going out
# continuously that is the worse trade. Every such event is written to
# person_registry_history.csv so a historical reference still resolves.
#
# ---------------------------------------------------------------------------
# CONFIDENTIALITY
# ---------------------------------------------------------------------------
# Identifiers only, no names -- the same class as link_ledger.csv. It is still
# patient data: an alert_id encodes province and health zone, and the person_id
# groupings are themselves an assertion about people. Stays in
# person_matching/.
#
# `person_id` on its own is pseudonymous and carries no personal information,
# which is what makes it a better join key for the ctsafe exports than
# alert_id. That is a property of the COLUMN, not of this file.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

PG_REGISTRY_DIR <- person_matching_dir()   # data/processed/person_matching/
PG_LEDGER_PATH  <- file.path(PG_REGISTRY_DIR, "link_ledger.csv")
PG_OUT_PATH     <- file.path(PG_REGISTRY_DIR, "person_registry.csv")
PG_HISTORY_PATH <- file.path(PG_REGISTRY_DIR, "person_registry_history.csv")

# The PREVIOUS registry and history are always read from the live files (ids
# are inherited from them). Setting BDBV_REGISTRY_OUT_DIR sends the NEW files
# somewhere else instead -- used to compare two versions of this script on the
# same inputs without touching the live registry. Unset = normal behaviour.
PG_PREV_PATH         <- PG_OUT_PATH
PG_PREV_HISTORY_PATH <- PG_HISTORY_PATH
.pg_out_dir <- Sys.getenv("BDBV_REGISTRY_OUT_DIR", "")
if (nzchar(.pg_out_dir)) {
  PG_OUT_PATH     <- file.path(.pg_out_dir, "person_registry.csv")
  PG_HISTORY_PATH <- file.path(.pg_out_dir, "person_registry_history.csv")
}

PG_EPI_DIR <- file.path(repo_root, "data", "processed",
                        "dhis2_linelist_processed")
PG_CT_DIR  <- file.path(repo_root, "data", "processed",
                        "contact_tracing_processed")

# Node prefix -> the name the outside world uses. Three types, not two: a
# source-case id is a distinct OCCURRENCE even when its value equals a linelist
# alert_id, because one is a case record and the other is a citation of that
# case by a contact.
PG_ID_TYPE <- c(ll = "alert_id",
                ct_case = "source_case_alert_id",
                ct_contact = "contact_id")

# Authority of the link that holds an identifier in its person. Mirrors LL_RANK
# in 3-build-link-ledger.R.
PG_RANK <- c(none = 0L, model = 1L, pipeline_drop = 2L, auto_rule = 2L,
             reviewer = 3L, harmoniser = 4L)
# `model` (the link classifier, 2026-10-31) joins identifiers like any accepted
# link, at the lowest authority. It is deliberately NOT in PG_SETTLED_BY below:
# its decisions are re-made every night and can be withdrawn, so a group held
# together by one is reported as human_reviewed = FALSE.

PG_COLUMNS <- c("person_id", "identifier_type", "original_id",
                "status", "decided_by", "first_seen_run", "last_seen_run",
                # The link classifier's confidence (2026-10-31); see "Model
                # scores per identifier" below. Blank where no model is involved.
                "model_surety", "model_version")


# -------------------------  Helper Functions  ---------------------------------

.pg_log <- character(0)
pglog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .pg_log <<- c(.pg_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

pg_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

pg_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                    colClasses = "character"),
           error = function(e) NULL)
}

pg_id <- function(n) sprintf("P-%06d", n)
pg_id_num <- function(x) suppressWarnings(as.integer(sub("^P-", "", x)))

# Which processed run to sweep identifiers from: `section` of latest.json.
pg_latest_folder <- function(dir, section) {
  info <- read_latest(section)
  if (is.null(info) || is.null(info$folder)) return(NULL)
  p <- file.path(dir, info$folder)
  if (dir.exists(p)) p else NULL
}


# ---------------------------------------------------------------------------
# The ledger
# ---------------------------------------------------------------------------

ledger <- pg_read_csv(PG_LEDGER_PATH)
if (is.null(ledger)) {
  stop("no link ledger at ", PG_LEDGER_PATH,
       " -- run 3-build-link-ledger.R first", call. = FALSE)
}
for (cl in c("link_status", "decided_by", "node_a", "node_b", "last_seen_run"))
  ledger[[cl]][is.na(ledger[[cl]])] <- ""

pglog("Person registry")
pglog("  ledger: ", nrow(ledger), " link(s)")

accepted  <- ledger[ledger$link_status == "accepted", , drop = FALSE]
# `inactive` links (undecided, and no longer proposed by any current run) are
# not under consideration, so they neither join identifiers nor mark them as
# pending -- only `undecided` does that.
undecided <- ledger[ledger$link_status == "undecided", , drop = FALSE]
pglog("  ", nrow(accepted), " accepted, ", nrow(undecided), " undecided, ",
      sum(ledger$link_status == "rejected"), " rejected, ",
      sum(ledger$link_status == "inactive"), " inactive")


# ---------------------------------------------------------------------------
# Every identifier in the current exports
# ---------------------------------------------------------------------------
# Swept from the processed files rather than from the ledger, because the
# ledger only knows about identifiers something proposed a link for.

pg_distinct <- function(path, col, prefix) {
  if (!file.exists(path)) return(character(0))
  d <- tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character"),
                error = function(e) NULL)
  if (is.null(d) || !col %in% names(d)) return(character(0))
  v <- unique(trimws(d[[col]]))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(character(0))
  paste0(prefix, ":", v)
}

epi_dir <- pg_latest_folder(PG_EPI_DIR, "epi_linelist")
# CT identifiers come from the same input the matching read (combined or
# latest, per contact_tracing.data.matching_input), so every contact the
# candidates build saw gets a person_id -- not only those in the last export.
ct_csv <- tryCatch(resolve_ct_matching_input(PG_CT_DIR)$names_csv,
                   error = function(e) {
                     if (identical(pipeline_configs$contact_tracing$data$matching_input,
                                   "combined"))
                       stop(conditionMessage(e), call. = FALSE)
                     NULL
                   })
swept <- character(0)
if (!is.null(epi_dir)) {
  swept <- c(swept, pg_distinct(file.path(epi_dir,
                                          "dhis2_processed_linelist_names.csv"),
                                "alert_id", "ll"))
} else {
  pglog("  WARNING: no processed linelist found -- linelist identifiers will ",
        "be limited to those in the ledger")
}
if (!is.null(ct_csv) && file.exists(ct_csv)) {
  swept <- c(swept,
             pg_distinct(ct_csv, "contact_id", "ct_contact"),
             pg_distinct(ct_csv, "source_case_alert_id", "ct_case"))
} else {
  pglog("  WARNING: no processed contact tracing export found -- contact ",
        "identifiers will be limited to those in the ledger")
}

# ---------------------------------------------------------------------------
# Retired CT- IDs: the same person as their replacement
# ---------------------------------------------------------------------------
# An assigned contact ID issued in error is retired, not deleted: its row in
# assigned_contact_ids.csv names the ID that replaces it (`replaced_by`, see
# 2-assign-contact-ids.R). The ledger still cites the retired ID -- it never
# forgets a link -- so without this the retired ID would stand as a person of
# its own, a phantom that inflates every person count. Each retirement is
# joined here like an accepted link decided by rule, so the usual merge keeps
# the lower person_id and writes the other to the history as superseded: a
# historical reference to either still resolves.
# 2026-10: 174 IDs issued 21-23 Oct while registration_date was blank (see
# contact_tracing_pipeline/maintenance/check_duplicate_ct_ids_2026_10.py).
pg_retired <- local({
  p <- file.path(PG_CT_DIR, "assigned_contact_ids.csv")
  r <- if (file.exists(p)) tryCatch(
    read.csv(p, stringsAsFactors = FALSE, na.strings = "", colClasses = "character"),
    error = function(e) NULL) else NULL
  if (is.null(r) || !"replaced_by" %in% names(r)) {
    data.frame(node_a = character(0), node_b = character(0),
               decided_by = character(0), stringsAsFactors = FALSE)
  } else {
    r <- r[!is.na(r$replaced_by) & nzchar(trimws(r$replaced_by)), , drop = FALSE]
    data.frame(node_a = paste0("ct_contact:", trimws(r$assigned_id)),
               node_b = paste0("ct_contact:", trimws(r$replaced_by)),
               decided_by = rep("auto_rule", nrow(r)), stringsAsFactors = FALSE)
  }
})
if (nrow(pg_retired))
  pglog("  ", nrow(pg_retired), " retired contact ID(s) joined to their replacement")

nodes <- sort(unique(c(ledger$node_a, ledger$node_b, swept,
                       pg_retired$node_a, pg_retired$node_b)))
nodes <- nodes[nzchar(nodes)]
linked <- unique(c(ledger$node_a, ledger$node_b, pg_retired$node_a, pg_retired$node_b))
pglog("  ", length(nodes), " identifier(s) in total; ", length(linked),
      " appear in a candidate link (",
      round(100 * length(linked) / max(length(nodes), 1)), "%)")

# An identifier whose prefix this script does not know would be silently
# dropped from the registry, so it is an error rather than a filter.
prefix <- sub(":.*$", "", nodes)
unknown <- setdiff(unique(prefix), names(PG_ID_TYPE))
if (length(unknown)) {
  stop("identifier prefix(es) with no type mapping: ",
       paste(unknown, collapse = ", "), call. = FALSE)
}


# ---------------------------------------------------------------------------
# Components over ACCEPTED links only
# ---------------------------------------------------------------------------
# Undecided links join nothing. Merging on them would merge people nobody has
# agreed are the same, which is the one thing this pipeline has never done --
# and rejected links are excluded by construction, which is why "a rejected
# pair never shares a person_id" needs no rule of its own.
#
# Union-find over the LINKED nodes only, then singletons appended. Running it
# over all 38,025 would be 32,158 pointless finds.
#
# ON INTEGER POSITIONS, NOT NAMES (2026-10-06). It used a named character
# vector, where every lookup by name scans the names -- fine at 5,000 nodes,
# minutes at 64,000. Same algorithm, same union rule (a's root is attached to
# b's root), same order (the ledger's), so every component keeps the SAME
# representative node it had before. That matters: new person_ids are handed
# out in the sorted order of those representatives, below.
#
# The accepted links, then the retirements above -- after them, so the
# ledger's own unions, and so every existing representative, come first.
joins <- rbind(accepted[, c("node_a", "node_b", "decided_by"), drop = FALSE],
               pg_retired)
root <- seq_along(linked)
pg_find <- function(x) {
  while (root[x] != x) {
    root[x] <<- root[root[x]]
    x <- root[x]
  }
  x
}
if (nrow(joins)) {
  ia <- match(joins$node_a, linked)
  ib <- match(joins$node_b, linked)
  for (k in which(!is.na(ia) & !is.na(ib))) {
    ra <- pg_find(ia[k]); rb <- pg_find(ib[k])
    if (ra != rb) root[ra] <- rb
  }
}
comp_of <- stats::setNames(rep(NA_character_, length(nodes)), nodes)
if (length(linked)) {
  comp_of[linked] <- linked[vapply(seq_along(linked), pg_find, integer(1))]
}
# A node with no links is its own component.
solo <- is.na(comp_of)
comp_of[solo] <- names(comp_of)[solo]

n_persons <- length(unique(comp_of))
pglog("  ", n_persons, " person(s) over ", length(nodes), " identifier(s)")


# ---------------------------------------------------------------------------
# status and decided_by, per identifier
# ---------------------------------------------------------------------------
# status  `undecided` when this identifier has an undecided link to an
#         identifier in a DIFFERENT person -- meaning this person could still
#         gain members. The "different person" qualifier matters: an undecided
#         link whose endpoints some other accepted path already joins cannot
#         change who anyone is, and counting it would mark settled people
#         unsettled.
#
# decided_by  the STRONGEST authority among the accepted links holding this
#         identifier into its person, since any one of them is enough to keep
#         it there. An identifier nobody proposed a link for gets `none`: it
#         stands alone because nothing contests it, not because anyone decided.

status <- stats::setNames(rep("accepted", length(nodes)), nodes)
if (nrow(undecided)) {
  ca <- unname(comp_of[undecided$node_a])
  cb <- unname(comp_of[undecided$node_b])
  cross <- !is.na(ca) & !is.na(cb) & ca != cb
  if (any(cross)) {
    status[unique(c(undecided$node_a[cross], undecided$node_b[cross]))] <-
      "undecided"
  }
}

decided <- stats::setNames(rep("none", length(nodes)), nodes)
if (nrow(joins)) {
  rk <- unname(PG_RANK[joins$decided_by])
  rk[is.na(rk)] <- 0L
  ord <- order(rk)                      # weakest first, so the strongest wins
  # The same "later write wins" as writing node_a then node_b for each link in
  # that order, done in one pass: lay the writes out in sequence and keep each
  # node's LAST one.
  seq_nodes <- as.vector(rbind(joins$node_a[ord], joins$node_b[ord]))
  seq_vals  <- rep(joins$decided_by[ord], each = 2L)
  last <- !duplicated(seq_nodes, fromLast = TRUE) & seq_nodes %in% nodes
  decided[seq_nodes[last]] <- seq_vals[last]
}


# ---------------------------------------------------------------------------
# Inherit person_ids
# ---------------------------------------------------------------------------

prev <- pg_read_csv(PG_PREV_PATH)
prev_id <- character(0)
next_n <- 1L
if (!is.null(prev) && all(c("person_id", "original_id", "identifier_type")
                          %in% names(prev))) {
  # Rebuild the node key the same way it is written out, so the join is on the
  # identifier and not on a person_id that may be about to change.
  back <- stats::setNames(names(PG_ID_TYPE), unname(PG_ID_TYPE))
  prev_nodes <- paste0(unname(back[prev$identifier_type]), ":",
                       prev$original_id)
  prev_id <- stats::setNames(prev$person_id, prev_nodes)
  mx <- suppressWarnings(max(pg_id_num(prev$person_id), na.rm = TRUE))
  if (is.finite(mx)) next_n <- mx + 1L
  pglog("  previous registry: ", nrow(prev), " row(s), ",
        length(unique(prev$person_id)), " person(s); next id ", pg_id(next_n))
} else {
  pglog("  no previous registry -- assigning from ", pg_id(1L))
}

comps <- split(names(comp_of), unname(comp_of))

# Which previously-assigned ids have been torn apart by this run?
events <- data.frame(person_id = character(0), event = character(0),
                     detail = character(0), at_run = character(0),
                     stringsAsFactors = FALSE)
# WHOLE COLUMNS AT A TIME (2026-10-06). These two steps used to loop over every
# person and look each member up by name, which grew with the square of the
# registry and took most of the step's 7+ minutes at 64,000 identifiers. The
# rules are unchanged:
#   * an old id whose members now sit in more than one person has been SPLIT;
#   * a person keeps the lowest-numbered old id its members carried (other than
#     split ones); any others it carried are recorded as superseded;
#   * a person carrying none gets a new id, handed out in the sorted order of
#     the person's representative node -- the same sort() as before, so new
#     ids land on the same people they always would have.
node_all <- names(comp_of)
comp_all <- unname(comp_of)
# (One line: at the top level of a script, an `else` that starts a new line is
# a syntax error.)
pid_all  <- if (length(prev_id)) unname(prev_id[node_all]) else rep(NA_character_, length(node_all))

split_ids <- character(0)
if (length(prev_id)) {
  has <- !is.na(pid_all)
  landed <- unique(data.frame(pid = pid_all[has], comp = comp_all[has],
                              stringsAsFactors = FALSE))
  n_comp <- table(landed$pid)
  split_ids <- sort(names(n_comp)[n_comp > 1L])   # ls() order, as before
}

run_stamp <- format(Sys.Date())
comp_order <- sort(names(comps))
ok <- !is.na(pid_all) & !(pid_all %in% split_ids)
carried <- unique(data.frame(comp = comp_all[ok], pid = pid_all[ok],
                             stringsAsFactors = FALSE))
carried <- carried[order(match(carried$comp, comp_order),
                         pg_id_num(carried$pid), seq_len(nrow(carried))), ,
                   drop = FALSE]
first <- !duplicated(carried$comp)
keep_of <- stats::setNames(carried$pid[first], carried$comp[first])

no_carry <- comp_order[!comp_order %in% names(keep_of)]
new_ids <- if (length(no_carry))
  pg_id(seq.int(next_n, length.out = length(no_carry))) else character(0)
next_n <- next_n + length(no_carry)
assigned <- c(keep_of, stats::setNames(new_ids, no_carry))

# A merge keeps the lowest-numbered id; the others are recorded as superseded
# rather than deleted, so a historical reference still resolves. Same order as
# before: by person, then by id number.
sup <- carried[!first, , drop = FALSE]
if (nrow(sup)) {
  events <- rbind(events, data.frame(
    person_id = sup$pid, event = "superseded",
    detail = unname(keep_of[sup$comp]), at_run = run_stamp,
    stringsAsFactors = FALSE))
}
for (pid in split_ids) {
  events <- rbind(events, data.frame(
    person_id = pid, event = "split",
    detail = "members reassigned to new person_ids", at_run = run_stamp,
    stringsAsFactors = FALSE))
}


# ---------------------------------------------------------------------------
# Model scores per identifier (2026-10-31)
# ---------------------------------------------------------------------------
# The link classifier scores LINKS; the registry has one row per IDENTIFIER, so
# each identifier gets the LOWEST predicted surety among the relevant links
# touching it -- the weakest link is the one most likely to be wrong.
#
#   person_registry.csv                   over the MODEL-ACCEPTED links that
#                                         hold the identifier in its person
#   person_registry_with_candidates.csv   over EVERY link touching it that the
#                                         model scored tonight, accepted or
#                                         left for review
#
# Blank where no such link exists -- including wherever humans decided, since
# the model only scores links nobody has voted on. Read from tonight's
# decisions file, and only if the model is switched on and that file was made
# for the current candidate run (the ledger applies the same test).
pg_scores <- local({
  none <- stats::setNames(numeric(0), character(0))
  md <- if (isTRUE(as.logical(match_param("model.enabled"))))
    pg_read_csv(file.path(PG_REGISTRY_DIR, "model_decisions",
                          "model_decisions_latest.csv")) else NULL
  stem_now <- tryCatch(read_latest("person_candidates")$stem, error = function(e) NULL)
  if (!is.null(md) && (!all(c("link_id", "predicted_surety", "model_version",
                              "candidate_run") %in% names(md)) ||
                       is.null(stem_now) || !isTRUE(all(md$candidate_run == stem_now))))
    md <- NULL
  if (is.null(md) || !nrow(md) || !"link_id" %in% names(ledger))
    return(list(official = none, candidate = none, version = ""))
  score_of <- stats::setNames(suppressWarnings(as.numeric(md$predicted_surety)),
                              md$link_id)
  node_min <- function(links) {
    s <- unname(score_of[links$link_id])
    ok <- !is.na(s)
    if (!any(ok)) return(none)
    v <- tapply(c(s[ok], s[ok]), c(links$node_a[ok], links$node_b[ok]), min)
    stats::setNames(as.numeric(v), names(v))
  }
  list(official  = node_min(ledger[ledger$link_status == "accepted" &
                                     ledger$decided_by == "model", , drop = FALSE]),
       candidate = node_min(ledger[ledger$link_status %in% c("accepted", "undecided"), ,
                                   drop = FALSE]),
       version   = paste(unique(md$model_version), collapse = ";"))
})
pg_score_cols <- function(scores, nds) {
  v <- round(unname(scores[nds]), 4)
  list(surety = v, version = ifelse(is.na(v), "", pg_scores$version))
}
pglog("  model scores: ", length(pg_scores$official), " identifier(s) held by a ",
      "model-accepted link; ", length(pg_scores$candidate), " touched by a ",
      "model-scored link", if (nzchar(pg_scores$version))
        paste0(" (", pg_scores$version, ")") else "")


# ---------------------------------------------------------------------------
# Write
# ---------------------------------------------------------------------------

.pg_off <- pg_score_cols(pg_scores$official, nodes)
registry <- data.frame(
  person_id = unname(assigned[unname(comp_of[nodes])]),
  identifier_type = unname(PG_ID_TYPE[sub(":.*$", "", nodes)]),
  original_id = sub("^[^:]*:", "", nodes),
  status = unname(status[nodes]),
  decided_by = unname(decided[nodes]),
  first_seen_run = run_stamp,
  last_seen_run = run_stamp,
  model_surety = .pg_off$surety,
  model_version = .pg_off$version,
  stringsAsFactors = FALSE
)
if (!is.null(prev) && "first_seen_run" %in% names(prev)) {
  i <- match(paste0(registry$identifier_type, "|", registry$original_id),
             paste0(prev$identifier_type, "|", prev$original_id))
  keep <- !is.na(i)
  registry$first_seen_run[keep] <- prev$first_seen_run[i[keep]]
}
registry <- registry[order(pg_id_num(registry$person_id),
                           registry$identifier_type,
                           registry$original_id), , drop = FALSE]
pg_write_csv(registry[, PG_COLUMNS, drop = FALSE], PG_OUT_PATH)

if (nrow(events)) {
  old_events <- pg_read_csv(PG_PREV_HISTORY_PATH)
  if (!is.null(old_events)) events <- rbind(old_events, events)
  pg_write_csv(events, PG_HISTORY_PATH)
}


# ---------------------------------------------------------------------------
# Companion copy: the registry WITH the algorithm's undecided proposals
# (2026-10-14)
# ---------------------------------------------------------------------------
# person_registry.csv joins only on accepted links. This copy also joins on
# `undecided` ones -- what the algorithm proposed and no one has settled yet --
# so the "if every proposal were right" grouping can be looked at alongside the
# official one. Rejected and inactive links still join nothing.
#
# READ-ONLY VIEW, NOT A JOIN KEY. It has no id lifecycle of its own:
#   candidate_person_id  the LOWEST official person_id in the candidate group.
#                        It moves whenever proposals or decisions change, and
#                        it is never written into the exports.
#   person_id ...        the official registry's columns, unchanged.
#   human_reviewed       TRUE when every link touching the group (accepted,
#                        rejected or still undecided; inactive ignored) is
#                        settled -- accepted or rejected -- by a reviewer, a
#                        harmoniser OR the automatic rules (auto_rule:
#                        auto-accepted clusters and same-ID links;
#                        pipeline_drop), and there is at least one such link.
#                        Automatic decisions count because those links are
#                        never sent to a person: they are as settled as they
#                        will ever be (2026-10-14).
#                        FALSE if any link is still undecided (including a
#                        harmoniser's unsure / escalate), or if nothing ever
#                        proposed a match.
#
# Same confidentiality class as the registry: identifiers only, no names.
PG_CAND_PATH <- file.path(dirname(PG_OUT_PATH),
                          "person_registry_with_candidates.csv")
PG_SETTLED_BY <- c("reviewer", "harmoniser", "auto_rule", "pipeline_drop")

cand_links <- ledger[ledger$link_status %in% c("accepted", "undecided"), ,
                     drop = FALSE]
# Retirements join here too: a retired ID is always its replacement's person.
cand_links <- rbind(cand_links[, c("node_a", "node_b"), drop = FALSE],
                    pg_retired[, c("node_a", "node_b"), drop = FALSE])
root <- seq_along(linked)            # fresh union-find; pg_find() reuses it
if (nrow(cand_links)) {
  ia <- match(cand_links$node_a, linked)
  ib <- match(cand_links$node_b, linked)
  for (k in which(!is.na(ia) & !is.na(ib))) {
    ra <- pg_find(ia[k]); rb <- pg_find(ib[k])
    if (ra != rb) root[ra] <- rb
  }
}
cgrp_of <- stats::setNames(names(comp_of), names(comp_of))   # solo by default
if (length(linked)) {
  cgrp_of[linked] <- linked[vapply(seq_along(linked), pg_find, integer(1))]
}

# Candidate id = lowest official person_id among the group's members.
reg_node <- paste0(unname(stats::setNames(names(PG_ID_TYPE),
                                          unname(PG_ID_TYPE))[
  registry$identifier_type]), ":", registry$original_id)
grp_vec <- unname(cgrp_of[reg_node])
grp_min <- tapply(pg_id_num(registry$person_id), grp_vec, min)
cand_pid <- pg_id(unname(grp_min[grp_vec]))

# human_reviewed, per group. Each non-inactive link is charged to the groups
# of BOTH its ends (the same group for an accepted/undecided link; two groups
# for a rejected one).
rel <- ledger[ledger$link_status %in% c("accepted", "rejected", "undecided"), ,
              drop = FALSE]
grp_human <- stats::setNames(rep(NA, length(unique(grp_vec))),
                             unique(grp_vec))   # NA = no link seen yet
if (nrow(rel)) {
  settled <- rel$link_status %in% c("accepted", "rejected") &
    rel$decided_by %in% PG_SETTLED_BY
  g <- c(unname(cgrp_of[rel$node_a]), unname(cgrp_of[rel$node_b]))
  s <- c(settled, settled)
  ok <- !is.na(g)
  all_human <- tapply(s[ok], g[ok], all)
  grp_human[names(all_human)] <- all_human
}
grp_human[is.na(grp_human)] <- FALSE
human_reviewed <- ifelse(unname(grp_human[grp_vec]), "TRUE", "FALSE")

cand_registry <- data.frame(candidate_person_id = cand_pid,
                            registry[, PG_COLUMNS, drop = FALSE],
                            human_reviewed = human_reviewed,
                            stringsAsFactors = FALSE)
# The candidate view's model columns cover every model-SCORED link touching
# the identifier, not only accepted ones (see "Model scores per identifier").
.pg_cand <- pg_score_cols(pg_scores$candidate, reg_node)
cand_registry$model_surety  <- .pg_cand$surety
cand_registry$model_version <- .pg_cand$version
cand_registry <- cand_registry[order(pg_id_num(cand_registry$candidate_person_id),
                                     pg_id_num(cand_registry$person_id),
                                     cand_registry$identifier_type,
                                     cand_registry$original_id), , drop = FALSE]
pg_write_csv(cand_registry, PG_CAND_PATH)


# ---------------------------------------------------------------------------
# person_id into the current exports (2026-10-07)
# ---------------------------------------------------------------------------
# So that anyone counting PEOPLE rather than records can do it from the export
# itself, without joining this registry. Added to the CURRENT exports only --
# the latest linelist, the latest contact-tracing export and the combined
# contact-tracing files -- in both the _names and the _ctsafe copies.
# person_id is pseudonymous (no personal information), which is why it may go
# in the ctsafe files and on to the analysis repository.
#
#   linelist          person_id, person_id_status           <- alert_id
#   contact tracing   contact_person_id, ..._status         <- contact_id
#                     source_case_person_id, ..._status     <- source_case_alert_id
#
# `_status` is the registry's status: `accepted` when nothing pending could
# change the grouping, `undecided` when a link still under review could.
# A blank person_id means the identifier is not in the registry (blank id).
#
# Refreshed on EVERY registry build, so the columns follow the latest
# decisions. person_ids can change (merges keep the lowest id, splits retire
# one): person_registry_history.csv resolves an old id. Each column sits right
# after the identifier it was looked up from. A file whose columns are already
# right is left untouched. Skipped in comparison mode (BDBV_REGISTRY_OUT_DIR),
# which must never touch live files.
PG_BACK <- stats::setNames(names(PG_ID_TYPE), unname(PG_ID_TYPE))
pg_reg_key <- paste0(unname(PG_BACK[registry$identifier_type]), ":",
                     registry$original_id)

# specs: named list, new column -> c(id = <identifier column>, prefix = <node prefix>)
pg_stamp_person_ids <- function(path, specs) {
  if (!file.exists(path)) return(NA)
  d <- tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         check.names = FALSE),
                error = function(e) NULL)
  if (is.null(d)) {
    pglog("  WARNING: could not read ", path, " -- person_id not added")
    return(NA)
  }
  changed <- FALSE
  for (nc in names(specs)) {
    idc <- specs[[nc]][["id"]]
    if (!idc %in% names(d)) next
    ids <- trimws(as.character(d[[idc]]))
    j <- match(paste0(specs[[nc]][["prefix"]], ":", ids), pg_reg_key)
    ok <- !is.na(ids) & nzchar(ids) & !is.na(j)
    pid <- ifelse(ok, registry$person_id[j], "")
    pst <- ifelse(ok, registry$status[j], "")
    stc <- paste0(nc, "_status")
    cur_p <- if (nc %in% names(d)) d[[nc]] else NULL
    cur_s <- if (stc %in% names(d)) d[[stc]] else NULL
    same <- !is.null(cur_p) && !is.null(cur_s) &&
      identical(ifelse(is.na(cur_p), "", as.character(cur_p)), pid) &&
      identical(ifelse(is.na(cur_s), "", as.character(cur_s)), pst)
    if (same) next
    d[[nc]] <- pid
    d[[stc]] <- pst
    # Place the pair right after the identifier column.
    rest <- setdiff(names(d), c(nc, stc))
    at <- match(idc, rest)
    d <- d[, c(rest[seq_len(at)], nc, stc,
               rest[setdiff(seq_along(rest), seq_len(at))]), drop = FALSE]
    changed <- TRUE
  }
  if (changed) pg_write_csv(d, path)
  changed
}

if (nzchar(.pg_out_dir)) {
  pglog("  comparison mode (BDBV_REGISTRY_OUT_DIR): person_id NOT written ",
        "into the exports")
} else {
  LL_SPEC <- list(person_id = c(id = "alert_id", prefix = "ll"))
  CT_SPEC <- list(contact_person_id     = c(id = "contact_id", prefix = "ct_contact"),
                  source_case_person_id = c(id = "source_case_alert_id",
                                            prefix = "ct_case"))
  targets <- list()
  if (!is.null(epi_dir)) {
    for (f in c("dhis2_processed_linelist_names.csv",
                "dhis2_processed_linelist_ctsafe.csv"))
      targets[[length(targets) + 1L]] <- list(path = file.path(epi_dir, f),
                                              spec = LL_SPEC)
  }
  ct_latest_dir <- pg_latest_folder(PG_CT_DIR, "contact_tracing")
  if (!is.null(ct_latest_dir)) {
    for (f in c("contact_tracing_processed_names.csv",
                "contact_tracing_processed_ctsafe.csv"))
      targets[[length(targets) + 1L]] <- list(path = file.path(ct_latest_dir, f),
                                              spec = CT_SPEC)
  }
  ct_comb_dir <- file.path(PG_CT_DIR, "combined")
  for (f in c("contact_tracing_combined_names.csv",
              "contact_tracing_combined_ctsafe.csv",
              "contact_tracing_combined_contacts.csv"))
    targets[[length(targets) + 1L]] <- list(path = file.path(ct_comb_dir, f),
                                            spec = CT_SPEC)

  pglog("")
  pglog("  person_id in the current exports:")
  for (t in targets) {
    r <- pg_stamp_person_ids(t$path, t$spec)
    if (!is.na(r))
      pglog("    ", if (r) "updated  " else "unchanged", "  ",
            sub(paste0(repo_root, "/"), "", t$path, fixed = TRUE))
  }
}


# ---------------------------------------------------------------------------
# What it says
# ---------------------------------------------------------------------------

sz <- table(table(registry$person_id))
pglog("")
pglog("  person sizes:")
for (k in names(sz)) {
  pglog(sprintf("    %2s identifier(s)  %6d person(s)", k, sz[[k]]))
}
pglog("")
pglog("  by identifier type:")
tt <- table(registry$identifier_type, registry$status)
for (ty in rownames(tt)) {
  pglog(sprintf("    %-22s %6d accepted  %6d undecided", ty,
                if ("accepted" %in% colnames(tt)) tt[ty, "accepted"] else 0L,
                if ("undecided" %in% colnames(tt)) tt[ty, "undecided"] else 0L))
}
pglog("")
pglog("  held together by:")
for (d in names(sort(table(registry$decided_by), decreasing = TRUE))) {
  pglog(sprintf("    %-14s %6d identifier(s)", d,
                sum(registry$decided_by == d)))
}
if (nrow(events)) {
  pglog("")
  pglog("  ", sum(events$at_run == run_stamp), " id lifecycle event(s) this run")
}
pglog("")
pglog("  with undecided proposals included: ",
      length(unique(cand_registry$candidate_person_id)), " candidate person(s); ",
      sum(cand_registry$human_reviewed == "TRUE"), " of ", nrow(cand_registry),
      " identifier(s) in fully settled groups (human or automatic)")
pglog("  ", nrow(cand_registry), " row(s) in ", PG_CAND_PATH)
pglog("")
pglog("  ", nrow(registry), " row(s) in ", PG_OUT_PATH)
write_log_file(.pg_log, "person_registry_log.txt")

person_registry_complete <- TRUE
