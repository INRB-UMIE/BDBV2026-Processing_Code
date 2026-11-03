#!/usr/bin/env Rscript
# 3-build-link-ledger.R
#
# The running record of what has been decided about every proposed link.
#
# One row per LINK -- an unordered pair of identifiers some run proposed might
# be the same person -- carrying what was decided, who decided it, and on what
# basis. It is the successor to id_lookup_table.csv, which was one row per
# person and so could not express "two reviewers disagreed" or "a rule nobody
# checked accepted this".
#
# Output: data/processed/person_matching/link_ledger.csv   (single running file)
#
# Specified in specs/2026-09-10-link-ledger-after-harmonisation.md. It fills the
# gap left by 2026-09-03-person-id-entity-resolution.md, whose
# `person_identifiers.decision_ref` is described as "points at the reviewed pair
# that put it here" without defining what it points at.
#
# ---------------------------------------------------------------------------
# ONE FILE, NOT ONE PER RUN
# ---------------------------------------------------------------------------
# It lives at the top of person_matching/ rather than inside a run folder,
# because the decisions are about people and not about a build. A run folder is
# a snapshot of what the matcher proposed on one export; the ledger is what
# humans concluded, and those conclusions have to outlive the build that
# prompted them or every new export re-asks the same questions.
#
# ---------------------------------------------------------------------------
# WHY REJECTIONS ARE KEPT, NOT DELETED
# ---------------------------------------------------------------------------
# The obvious design drops rejected links so the table reads as the live set.
# Measured on LINELIST_01092026 x CONTACT-TRACING_01092026, that quietly fails:
# of the links a reviewer broke, 6-10% have their two endpoints STILL CONNECTED
# by another surviving link in the same cluster. Delete the row and transitive
# closure reunites the pair anyway -- the reviewer's decision reversed by an
# algorithm, silently, on about one rejection in twelve.
#
# So a rejection is a CANNOT-LINK CONSTRAINT that component building must
# honour, not an absence. `ll_zombie_rejections()` below measures the same
# quantity on every run so the number is never assumed again.
#
# Two more reasons, both already argued in the repo:
#   * a rejection is not recoverable from the data -- `timeline_impossible` is
#     a judgement, and a better matcher re-proposes the pair, so the team
#     re-adjudicates its hardest decisions;
#   * the churn spec requires append-only. A table rebuilt from the current
#     export loses a member every time DHIS2 reissues an ID.
#
# ---------------------------------------------------------------------------
# CONFIDENTIALITY
# ---------------------------------------------------------------------------
# This file holds IDENTIFIERS AND DECISIONS, NO NAMES. That is deliberate and
# worth preserving: it puts the ledger in the same class as
# person_candidate_node_dates.csv rather than person_candidate_edges.csv, and
# this is the file most likely to be handed onward. Names are one join away in
# the candidate build for anyone who needs them. DO NOT add name_a / name_b.
#
# It is still patient data -- an alert_id encodes province and health zone --
# and stays inside person_matching/ with the rest.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

# data/processed/person_matching/ (person_matching_dir() in 0-setup.R): the
# ledger at the top, one folder per run below it.
LL_REGISTRY_DIR  <- person_matching_dir()
LL_LEDGER_PATH   <- file.path(LL_REGISTRY_DIR, "link_ledger.csv")

# NO BUCKET LISTS ANY MORE. 2-build-person-candidates.R writes a single
# `auto_accept` column, which it derives from the cluster's weakest link score
# and the confirmed-case / non-case veto. There is nothing left for this
# script to mirror and nothing to validate: a logical column is either TRUE or
# it is not, where a bucket string could be one this script had never heard of.
# ll_check_buckets() existed for that risk and is gone with it.
#
# THE DROPPED BUCKETS ARE GONE TOO, and that is a real change in behaviour.
# X1 and X2 used to reject links outright -- 934 clusters on the last build
# before this. Under the score there is one cutoff: a link below
# link_score.min_keep never reaches this file at all, and everything that does
# reach it is either accepted or left undecided for a human. Nothing is
# rejected by rule any more, so `pipeline_drop` is no longer emitted on a
# fresh build. It stays in LL_RANK below because the ledger is CUMULATIVE:
# rows written by earlier runs still carry it, and dropping it from the
# precedence table would make those rows unrankable.

# Precedence, as ranks. A decision is only ever REPLACED by one of equal or
# higher authority, which is what makes the ledger safe to rebuild on every run
# (see ll_merge()).
#
# `auto_rule` and `pipeline_drop` are AUTHORITY LEVELS, not bucket names --
# they say who decided, not which rule. They survive the buckets unchanged,
# and they have to: 4-build-person-registry.R grades every identifier by
# `decided_by` through its own PG_RANK, so an identifier whose link was
# accepted by rule would silently fall to rank 0 if this vocabulary changed.
LL_RANK <- c(none = 0L, model = 1L, pipeline_drop = 2L, auto_rule = 2L,
             reviewer = 3L, harmoniser = 4L)

# THE LINK CLASSIFIER (`model`, rank 1, added 2026-10-31). Below every human
# and below the rules: it only ever decides a link nobody else has.
#
# Its decisions are RE-APPLIED FROM SCRATCH ON EVERY RUN, never carried
# forward: every model row is reset below with the other recomputed rows, and
# only the decisions in TONIGHT's file (written by the predict step for THIS
# candidate run) are applied. So a link whose evidence changes and whose new
# prediction falls under the cut-off goes back to undecided by itself, and
# switching the model off (matching.model.enabled = 0) withdraws every model
# decision on the next run.
#
# Applied only to a link that is undecided, proposed by this run, has no
# harmoniser verdict, and whose reviewer votes -- if any -- ALL agree with it
# (kept, cluster confirmed; e.g. an audit "same person"). Any dissenting or
# unsure vote leaves the link with humans (2026-11-01 audit rule). Only
# "accept" decisions are applied; the model never rejects.
LL_MODEL_ENABLED <- isTRUE(as.logical(match_param("model.enabled")))
LL_MODEL_DECISIONS <- file.path(LL_REGISTRY_DIR, "model_decisions",
                                "model_decisions_latest.csv")

# Recorded in decision_basis so a ledger row says what it was accepted on.
LL_AUTO_ACCEPT_SCORE <- match_param("link_score.auto_accept")

# How many CONFIRMED votes, all making the same call, decide a link without a
# harmoniser (rule 3 below). Votes in clusters marked unsure are not counted;
# any confirmed vote the other way sends the link to the Harmonise tab. (A
# single reviewer is never authoritative.)
LL_CONSENSUS_MIN <- as.integer(match_param("ledger.consensus_min_reviewers"))

LL_COLUMNS <- c(
  "link_id", "node_a", "node_b", "id_type_a", "id_type_b",
  "link_status", "decided_by", "decision_basis", "decision_reasons",
  "decided_by_name", "decided_on",
  "reviewers_seen", "reviewers_kept", "reviewers_broke", "reviewers_agreed",
  # `link_class` and `tier` retired with the buckets and tiers they described
  # (tier dropped 2026-10-03); `link_score` is what decides a link now.
  "link_score", "link_type", "name_weight", "n_corroborators",
  "n_corroborators_available",
  "last_cluster_id", "first_seen_run", "last_seen_run",
  # The date of the most recent run that proposed this link (YYYY-MM-DD), so
  # the ledger can be sorted and filtered by it without parsing run names.
  "last_proposed_date",
  # How many DISTINCT runs have proposed it. Counted once per run, not once
  # per ledger rebuild: rebuilding on the same run (harmonising does this)
  # leaves it alone.
  "runs_seen"
)


# -------------------------  Helper Functions  ---------------------------------

.ll_log <- character(0)
lllog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .ll_log <<- c(.ll_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

ll_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

ll_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                    colClasses = "character"),
           error = function(e) NULL)
}

# A run's date: the latest DDMMYYYY in its name, as YYYY-MM-DD. A run is named
# after the exports it was built from (LINELIST_02102026__CT-COMBINED_02102026),
# and the newer of the two is when the data it proposed from was current.
ll_run_date <- function(stem) {
  vapply(as.character(stem), function(s) {
    m <- regmatches(s, gregexpr("[0-9]{8}", s))[[1]]
    d <- suppressWarnings(as.Date(m, format = "%d%m%Y"))
    d <- d[!is.na(d)]
    if (length(d)) format(max(d), "%Y-%m-%d") else ""
  }, character(1), USE.NAMES = FALSE)
}

ll_blank_to_empty <- function(d, cols) {
  for (cl in cols) {
    if (!cl %in% names(d)) d[[cl]] <- ""
    d[[cl]][is.na(d[[cl]])] <- ""
  }
  d
}

# The identity of a link: the unordered pair of identifiers it joins.
#
# NOT edge_id, which is a row number assigned after sorting and changes between
# builds. Joining three people's decisions on a row number lines them up by
# position and attributes each to the wrong link. The node pair is the one
# thing about a link that cannot change without it being a different link, and
# it is already the key the review and harmonise tabs merge on.
# In byte order, the same on every machine (node_pair_key() in 0-setup.R).
ll_link_id <- function(node_a, node_b) node_pair_key(node_a, node_b, "|")

# Which candidate run to read. The ledger is cumulative, so this is only ever
# "which build is proposing links today".
ll_latest_run <- function() {
  info <- read_latest("person_candidates")
  if (!is.null(info) && !is.null(info$stem) &&
      dir.exists(pm_run_dir(info$stem, "candidates"))) {
    return(info$stem)
  }
  d <- list.dirs(LL_REGISTRY_DIR, full.names = FALSE, recursive = FALSE)
  d <- d[nzchar(d) & file.exists(file.path(LL_REGISTRY_DIR, d, "candidates",
                                           "person_candidate_edges.csv"))]
  if (!length(d)) return(NULL)
  d[[length(d)]]
}

# ll_check_buckets() removed with the buckets it validated. Its job was to
# stop the run if the pipeline wrote a bucket string this script had no rule
# for; a logical column cannot be unrecognised, so there is nothing to check.
#
# What replaced it is the guard below, at the point of use: the auto_accept
# column must EXIST. A candidate build written before 2026-09-23 has a
# review_bucket column instead, and reading a missing column in R gives NULL
# rather than an error -- every link would silently come out undecided.


# ---------------------------------------------------------------------------
# Read the current candidate build
# ---------------------------------------------------------------------------

stem <- ll_latest_run()
if (is.null(stem)) {
  stop("no candidate run found under ", LL_REGISTRY_DIR,
       " -- run 2-build-person-candidates.R first", call. = FALSE)
}
run_dir     <- pm_run_dir(stem, "candidates")   # what the pipeline built
lllog("Link ledger")
lllog("  candidate run: ", stem)

edges <- ll_read_csv(file.path(run_dir, "person_candidate_edges.csv"))
clusters <- ll_read_csv(file.path(run_dir, "person_candidate_clusters.csv"))
if (is.null(edges) || is.null(clusters)) {
  stop("candidate run ", stem, " has no edges or clusters", call. = FALSE)
}
edges$link_id <- ll_link_id(edges$node_a, edges$node_b)

if (!"auto_accept" %in% names(clusters)) {
  stop("candidate run ", stem, " has no `auto_accept` column. Builds before ",
       "2026-09-23 carry `review_bucket` instead, and this script no longer ",
       "reads it -- re-run 2-build-person-candidates.R. Stopping rather than ",
       "continuing, because a missing column reads as NULL in R and every ",
       "link would come out undecided with no error.", call. = FALSE)
}
auto_of <- stats::setNames(as.logical(clusters$auto_accept), clusters$cluster_id)
edge_auto <- unname(auto_of[as.character(edges$cluster_id)])
edge_auto[is.na(edge_auto)] <- FALSE

lllog("  ", nrow(edges), " link(s) proposed across ", nrow(clusters),
      " cluster(s)")


# ---------------------------------------------------------------------------
# Classify each link (the algorithm's part)
# ---------------------------------------------------------------------------
# Precedence:  harmoniser > reviewer consensus > auto_rule / pipeline_drop >
# nothing.
#
# This section decides only what the ALGORITHM says about the links this run
# proposes. Human work is applied afterwards, to the WHOLE ledger, from
# reviewer_history/ and harmoniser_history/ (see "Apply human decisions"
# below): those folders belong to no run, so a vote cast on an older run, or
# on a link this run no longer proposes, still counts.
#
#   AUTO-ACCEPTED LINKS ENTER AS `accepted`, FLAGGED. decided_by = auto_rule,
#   so a consumer can filter to human-decided links whenever that matters.

n <- nrow(edges)
link_status <- rep("undecided", n)
decided_by  <- rep("none", n)
basis       <- rep("awaiting_review", n)
reasons     <- rep("", n)
by_name     <- rep("", n)

# --- the algorithm's own decisions ---
# One rule now, where there were five buckets. `decision_basis` records the
# score the cluster was accepted on rather than a bucket name, so a row in the
# ledger says how strong the claim was, not which category it fell in.
is_auto <- edge_auto
link_status[is_auto] <- "accepted"
decided_by[is_auto]  <- "auto_rule"
basis[is_auto]       <- paste0("auto_accept>=", LL_AUTO_ACCEPT_SCORE)

# SAME-ID LINKS ARE ACCEPTED ON THEIR OWN (2026-10-07). A contact-tracing
# source case and a linelist case with the same alert_id are, nearly always,
# the same case cited twice. They used to be accepted only when their cluster
# held nothing else; if the case also had a name-based link to anyone, the
# whole cluster went to review and the same-ID link waited there undecided --
# 157 of 771 source cases had a different person_id from their own linelist
# record. Now each such link is accepted by itself, whatever else is in the
# cluster (which still goes to review for its other links).
#
# Still challengeable: a reviewer vote to break one contradicts the rule, and
# rule 4 under "Apply human decisions" then sends it to the harmoniser.
is_exact <- !is_auto & edges$link_type == "exact_source_alert"
link_status[is_exact] <- "accepted"
decided_by[is_exact]  <- "auto_rule"
basis[is_exact]       <- "auto_exact_id"

# NOTHING IS REJECTED BY RULE ANY MORE. X1 and X2 used to reject 934 clusters
# here. The score does that work earlier and more bluntly: a link below
# link_score.min_keep is never written by the matcher, so it never reaches
# this file to be rejected. What arrives is accepted or left for a human.
#
# `pipeline_drop` therefore stops appearing on fresh builds, but rows carrying
# it from earlier runs are NOT rewritten -- the ledger is cumulative and a
# rejection is a cannot-link constraint that a later build has no grounds to
# lift. They keep their status and their old basis string.

current <- data.frame(
  link_id = edges$link_id,
  node_a = edges$node_a, node_b = edges$node_b,
  id_type_a = edges$id_type_a, id_type_b = edges$id_type_b,
  link_status = link_status, decided_by = decided_by,
  decision_basis = basis, decision_reasons = reasons,
  decided_by_name = by_name, decided_on = "",
  # Filled in for every ledger row by "Apply human decisions" below.
  reviewers_seen = "0", reviewers_kept = "0", reviewers_broke = "0",
  reviewers_agreed = "",
  link_score = if ("link_score" %in% names(edges)) edges$link_score else NA_real_,
  link_type = if ("link_type" %in% names(edges)) edges$link_type else "",
  name_weight = edges$name_weight,
  n_corroborators = edges$n_corroborators,
  n_corroborators_available =
    if ("n_corroborators_available" %in% names(edges))
      edges$n_corroborators_available else "",
  last_cluster_id = edges$cluster_id,
  first_seen_run = stem, last_seen_run = stem,
  last_proposed_date = ll_run_date(stem), runs_seen = "1",
  stringsAsFactors = FALSE
)


# ---------------------------------------------------------------------------
# Merge onto the running ledger
# ---------------------------------------------------------------------------
# The property that makes this safe to run on every export:
#
#   A DECISION IS ONLY EVER REPLACED BY ONE OF EQUAL OR HIGHER AUTHORITY.
#
# Without it, a rebuild that knew nothing of earlier decisions would
# recompute every link as `awaiting_review` and wipe months of adjudication.
# With it, the worst such a run can do is refresh the evidence columns.
#
# Equal rank replaces, so a harmoniser can change their mind and a rebuild that
# moves a cluster from auto-accepted to dropped is honoured -- in both cases
# nothing human is lost.

prev <- ll_read_csv(LL_LEDGER_PATH)
if (is.null(prev)) {
  lllog("  no existing ledger -- creating ", basename(LL_LEDGER_PATH))
  ledger <- current[, LL_COLUMNS, drop = FALSE]
  n_new <- nrow(current); n_kept_old <- 0L; n_upgraded <- 0L; n_absent <- 0L
} else {
  prev <- ll_blank_to_empty(prev, LL_COLUMNS)
  i <- match(current$link_id, prev$link_id)
  seen_before <- !is.na(i)

  rank_new <- unname(LL_RANK[current$decided_by])
  rank_old <- rep(-1L, nrow(current))
  rank_old[seen_before] <- unname(LL_RANK[prev$decided_by[i[seen_before]]])
  rank_old[is.na(rank_old)] <- 0L

  keep_old <- seen_before & rank_old > rank_new
  n_upgraded <- sum(seen_before & !keep_old &
                    (current$decided_by != prev$decided_by[i] |
                     current$link_status != prev$link_status[i]), na.rm = TRUE)

  # Where the old decision wins, carry its five decision columns forward onto
  # the fresh evidence: the link's scores are whatever the CURRENT
  # build says, while what was concluded about it is whatever was concluded.
  for (cl in c("link_status", "decided_by", "decision_basis",
               "decision_reasons", "decided_by_name", "decided_on")) {
    current[[cl]][keep_old] <- prev[[cl]][i[keep_old]]
  }
  # Reviewer counts always come from the current build, since they describe
  # this run's files -- except where the link is not in this build at all.
  current$first_seen_run[seen_before] <- prev$first_seen_run[i[seen_before]]
  # One more run only if this is a DIFFERENT run from the one that last
  # proposed it. A rebuild on the same run (harmonising triggers one) used to
  # add 1 every time, so the count measured ledger rebuilds, not runs.
  prev_runs <- suppressWarnings(as.integer(prev$runs_seen[i[seen_before]]))
  prev_runs[is.na(prev_runs)] <- 0L
  new_run <- prev$last_seen_run[i[seen_before]] != stem
  current$runs_seen[seen_before] <- as.character(prev_runs + as.integer(new_run))

  # Links the current build no longer proposes are RETAINED, untouched, with
  # last_seen_run left where it was. This is the churn requirement: an
  # identifier that leaves the export must not take the decisions about it with
  # it, or every reissued DHIS2 id costs a re-review.
  gone <- prev[is.na(match(prev$link_id, current$link_id)), , drop = FALSE]
  # A ledger written before last_proposed_date existed: derive it from the run.
  fill <- !nzchar(gone$last_proposed_date)
  gone$last_proposed_date[fill] <- ll_run_date(gone$last_seen_run[fill])
  n_absent <- nrow(gone)
  n_new <- sum(!seen_before)
  n_kept_old <- sum(keep_old)
  ledger <- rbind(current[, LL_COLUMNS, drop = FALSE],
                  gone[, LL_COLUMNS, drop = FALSE])
}


# ---------------------------------------------------------------------------
# Apply human decisions, to EVERY ledger row
# ---------------------------------------------------------------------------
# Votes and verdicts live in person_matching/reviewer_history/ and
# harmoniser_history/, one file per person, rows only ever appended; a
# person's LAST row on a link is the one that counts (read_history_latest() in
# 0-setup.R). Neither folder belongs to a run, so they are applied to the
# whole ledger -- including links this run no longer proposes -- and nothing a
# person did is stranded when new data produces a newer run.
#
# Recomputed from the files on every build, so the result never depends on
# what the ledger said last time about anything a person can change:
#
#   1. A harmoniser's verdict (harmoniser_history) decides the link. `settled`
#      accepts or rejects it; `unsure` / `escalate` leave it undecided, flagged.
#   2. Else a harmoniser decision already in the ledger stands (imported ones,
#      e.g. from the old matcher, have no history file behind them).
#   3. Else reviewer consensus decides it (decided_by = reviewer, basis =
#      reviewer_consensus): at least LL_CONSENSUS_MIN reviewers voted on the
#      link with the cluster marked CONFIRMED, all made the same call, and no
#      other confirmed vote says the opposite. Votes in a cluster marked
#      UNSURE are left out entirely -- they neither count towards the bar nor
#      block it -- but they still show in reviewers_* and in the Harmonise tab.
#   4. Else any votes leave it `awaiting_harmonisation` -- except an
#      algorithm decision that every vote agrees with, which stands.
#   5. Else it is whatever the algorithm says this run.
#   6. A link still undecided that this run does not propose is `inactive`:
#      kept, but no longer under consideration.

ll_history_date <- function(x) substr(x, 1, 10)

# The newest dated model_decisions_<YYYY-MM-DD>.csv from BEFORE today (UTC), or
# NA. Used to report how the model's acceptances changed since the last day.
ll_prev_model_file <- function(dir) {
  f <- list.files(dir, pattern = "^model_decisions_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv$")
  d <- as.Date(sub("^model_decisions_(.*)\\.csv$", "\\1", f), format = "%Y-%m-%d")
  ok <- !is.na(d) & d < as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
  if (!any(ok)) return(NA_character_)
  file.path(dir, f[ok][which.max(d[ok])])
}

auto_ids <- current$link_id[current$decided_by == "auto_rule"]
auto_basis <- paste0("auto_accept>=", LL_AUTO_ACCEPT_SCORE)

# Rows whose human layer is recomputed below start from the algorithm's view.
# Harmoniser rows (rule 2) and pipeline_drop rows (cumulative rejections) are
# left as they are; auto_rule rows from earlier runs too -- an acceptance is
# not withdrawn just because a later run stopped proposing the link.
reset <- ledger$decided_by %in% c("none", "reviewer", "model")
# Remember which links the model held before this run, to report flips below.
was_model <- ledger$link_id[ledger$decided_by == "model"]
to_auto <- reset & ledger$link_id %in% auto_ids
to_none <- reset & !ledger$link_id %in% auto_ids
ledger$link_status[to_auto] <- "accepted"
ledger$decided_by[to_auto]  <- "auto_rule"
ledger$decision_basis[to_auto] <-
  current$decision_basis[match(ledger$link_id[to_auto], current$link_id)]
ledger$link_status[to_none] <- "undecided"
ledger$decided_by[to_none]  <- "none"
ledger$decision_basis[to_none] <- "awaiting_review"
for (cl in c("decision_reasons", "decided_by_name", "decided_on"))
  ledger[[cl]][reset] <- ""

# ---- reviewer votes ----
rv <- read_history_latest(reviewer_history_dir())
if (!is.null(rv)) {
  for (cl in REVIEWER_HISTORY_COLS) if (!cl %in% names(rv)) rv[[cl]] <- ""
  rv <- rv[nzchar(rv$vote), , drop = FALSE]     # "" = withdrawn
}
n_rev_people <- if (is.null(rv)) 0L else length(unique(rv$who))
lllog("  reviewer_history: ", n_rev_people, " reviewer(s), ",
      if (is.null(rv)) 0L else nrow(rv), " current vote(s)")

seen <- kept <- broke <- integer(nrow(ledger))
who_of <- last_on <- character(nrow(ledger))
# The same counts over CONFIRMED votes only, for rule 3.
ckept <- cbroke <- integer(nrow(ledger))
cwho_of <- clast_on <- character(nrow(ledger))
if (!is.null(rv) && nrow(rv)) {
  key <- factor(rv$link_id, levels = ledger$link_id)
  orphan <- sum(is.na(key))
  if (orphan)
    lllog("  NOTE: ", orphan, " vote(s) are on links not in the ledger ",
          "(kept in reviewer_history, not applied)")
  ok <- !is.na(key)
  seen  <- as.integer(table(key[ok]))
  kept  <- as.integer(table(key[ok & rv$vote == "kept"]))
  broke <- as.integer(table(key[ok & rv$vote == "broken"]))
  w <- tapply(rv$who[ok], key[ok], function(x) paste(sort(x), collapse = "; "))
  who_of <- ifelse(is.na(w), "", unname(w))
  lo <- tapply(rv$recorded_on[ok], key[ok], max)
  last_on <- ifelse(is.na(lo), "", ll_history_date(unname(lo)))

  conf <- ok & rv$cluster_mark == "confirmed"
  ckept  <- as.integer(table(key[conf & rv$vote == "kept"]))
  cbroke <- as.integer(table(key[conf & rv$vote == "broken"]))
  if (any(conf)) {
    cw <- tapply(rv$who[conf], key[conf], function(x) paste(sort(x), collapse = "; "))
    cwho_of <- ifelse(is.na(cw), "", unname(cw))
    clo <- tapply(rv$recorded_on[conf], key[conf], max)
    clast_on <- ifelse(is.na(clo), "", ll_history_date(unname(clo)))
  }
}
ledger$reviewers_seen  <- as.character(seen)
ledger$reviewers_kept  <- as.character(kept)
ledger$reviewers_broke <- as.character(broke)
# Agreement among those who LOOKED. Blank where nobody did, rather than TRUE --
# three silences are not a consensus.
ledger$reviewers_agreed <- ifelse(seen == 0, "",
                           ifelse(kept == 0 | broke == 0, "TRUE", "FALSE"))

is_harm <- ledger$decided_by == "harmoniser"

# Rule 3: consensus among CONFIRMED votes. Unsure votes are not counted and
# cannot block; a single confirmed dissent sends the link to a harmoniser
# rather than being outvoted.
cons <- !is_harm &
  ((ckept >= LL_CONSENSUS_MIN & cbroke == 0) |
   (cbroke >= LL_CONSENSUS_MIN & ckept == 0))
n_conf   <- ckept + cbroke
n_unsure <- seen - n_conf
ledger$link_status[cons]      <- ifelse(ckept[cons] > 0, "accepted", "rejected")
ledger$decided_by[cons]       <- "reviewer"
ledger$decision_basis[cons]   <- "reviewer_consensus"
ledger$decision_reasons[cons] <- paste0(
  ifelse(ckept[cons] > 0, "kept", "broken"), " by ", n_conf[cons],
  " confirmed reviewer(s)",
  ifelse(n_unsure[cons] > 0,
         paste0("; ", n_unsure[cons], " unsure vote(s) not counted"), ""))
ledger$decided_by_name[cons]  <- cwho_of[cons]
ledger$decided_on[cons]       <- clast_on[cons]

# Rule 4: votes, but no consensus. An algorithm decision stands only where
# every vote agrees with it.
algo <- ledger$decided_by %in% c("auto_rule", "pipeline_drop")
contradicted <- algo & ((ledger$link_status == "accepted" & broke > 0) |
                        (ledger$link_status == "rejected" & kept > 0))
pend <- !is_harm & !cons & seen > 0 & (!algo | contradicted)
ledger$link_status[pend]    <- "undecided"
ledger$decided_by[pend]     <- "none"
ledger$decision_basis[pend] <- "awaiting_harmonisation"
ledger$decision_reasons[pend] <- ""
ledger$decided_by_name[pend]  <- ""
ledger$decided_on[pend]       <- ""

# ---- harmoniser verdicts (rule 1) ----
hv <- read_history_latest(harmoniser_history_dir())
n_harm <- 0L
if (!is.null(hv) && nrow(hv)) {
  for (cl in HARMONISER_HISTORY_COLS) if (!cl %in% names(hv)) hv[[cl]] <- ""
  hv <- hv[nzchar(hv$final_cluster_verdict), , drop = FALSE]
  # Two harmonisers on the same link: the more recent verdict wins. Rare, and
  # logged, because two people harmonising one cluster is a process problem.
  hv <- hv[order(hv$link_id, hv$recorded_on), , drop = FALSE]
  dup <- duplicated(hv$link_id, fromLast = TRUE)
  if (any(dup))
    lllog("  WARNING: ", length(unique(hv$link_id[dup])), " link(s) harmonised ",
          "by more than one person; the most recent verdict taken")
  hv <- hv[!dup, , drop = FALSE]
  j <- match(ledger$link_id, hv$link_id)
  orphan <- sum(is.na(match(hv$link_id, ledger$link_id)))
  if (orphan)
    lllog("  NOTE: ", orphan, " harmonised link(s) are not in the ledger ",
          "(kept in harmoniser_history, not applied)")
  h <- !is.na(j)
  v <- hv$final_cluster_verdict[j[h]]
  d <- hv$final_decision[j[h]]
  st <- ifelse(v == "settled", ifelse(d == "broken", "rejected", "accepted"),
               "undecided")
  ledger$link_status[h]      <- st
  ledger$decided_by[h]       <- "harmoniser"
  ledger$decision_basis[h]   <- ifelse(v == "settled", "harmonised",
                                       paste0("harmonised_", v))
  ledger$decision_reasons[h] <- hv$final_reasons[j[h]]
  ledger$decided_by_name[h]  <- hv$who[j[h]]
  ledger$decided_on[h]       <- ll_history_date(hv$recorded_on[j[h]])
  n_harm <- sum(h)
}
lllog("  harmoniser_history: ", n_harm, " link(s) with a verdict; ",
      sum(cons), " decided by reviewer consensus (", LL_CONSENSUS_MIN,
      "+ confirmed, none confirmed against); ", sum(pend),
      " awaiting harmonisation")

# ---- the link classifier (rank 1) ----
# After every human rule, so it can only fill what humans left empty.
n_model <- 0L
if (LL_MODEL_ENABLED) {
  md <- ll_read_csv(LL_MODEL_DECISIONS)
  if (is.null(md) || !nrow(md)) {
    lllog("  model: enabled, but no ", basename(LL_MODEL_DECISIONS),
          " yet -- nothing applied")
  } else if (!all(c("link_id", "decision", "model_version", "predicted_surety",
                    "predicted_on", "candidate_run") %in% names(md))) {
    lllog("  model: WARNING -- ", basename(LL_MODEL_DECISIONS), " is missing ",
          "expected columns -- nothing applied")
  } else if (!isTRUE(all(md$candidate_run == stem))) {
    # Decisions made for a different build: its links and evidence may have
    # moved, so they are not applied (the predict step re-runs after this).
    lllog("  model: decisions file is for run ",
          paste(unique(md$candidate_run), collapse = ", "), ", not ", stem,
          " -- not applied this pass")
  } else {
    md <- md[md$decision %in% "accept", , drop = FALSE]
    k <- match(ledger$link_id, md$link_id)
    # THE AUDIT RULE (2026-11-01), as for same-ID links: a model acceptance
    # stands while EVERY vote on the link agrees with it -- kept, in a cluster
    # marked confirmed. One "broken" or one unsure vote and the model's
    # decision is not applied: rule 4 above has already left such a link
    # awaiting harmonisation. (ckept counts confirmed "kept" votes.)
    votes_agree <- seen == 0L | ckept == seen
    apply_m <- !is.na(k) & ledger$decided_by == "none" &
      ledger$link_status == "undecided" & votes_agree &
      ledger$link_id %in% current$link_id &
      !ledger$link_type %in% "exact_source_alert"
    apply_m <- apply_m %in% TRUE        # never NA in a subscripted assignment
    ledger$link_status[apply_m]      <- "accepted"
    ledger$decided_by[apply_m]       <- "model"
    ledger$decision_basis[apply_m]   <- md$model_version[k[apply_m]]
    ledger$decision_reasons[apply_m] <- paste0("predicted surety ",
                                               md$predicted_surety[k[apply_m]])
    ledger$decided_by_name[apply_m]  <- ""
    ledger$decided_on[apply_m]       <- substr(md$predicted_on[k[apply_m]], 1, 10)
    n_model <- sum(apply_m)
    now_model <- ledger$link_id[apply_m]
    lllog("  model: ", n_model, " link(s) accepted by ",
          paste(unique(md$model_version), collapse = ", "), " (",
          sum(apply_m & seen > 0L), " of them confirmed by agreeing votes)")
    # FLIPS against the PREVIOUS DAY's decisions -- not against this run's
    # first pass, which on a new-export night has already cleared every model
    # decision (its file was for another build). The dated files are kept for
    # exactly this.
    prev_f <- ll_prev_model_file(dirname(LL_MODEL_DECISIONS))
    if (!is.na(prev_f)) {
      pm <- ll_read_csv(prev_f)
      was <- if (is.null(pm) || !"decision" %in% names(pm)) character(0)
             else pm$link_id[pm$decision %in% "accept"]
      gone <- setdiff(was, now_model)
      human_now <- ledger$link_id[ledger$decided_by %in% c("reviewer", "harmoniser")]
      lllog("  model vs ", basename(prev_f), ": ", length(intersect(was, now_model)),
            " still accepted, ", length(gone), " no longer (",
            sum(gone %in% human_now), " now decided by people, ",
            sum(!gone %in% current$link_id), " no longer proposed, the rest ",
            "under the cut-off or awaiting a harmoniser after a dissenting vote), ",
            length(setdiff(now_model, was)), " newly accepted")
    }
  }
} else if (length(was_model)) {
  lllog("  model: DISABLED -- ", length(was_model),
        " earlier model acceptance(s) withdrawn")
}

# ---- No longer under consideration (rule 6) ----
# A link nobody has decided that THIS run does not propose is kept in the
# ledger but taken out of play: link_status `inactive`, basis
# `no_longer_proposed`. The app only ever shows the current run, so nobody
# could review it; leaving it `undecided` would make "undecided" stop meaning
# "work to do", and would mark its identifiers as pending in the registry.
# Any votes on it are kept and still counted. If a later run proposes it
# again it is reset above and comes back as awaiting_review (or
# awaiting_harmonisation, if it has votes).
inactive <- ledger$decided_by == "none" & !ledger$link_id %in% current$link_id
ledger$link_status[inactive]    <- "inactive"
ledger$decision_basis[inactive] <- "no_longer_proposed"
lllog("  ", sum(inactive), " undecided link(s) not proposed by this run -> ",
      "inactive (kept, not under consideration)")

ledger <- ledger[order(ledger$link_id), , drop = FALSE]
ll_write_csv(ledger, LL_LEDGER_PATH)


# ---------------------------------------------------------------------------
# What the ledger now says
# ---------------------------------------------------------------------------

lllog("")
lllog("  merged: ", n_new, " new link(s), ", n_absent,
      " retained from earlier builds and not in this one, ",
      n_kept_old, " kept an earlier and stronger decision, ",
      n_upgraded, " changed")
lllog("")
tab <- table(ledger$link_status, ledger$decided_by)
for (st in rownames(tab)) {
  for (db in colnames(tab)) {
    if (tab[st, db] > 0)
      lllog(sprintf("  %-10s %-14s %6d", st, db, tab[st, db]))
  }
}

# The measurement that decided the design, re-run every time so it is never
# assumed: rejected links whose endpoints the ACCEPTED links reconnect anyway.
# A consumer that treats rejection as mere absence will merge these.
ll_zombie_rejections <- function(led) {
  rej <- led[led$link_status == "rejected", , drop = FALSE]
  if (!nrow(rej)) return(0L)
  acc <- led[led$link_status == "accepted", , drop = FALSE]
  nodes <- unique(c(led$node_a, led$node_b))

  # Union-find over the ACCEPTED links only, exactly as pr_components() does in
  # the review tab -- `<<-` for the path compression because the assignment has
  # to reach `root` in the enclosing frame, not create a local copy.
  root <- stats::setNames(nodes, nodes)
  find <- function(x) {
    while (root[[x]] != x) {
      root[[x]] <<- root[[root[[x]]]]
      x <- root[[x]]
    }
    x
  }
  if (nrow(acc)) {
    for (k in seq_len(nrow(acc))) {
      ra <- find(acc$node_a[k]); rb <- find(acc$node_b[k])
      if (ra != rb) root[[ra]] <- rb
    }
  }
  sum(vapply(seq_len(nrow(rej)),
             function(k) find(rej$node_a[k]) == find(rej$node_b[k]),
             logical(1)))
}
z <- ll_zombie_rejections(ledger)
n_rej <- sum(ledger$link_status == "rejected")
lllog("")
lllog("  ", z, " of ", n_rej, " rejected link(s) (",
      if (n_rej) round(100 * z / n_rej) else 0,
      "%) have endpoints the accepted links reconnect anyway.")
lllog("  Those are why rejections are stored as cannot-link constraints rather")
lllog("  than deleted: a consumer that reads absence as rejection merges them.")

lllog("")
lllog("  ", nrow(ledger), " link(s) in ", LL_LEDGER_PATH)
write_log_file(.ll_log, "link_ledger_log.txt")

link_ledger_complete <- TRUE
