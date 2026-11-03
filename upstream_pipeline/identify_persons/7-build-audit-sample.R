#!/usr/bin/env Rscript
# 7-build-audit-sample.R
#
# The day's AUDIT: a small random set of pairs shown to reviewers BLIND, before
# the normal queue, to measure how the link classifier and the 0.4 cut-off do
# on links nobody chose to review (agreed 2026-10-31).
#
#   out: data/processed/person_matching/audit/
#          audit_<YYYY-MM-DD>.csv   one per day, drawn ONCE: a rerun the same
#                                   day for the same run keeps it (a new
#                                   export that day draws a fresh one)
#          audit_latest.csv         what Review Persons reads: these clusters
#                                   go at the front of its queue
#
# Three samples, shuffled together so a reviewer cannot tell which is which:
#   model_accept   links the MODEL accepted          matching.audit.n_model_accept
#                  (at least matching.audit.min_within_linelist of them
#                   within-linelist -- the type with the weakest CV result and
#                   in no review queue)
#   model_review   links the model scored but left   matching.audit.n_model_review
#                  for people (any score)
#   near_miss      pairs scoring 0.3-0.4, never      matching.audit.n_near_miss
#                  made into links (the cut-off check)
#
# Only links NOBODY has voted on are drawn (near misses: nobody has judged), so
# every audit answer is a fresh, independent label. Drawn once a day (seeded
# by date + run). It is NOT redrawn when the ledger is updated later that day:
# the pools exclude anything voted on, so a redraw would swap the clusters
# people had just audited for fresh ones (seen 2026-11-02).
#
# The `sample` and `predicted_surety` columns are for analysing the audit
# afterwards. The app NEVER shows them -- that is what keeps the audit blind
# (see pr_audit_rows() in shiny/server_person_review.R).
#
# CONFIDENTIALITY: identifiers and scores only, no names (the app joins names
# from the candidate files when it renders a pair).

# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}

.au_log <- character(0)
aulog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .au_log <<- c(.au_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

au_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                                colClasses = "character"), error = function(e) NULL)
  if (!is.null(d)) d[is.na(d)] <- ""
  d
}

audit_built <- FALSE
aulog("Audit sample")

tryCatch(local({
  # Inside the tryCatch, so a missing config key is logged like any other
  # failure rather than escaping.
  AU_N_ACCEPT <- as.integer(match_param("audit.n_model_accept"))
  AU_MIN_LL   <- as.integer(match_param("audit.min_within_linelist"))
  AU_N_REVIEW <- as.integer(match_param("audit.n_model_review"))
  AU_N_NEAR   <- as.integer(match_param("audit.n_near_miss"))

  pm    <- person_matching_dir()
  stem  <- read_latest("person_candidates")$stem
  if (is.null(stem) || !nzchar(stem)) stop("no current candidate run", call. = FALSE)
  day   <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")

  # Already drawn today for this run? Keep it.
  existing <- au_read(file.path(pm, "audit", paste0("audit_", day, ".csv")))
  if (!is.null(existing) && nrow(existing) &&
      isTRUE(all(existing$candidate_run == stem))) {
    aulog("  today's audit already drawn for this run (", nrow(existing),
          " pair(s)); kept")
    audit_built <<- TRUE
    return(invisible(NULL))
  }

  ledger <- au_read(file.path(pm, "link_ledger.csv"))
  if (is.null(ledger)) stop("no link ledger", call. = FALSE)
  voted <- unique(c(read_history_latest(reviewer_history_dir())$link_id,
                    read_history_latest(harmoniser_history_dir())$link_id))
  live <- ledger[ledger$last_seen_run == stem &
                   ledger$link_type != "exact_source_alert" &
                   !ledger$link_id %in% voted, , drop = FALSE]

  # The model's scores for tonight's run, if it ran.
  md <- au_read(file.path(pm, "model_decisions", "model_decisions_latest.csv"))
  if (!is.null(md) && !isTRUE(all(md$candidate_run == stem))) md <- NULL
  score_of <- if (is.null(md)) character(0) else
    stats::setNames(md$predicted_surety, md$link_id)

  # Reproducible per day: the date (and run) set the seed.
  seed <- sum(utf8ToInt(paste0(day, stem)) * seq_along(utf8ToInt(paste0(day, stem))))
  set.seed(seed %% .Machine$integer.max)
  take <- function(ids, n) if (length(ids) <= n) ids else sample(ids, n)

  # ---- model_accept: at least AU_MIN_LL within-linelist ----
  acc <- live[live$decided_by == "model" & live$link_status == "accepted", , drop = FALSE]
  ll_ids    <- take(acc$link_id[acc$link_type == "within_linelist"], AU_MIN_LL)
  rest_pool <- setdiff(acc$link_id, ll_ids)
  acc_ids   <- c(ll_ids, take(rest_pool, max(0L, AU_N_ACCEPT - length(ll_ids))))

  # ---- model_review: scored by the model, left for people ----
  rev_pool <- live$link_id[live$decided_by == "none" & live$link_status == "undecided" &
                             live$link_id %in% names(score_of) &
                             live$link_id %in% md$link_id[md$decision == "human_review"]]
  rev_ids <- take(rev_pool, AU_N_REVIEW)

  rows <- function(ids, sample_name) {
    if (!length(ids)) return(NULL)
    l <- live[match(ids, live$link_id), , drop = FALSE]
    data.frame(link_id = l$link_id, node_a = l$node_a, node_b = l$node_b,
               link_type = l$link_type, sample = sample_name,
               predicted_surety = unname(score_of[l$link_id]),
               link_score = l$link_score, stringsAsFactors = FALSE)
  }

  # ---- near_miss: from tonight's near-miss sample, not yet judged ----
  nm <- au_read(file.path(pm_run_dir(stem, "candidates"),
                          "person_candidate_near_misses.csv"))
  # Near-miss verdicts live in their own running files (one per person), in
  # the same shape as the vote files, so the same reader serves.
  nm_judged <- read_history_latest(file.path(pm, "near_miss_history"))$link_id
  near <- NULL
  if (!is.null(nm) && nrow(nm)) {
    pool <- nm[!nm$link_id %in% c(nm_judged, voted), , drop = FALSE]
    pick <- take(pool$link_id, AU_N_NEAR)
    if (length(pick)) {
      p <- pool[match(pick, pool$link_id), , drop = FALSE]
      near <- data.frame(link_id = p$link_id, node_a = p$node_a, node_b = p$node_b,
                         link_type = p$link_type, sample = "near_miss",
                         predicted_surety = NA_character_, link_score = p$link_score,
                         stringsAsFactors = FALSE)
    }
  }

  out <- do.call(rbind, list(rows(acc_ids, "model_accept"),
                             rows(rev_ids, "model_review"), near))
  if (is.null(out) || !nrow(out)) {
    aulog("  nothing to audit tonight")
    return(invisible(NULL))
  }
  out <- out[sample(nrow(out)), , drop = FALSE]       # shuffle the three together
  out <- data.frame(audit_order = seq_len(nrow(out)), out,
                    candidate_run = stem, audit_date = day, stringsAsFactors = FALSE)

  dir <- file.path(pm, "audit")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dated <- file.path(dir, paste0("audit_", day, ".csv"))
  utils::write.csv(out, paste0(dated, ".tmp"), row.names = FALSE, na = "")
  file.rename(paste0(dated, ".tmp"), dated)
  latest <- file.path(dir, "audit_latest.csv")
  file.copy(dated, paste0(latest, ".tmp"), overwrite = TRUE)
  file.rename(paste0(latest, ".tmp"), latest)

  tab <- table(out$sample)
  aulog("  ", nrow(out), " pair(s): ",
        paste(names(tab), as.integer(tab), sep = " ", collapse = ", "),
        " (", sum(out$sample == "model_accept" & out$link_type == "within_linelist"),
        " of the model's within-linelist) -> ", dated)
  audit_built <<- TRUE
}), error = function(e) aulog("  audit sample not built: ", conditionMessage(e)),
finally = write_log_file(.au_log, "audit_sample_log.txt"))
