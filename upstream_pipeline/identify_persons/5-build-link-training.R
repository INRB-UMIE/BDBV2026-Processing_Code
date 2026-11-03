#!/usr/bin/env Rscript
# 5-build-link-training.R
#
# The training table for the link classifier: one row per link a HUMAN has
# judged, carrying how sure the humans were that it is a true link (a surety
# score out of 1) and a set of NAME-FREE predictors. The model itself is built
# in the separate BDBV2026-Link_Classification repo; this step only prepares
# its data, because the inputs it needs (ages, phones, villages, dates) sit in
# files that also hold names, and those never leave the instance.
#
# Output: data/processed/person_matching/training/
#           link_training_<YYYY-MM-DD>.csv        one per day (a rebuild on the
#                                                 same day replaces that day's)
#           link_training_<YYYY-MM-DD>_meta.json  parameters + counts behind it
#           link_training_latest.csv              copy of today's, at a fixed path
#           link_training_latest_meta.json
#           link_scoring_<YYYY-MM-DD>.csv         the links the model will DECIDE:
#           link_scoring_latest.csv               undecided, no votes, proposed by
#                                                 today's run; same predictors, no
#                                                 labels (see "the SCORING links")
#
# MIRRORED TO GITHUB (agreed 2026-10-31): the link_training_* files only, via
# the allow list in .github/workflows/mirror-from-s3.yml. link_id is a pair of
# identifiers (an alert_id encodes province and health zone), so the repo it
# lands in must stay access-restricted.
#
# Runs straight after the link ledger and person registry, on every path that
# rebuilds them (full pipeline, Identify Persons, ledger-only/nightly), because
# new votes arrive every day whether or not there is a new export.
#
# ---------------------------------------------------------------------------
# THE SURETY SCORE
# ---------------------------------------------------------------------------
# Every reviewer's LATEST vote on the link (read_history_latest(), as the
# ledger reads them) is given a value:
#
#   kept,   cluster confirmed   1.00      kept,   cluster unsure   0.75
#   broken, cluster confirmed   0.00      broken, cluster unsure   0.25
#
# "Unsure" is marked on the whole cluster, so it applies to every link in it.
# Any mark other than `confirmed` counts as unsure, exactly as in the ledger.
# The most recent harmoniser verdict counts as `harmoniser_weight` (2) votes:
# settled + kept = 1, settled + broken = 0, unsure = 0.5. `escalate` is not a
# judgement and is left out. surety = weighted mean. All the values are in
# pipeline_configs.json, matching.training.
#
# Disagreement and doubt both pull the score towards 0.5, so the one number
# carries both what people decided and how sure they were.
#
# ---------------------------------------------------------------------------
# WHICH LINKS
# ---------------------------------------------------------------------------
#   * every ledger link with at least one counted vote or harmoniser verdict;
#   * ledger links a harmoniser decided with no history file behind them
#     (imported decisions), at value 1 / 0 and harmoniser weight, flagged
#     label_source = ledger_only;
#   * NOT same-ID links (link_type exact_source_alert): they are accepted by
#     rule and taken as given, so there is nothing for a model to learn.
#
# ---------------------------------------------------------------------------
# COLUMN CONVENTION
# ---------------------------------------------------------------------------
#   link_id, group_id, link_type, review_source, features_*   identity / meta
#   label_*   what humans said -- the TARGET. Never a predictor.
#   x_*       predictors. Symmetric by construction: a link has no left and
#             right side, so per-side values are given as _min / _max.
#
# group_id: links that share an identifier must land in the same train/test
# split, or the test flatters the model. group_id is a hash of the connected
# component the training links form, so the modelling code can split by it.
# It can change when a new vote joins two components; that is expected.
#
# Booleans are written 1 / 0, blank where the question cannot be asked (a
# field missing on either side, or a link type that does not carry it).
#
# CONFIDENTIALITY: no names, phone numbers, villages or identifiers are
# written -- link_id is the only key, and it is already in the ledger. The
# file is still derived from patient data and stays in person_matching/.

# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

LT_OUT_DIR <- file.path(person_matching_dir(), "training")

LT_VOTE_KC <- match_param("training.vote_kept_confirmed")
LT_VOTE_KU <- match_param("training.vote_kept_unsure")
LT_VOTE_BU <- match_param("training.vote_broken_unsure")
LT_VOTE_BC <- match_param("training.vote_broken_confirmed")
LT_HARM_UNSURE <- match_param("training.harmoniser_unsure")
LT_HARM_WEIGHT <- match_param("training.harmoniser_weight")
LT_PHONE_MAX   <- match_param("training.shared_phone_max_records")

# The plausible-date window, as in 2-build-person-candidates.R: an onset up to a
# year before the first report, nothing more than a month after the last.
LT_DATE_BACK_DAYS <- 365L
LT_DATE_FWD_DAYS  <- 31L

# Columns read from the candidate edges of the run that last proposed a link.
# Names are deliberately NOT in this list.
LT_EDGE_COLS <- c("node_a", "node_b", "link_type", "link_score", "name_score",
                  "name_distance", "corrob_score", "n_corroborators",
                  "n_corroborators_available",
                  "sex_a", "sex_b", "age_a", "age_b", "zone_a", "zone_b",
                  "area_a", "area_b", "classification_a", "classification_b",
                  "alert_type_a", "alert_type_b",
                  "name_rarity_pct_a", "name_rarity_pct_b",
                  "name_share_all_outside_a", "name_share_all_outside_b")

LT_CLASS_TERMINAL <- c("confirmed_case", "not_a_case")


# -------------------------  Helper Functions  ---------------------------------

.lt_log <- character(0)
ltlog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .lt_log <<- c(.lt_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

lt_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  write.table(df, tmp, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
  file.rename(tmp, path)
}

lt_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# Read only the named columns of a CSV (the rest are skipped, not loaded), all
# as character. Missing columns come back as NA, so an older file still reads.
lt_read_cols <- function(path, cols) {
  if (!file.exists(path)) return(NULL)
  hdr <- tryCatch(names(read.csv(path, nrows = 1, check.names = FALSE)),
                  error = function(e) NULL)
  if (is.null(hdr)) return(NULL)
  cc <- ifelse(hdr %in% cols, "character", "NULL")
  d <- tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = cc, check.names = FALSE),
                error = function(e) NULL)
  if (is.null(d)) return(NULL)
  for (cl in setdiff(cols, names(d))) d[[cl]] <- NA_character_
  d[, cols, drop = FALSE]
}

lt_as_date <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
}

lt_num <- function(x) suppressWarnings(as.numeric(x))

# TRUE/FALSE/NA -> 1/0/NA, for writing.
lt_b <- function(x) ifelse(is.na(x), NA_integer_, as.integer(x))

# Lower-case ASCII, letters and digits only: for comparing free text such as
# zone and village names across files.
lt_key <- function(x) {
  x <- name_translit_ascii(as.character(x))
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]", "", x)
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

# Same / different / can't tell, from two already-normalised vectors.
lt_same <- function(a, b) ifelse(is.na(a) | is.na(b), NA, a == b)

# Most frequent non-blank value per id, as pc_collapse() does in the candidate
# build (ties: first in byte order).
lt_mode_by <- function(id, x) {
  ok <- !lt_blank(id) & !lt_blank(x)
  if (!any(ok)) return(stats::setNames(character(0), character(0)))
  s <- split(trimws(as.character(x[ok])), trimws(as.character(id[ok])))
  vapply(s, function(v) {
    u <- sort(unique(v), method = "radix")   # byte order: same on every machine
    u[which.max(tabulate(match(v, u), nbins = length(u)))]
  }, character(1))
}

# Min / max of a Date per id, NA-safe. Returns a named numeric (days).
lt_date_by <- function(id, d, fn) {
  ok <- !lt_blank(id) & !is.na(d)
  if (!any(ok)) return(stats::setNames(numeric(0), character(0)))
  vapply(split(as.numeric(d[ok]), trimws(as.character(id[ok]))), fn, numeric(1))
}

# Deterministic string hash, the same rule as pc_hash() in the candidate build,
# written as 8 hex characters.
lt_hash <- function(x) {
  vapply(as.character(x), function(s) {
    h <- 0
    for (cp in utf8ToInt(enc2utf8(s))) h <- (h * 31 + cp) %% 2147483647
    sprintf("%08x", as.integer(h))
  }, character(1), USE.NAMES = FALSE)
}

# Connected components (union-find), returning each node's root.
lt_components <- function(a, b) {
  nodes <- unique(c(a, b))
  parent <- stats::setNames(nodes, nodes)
  find <- function(x) {
    while (parent[[x]] != x) {
      parent[[x]] <<- parent[[parent[[x]]]]
      x <- parent[[x]]
    }
    x
  }
  for (i in seq_along(a)) {
    ra <- find(a[i]); rb <- find(b[i])
    if (ra != rb) parent[[ra]] <- rb
  }
  vapply(nodes, find, character(1))
}

# ---- the surety score --------------------------------------------------------

# Value of one reviewer vote.
lt_vote_value <- function(vote, mark) {
  conf <- !is.na(mark) & mark == "confirmed"
  ifelse(vote == "kept", ifelse(conf, LT_VOTE_KC, LT_VOTE_KU),
         ifelse(vote == "broken", ifelse(conf, LT_VOTE_BC, LT_VOTE_BU),
                NA_real_))
}

# Value of one harmoniser verdict (NA = not a judgement: escalate, or blank).
# A settled verdict reads exactly as the ledger reads it: broken = 0, anything
# else = 1, so the two can never disagree about a settled link.
lt_harm_value <- function(verdict, decision) {
  ifelse(verdict == "settled", ifelse(decision == "broken", 0, 1),
         ifelse(verdict == "unsure", LT_HARM_UNSURE, NA_real_))
}

# ---- predictors --------------------------------------------------------------

# One typing slip between two ages: the same two digits swapped (34 / 43), or
# the same number of digits (two or more) with exactly one different (34 / 38).
# Two different single-digit ages (5 / 7) are NOT a slip -- for young children
# that is just a different age. Equal ages are not a slip (0); either age
# missing is NA.
lt_age_digit_slip <- function(a, b) {
  a <- round(lt_num(a)); b <- round(lt_num(b))
  vapply(seq_along(a), function(i) {
    if (is.na(a[i]) || is.na(b[i]) || a[i] < 0 || b[i] < 0) return(NA)
    if (a[i] == b[i]) return(FALSE)
    sa <- strsplit(as.character(as.integer(a[i])), "")[[1]]
    sb <- strsplit(as.character(as.integer(b[i])), "")[[1]]
    if (length(sa) != length(sb) || length(sa) < 2L) return(FALSE)
    swapped <- length(sa) == 2L && identical(sa, rev(sb))
    swapped || sum(sa != sb) == 1L
  }, logical(1))
}

# A phone number reduced to its last 9 digits (DRC numbers without the 243 or
# 0 prefix). Fewer than 9 digits, or one digit repeated, is not a number.
lt_phone <- function(x) {
  d <- gsub("[^0-9]", "", as.character(x))
  d[is.na(d)] <- ""
  out <- ifelse(nchar(d) >= 9L, substr(d, nchar(d) - 8L, nchar(d)), NA_character_)
  out[!is.na(out) & grepl("^(.)\\1+$", out, perl = TRUE)] <- NA_character_
  out
}

# Vaccination as yes / no / unknown(NA). The linelist codes O / N / Inc, the
# contact export V / N.
lt_vaccine <- function(x) {
  k <- toupper(trimws(as.character(x)))
  ifelse(k %in% c("O", "OUI", "V", "Y", "YES", "VACCINE", "VACCINATED"), "yes",
         ifelse(k %in% c("N", "NO", "NON"), "no", NA_character_))
}

# Great-circle distance in km (as pr_km() in the review tab).
lt_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371.0088
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}


# -------------------------  Inputs  -------------------------------------------

lt_build <- function() {
  ltlog("Link training table")

  # ---- ledger ----
  ledger <- lt_read_cols(file.path(person_matching_dir(), "link_ledger.csv"),
                         c("link_id", "node_a", "node_b", "link_type",
                           "link_status", "decided_by", "decision_basis",
                           "last_seen_run"))
  if (is.null(ledger) || !nrow(ledger))
    stop("no link ledger -- run 3-build-link-ledger.R first", call. = FALSE)
  ledger[is.na(ledger)] <- ""

  # ---- votes ----
  rv <- read_history_latest(reviewer_history_dir())
  if (is.null(rv)) rv <- data.frame(link_id = character(0), vote = character(0),
                                    cluster_mark = character(0),
                                    stringsAsFactors = FALSE)
  for (cl in c("vote", "cluster_mark", "edge_reason"))
    if (!cl %in% names(rv)) rv[[cl]] <- rep("", nrow(rv))   # rep(): rv may have 0 rows
  rv <- rv[rv$vote %in% c("kept", "broken"), , drop = FALSE]
  rv$value <- lt_vote_value(rv$vote, rv$cluster_mark)
  rv$conf  <- rv$cluster_mark == "confirmed"
  # Votes cast in the daily AUDIT (Audit view, 2026-11-01) carry an
  # edge_reason starting "audit".
  rv$audit <- startsWith(as.character(rv$edge_reason), "audit")

  # ---- harmoniser verdicts: the most recent per link, as in the ledger ----
  hv <- read_history_latest(harmoniser_history_dir())
  if (is.null(hv)) hv <- data.frame(link_id = character(0),
                                    final_cluster_verdict = character(0),
                                    final_decision = character(0),
                                    recorded_on = character(0),
                                    stringsAsFactors = FALSE)
  for (cl in c("final_cluster_verdict", "final_decision", "recorded_on"))
    if (!cl %in% names(hv)) hv[[cl]] <- ""
  hv <- hv[nzchar(hv$final_cluster_verdict), , drop = FALSE]
  hv <- hv[order(hv$link_id, hv$recorded_on), , drop = FALSE]
  hv <- hv[!duplicated(hv$link_id, fromLast = TRUE), , drop = FALSE]
  hv$value <- lt_harm_value(hv$final_cluster_verdict, hv$final_decision)
  hv <- hv[!is.na(hv$value), , drop = FALSE]

  # ---- which links ----
  judged <- ledger$link_type != "exact_source_alert"
  has_vote <- ledger$link_id %in% rv$link_id
  has_harm <- ledger$link_id %in% hv$link_id
  # Every vote on the link is "kept" in a confirmed cluster -- the votes the
  # ledger lets a model acceptance stand with (its audit rule).
  per_link <- function(ids) {
    t <- table(ids)
    v <- unname(stats::setNames(as.integer(t), names(t))[ledger$link_id])
    v[is.na(v)] <- 0L
    v
  }
  votes_agree <- per_link(rv$link_id[rv$conf & rv$vote == "kept"]) ==
    per_link(rv$link_id)                   # TRUE too where there are no votes
  ledger_only <- !has_vote & !has_harm & ledger$decided_by == "harmoniser" &
    ledger$link_status %in% c("accepted", "rejected")
  tr <- ledger[judged & (has_vote | has_harm | ledger_only) &
                 nzchar(ledger$node_a) & nzchar(ledger$node_b), , drop = FALSE]
  ltlog("  ", nrow(tr), " judged link(s) (same-ID links excluded); ",
        sum(!(rv$link_id %in% ledger$link_id)), " vote(s) on links not in the ",
        "ledger are skipped")
  if (!nrow(tr)) {
    # Remove yesterday's scoring file too, so the predict step finds nothing
    # rather than scoring stale links.
    unlink(file.path(LT_OUT_DIR, "link_scoring_latest.csv"))
    ltlog("  nothing to write")
    return(invisible(NULL))
  }

  # ---- labels ----
  k <- factor(rv$link_id, levels = tr$link_id)
  ok <- !is.na(k)
  cnt <- function(sel) as.integer(table(k[ok & sel]))
  tr$label_n_votes       <- cnt(rep(TRUE, nrow(rv)))
  tr$label_n_confirmed   <- cnt(rv$conf)
  tr$label_n_unsure      <- tr$label_n_votes - tr$label_n_confirmed
  tr$label_n_kept        <- cnt(rv$vote == "kept")
  tr$label_n_broken      <- cnt(rv$vote == "broken")
  vsum <- tapply(rv$value[ok], k[ok], sum)
  vsum <- ifelse(is.na(vsum), 0, as.numeric(vsum))

  j <- match(tr$link_id, hv$link_id)
  hval <- hv$value[j]
  tr$label_harmoniser <- ifelse(is.na(j), "",
    ifelse(hv$final_cluster_verdict[j] == "settled",
           paste0("settled_", hv$final_decision[j]), hv$final_cluster_verdict[j]))
  # Imported harmoniser decisions with no vote or verdict file behind them.
  lo <- tr$label_n_votes == 0L & is.na(j) & tr$decided_by == "harmoniser" &
    tr$link_status %in% c("accepted", "rejected")
  hval[lo] <- ifelse(tr$link_status[lo] == "accepted", 1, 0)
  tr$label_harmoniser[lo] <- paste0("ledger_", tr$link_status[lo])
  hw <- ifelse(is.na(hval), 0, LT_HARM_WEIGHT)

  tr$label_weight <- tr$label_n_votes + hw
  tr$label_surety <- round((vsum + ifelse(is.na(hval), 0, hval * hw)) /
                             tr$label_weight, 4)
  tr$label_source <- ifelse(lo, "ledger_only",
                     ifelse(!is.na(j) & tr$label_n_votes > 0, "votes+harmoniser",
                     ifelse(!is.na(j), "harmoniser", "votes")))
  tr$label_ledger_status <- tr$link_status
  # Where these labels came from: "audit" if any vote on the link was cast in
  # the daily audit (a random sample, so model performance can be measured on
  # these alone), otherwise "review".
  tr$review_source <- ifelse(tr$link_id %in% rv$link_id[rv$audit], "audit", "review")

  # ---- the SCORING links (added 2026-10-31) ----
  # The links the link classifier will decide: undecided, never voted on or
  # harmonised (a link humans have started on stays with humans), proposed by
  # TODAY's candidate run (so their evidence is current), not same-ID. They
  # get exactly the same predictors as the training links -- computed by the
  # same code below, in the same pass -- and no labels. Written to their own
  # file, link_scoring_<date>.csv.
  cur_s <- tryCatch(read_latest("person_candidates")$stem, error = function(e) NULL)
  if (is.null(cur_s) || !length(cur_s)) cur_s <- ""
  # Links the MODEL accepted on an earlier pass are scored again too: model
  # decisions are re-made every night from fresh evidence, never carried
  # forward (see LL_RANK in 3-build-link-ledger.R).
  #
  # Links with votes are scored too IF EVERY VOTE AGREES with acceptance
  # (kept, confirmed -- e.g. an audit "same person"): the ledger lets a model
  # acceptance stand on those (2026-11-01). Such a link is then in BOTH files:
  # training (it has labels) and scoring.
  sc <- ledger[judged & !has_harm & votes_agree &
                 ((ledger$link_status == "undecided" & ledger$decided_by == "none" &
                     ledger$decision_basis %in% c("awaiting_review",
                                                  "awaiting_harmonisation")) |
                    ledger$decided_by == "model") &
                 nzchar(cur_s) & ledger$last_seen_run == cur_s &
                 nzchar(ledger$node_a) & nzchar(ledger$node_b), , drop = FALSE]
  ltlog("  ", nrow(sc), " link(s) to score (", sum(sc$link_id %in% rv$link_id),
        " with agreeing votes), proposed by ",
        if (nzchar(cur_s)) cur_s else "no current run")
  if (nrow(sc)) {
    for (cl in setdiff(names(tr), names(sc))) sc[[cl]] <- NA
    sc$label_source  <- "unlabelled"
    sc$review_source <- ""
    tr <- rbind(tr, sc[, names(tr), drop = FALSE])
  }

  # ---- group_id: components over the LABELLED training links only ----
  # (Scoring links are left out: they would join otherwise separate training
  # groups together, and their group_id is never written anyway.)
  lab <- !tr$label_source %in% "unlabelled"
  root <- lt_components(tr$node_a[lab], tr$node_b[lab])
  members <- split(names(root), unname(root))
  first_of <- vapply(members, function(m) sort(m, method = "radix")[1],
                     character(1))
  tr$group_id <- NA_character_
  tr$group_id[lab] <- lt_hash(first_of[unname(root[tr$node_a[lab]])])

  # ---- pair-level evidence from the run that last proposed each link ----
  e_all <- list()
  runs <- unique(tr$last_seen_run[nzchar(tr$last_seen_run)])
  for (r in runs) {
    p <- file.path(pm_run_dir(r, "candidates"), "person_candidate_edges.csv")
    e <- lt_read_cols(p, LT_EDGE_COLS)
    if (is.null(e)) next
    e$link_id <- node_pair_key(e$node_a, e$node_b, "|")
    e <- e[e$link_id %in% tr$link_id[tr$last_seen_run == r], , drop = FALSE]
    e <- e[!duplicated(e$link_id), , drop = FALSE]
    if (nrow(e)) { e$features_run <- r; e_all[[length(e_all) + 1L]] <- e }
  }
  E <- if (length(e_all)) do.call(rbind, e_all) else NULL
  ie <- if (is.null(E)) rep(NA_integer_, nrow(tr)) else match(tr$link_id, E$link_id)
  ltlog("  candidate evidence found for ", sum(!is.na(ie)), " of ", nrow(tr),
        " link(s), from ", length(e_all), " run(s)")
  ec <- function(cl) if (is.null(E)) rep(NA_character_, nrow(tr)) else E[[cl]][ie]

  cur <- tryCatch(read_latest("person_candidates")$stem, error = function(e) NULL)
  if (is.null(cur) || !length(cur)) cur <- ""
  tr$features_run <- ifelse(is.na(ie), "", ec("features_run"))
  tr$features_from_current_run <- lt_b(ifelse(is.na(ie), NA,
                                              nzchar(cur) & tr$features_run == cur))
  # The ledger's link_type (what the exact-ID filter above used); the edges'
  # only where the ledger has none.
  tr$link_type <- ifelse(nzchar(tr$link_type) | is.na(ie), tr$link_type,
                         ec("link_type"))

  # Node order as written in the edges file, so _a / _b columns line up.
  na_ <- ifelse(is.na(ie), tr$node_a, ec("node_a"))
  nb_ <- ifelse(is.na(ie), tr$node_b, ec("node_b"))
  type_a <- sub(":.*$", "", na_); id_a <- sub("^[^:]*:", "", na_)
  type_b <- sub(":.*$", "", nb_); id_b <- sub("^[^:]*:", "", nb_)

  out <- data.frame(link_id = tr$link_id, group_id = tr$group_id,
                    link_type = tr$link_type, review_source = tr$review_source,
                    features_run = tr$features_run,
                    features_from_current_run = tr$features_from_current_run,
                    label_surety = tr$label_surety, label_weight = tr$label_weight,
                    label_n_votes = tr$label_n_votes,
                    label_n_confirmed = tr$label_n_confirmed,
                    label_n_unsure = tr$label_n_unsure,
                    label_n_kept = tr$label_n_kept,
                    label_n_broken = tr$label_n_broken,
                    label_harmoniser = tr$label_harmoniser,
                    label_source = tr$label_source,
                    label_ledger_status = tr$label_ledger_status,
                    stringsAsFactors = FALSE)

  # -- name and overall evidence --
  out$x_link_score    <- lt_num(ec("link_score"))
  out$x_name_score    <- lt_num(ec("name_score"))
  out$x_name_distance <- lt_num(ec("name_distance"))
  rp_a <- lt_num(ec("name_rarity_pct_a")); rp_b <- lt_num(ec("name_rarity_pct_b"))
  out$x_name_rarity_pct_min <- suppressWarnings(pmin(rp_a, rp_b))
  out$x_name_rarity_pct_max <- suppressWarnings(pmax(rp_a, rp_b))
  so_a <- lt_num(ec("name_share_all_outside_a"))
  so_b <- lt_num(ec("name_share_all_outside_b"))
  out$x_name_shared_outside_min <- suppressWarnings(pmin(so_a, so_b))
  out$x_name_shared_outside_max <- suppressWarnings(pmax(so_a, so_b))
  out$x_corrob_score <- lt_num(ec("corrob_score"))
  out$x_n_corroborators <- lt_num(ec("n_corroborators"))
  out$x_n_corroborators_available <- lt_num(ec("n_corroborators_available"))

  # -- person details from the edges --
  sx_a <- lt_key(ec("sex_a")); sx_b <- lt_key(ec("sex_b"))
  out$x_same_sex <- lt_b(lt_same(sx_a, sx_b))
  ag_a <- lt_num(ec("age_a")); ag_b <- lt_num(ec("age_b"))
  out$x_age_gap <- abs(ag_a - ag_b)
  out$x_age_digit_slip <- lt_b(lt_age_digit_slip(ag_a, ag_b))

  # -- place --
  z_a <- lt_key(ec("zone_a")); z_b <- lt_key(ec("zone_b"))
  out$x_same_zone <- lt_b(lt_same(z_a, z_b))
  out$x_same_area <- lt_b(lt_same(lt_key(ec("area_a")), lt_key(ec("area_b"))))

  geo_dir <- file.path(repo_root, "data", "geo")
  cent <- tryCatch(read.csv(file.path(geo_dir, "derived", "health_zone_centroids.csv"),
                            stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(cent) && nrow(cent)) {
    ck <- lt_key(cent$health_zone)
    ia <- match(z_a, ck); ib <- match(z_b, ck)
    out$x_zone_km <- round(lt_km(cent$lon[ia], cent$lat[ia],
                                 cent$lon[ib], cent$lat[ib]), 1)
    ltlog("  zone distance: ", sum(!is.na(out$x_zone_km)), " link(s) placed")
  } else {
    out$x_zone_km <- NA_real_
    ltlog("  WARNING: no health_zone_centroids.csv -- x_zone_km left blank")
  }

  # ETCs IN each side's zone: distinct sites (facilities sharing coordinates are
  # one site, as on the review map), from the newest cte_actives_YYYYMMDD.xlsx.
  etc_f <- sort(list.files(geo_dir, pattern = "^cte_actives_[0-9]{8}\\.xlsx$"),
                decreasing = TRUE)
  etc_per_zone <- NULL; etc_as_of <- ""
  if (length(etc_f) && requireNamespace("readxl", quietly = TRUE)) {
    d <- tryCatch(as.data.frame(readxl::read_excel(file.path(geo_dir, etc_f[1]))),
                  error = function(e) NULL)
    if (!is.null(d) && all(c("zonesante", "lat", "lon") %in% names(d))) {
      # Rows without coordinates are dropped first, as on the review map.
      d <- d[!is.na(lt_num(d$lat)) & !is.na(lt_num(d$lon)), , drop = FALSE]
      site <- paste(round(lt_num(d$lat), 4), round(lt_num(d$lon), 4))
      zk <- lt_key(d$zonesante)
      s <- unique(data.frame(z = zk, site = site, stringsAsFactors = FALSE))
      s <- s[!is.na(s$z), , drop = FALSE]
      etc_per_zone <- if (nrow(s)) table(s$z) else NULL
      etc_as_of <- sub("^cte_actives_([0-9]{8})\\.xlsx$", "\\1", etc_f[1])
    }
  }
  if (!is.null(etc_per_zone)) {
    n_etc <- function(z) ifelse(is.na(z), NA_real_,
                                ifelse(z %in% names(etc_per_zone),
                                       as.numeric(etc_per_zone[z]), 0))
    e_a <- n_etc(z_a); e_b <- n_etc(z_b)
    out$x_etc_in_zone_min <- suppressWarnings(pmin(e_a, e_b))
    out$x_etc_in_zone_max <- suppressWarnings(pmax(e_a, e_b))
    ltlog("  ETCs: ", sum(etc_per_zone), " site(s) in ", length(etc_per_zone),
          " zone(s), list dated ", etc_as_of)
  } else {
    out$x_etc_in_zone_min <- NA_real_
    out$x_etc_in_zone_max <- NA_real_
    ltlog("  WARNING: no readable cte_actives_YYYYMMDD.xlsx -- ETC columns blank")
  }

  # -- plausibility from the edges --
  cl_a <- trimws(ec("classification_a")); cl_b <- trimws(ec("classification_b"))
  both_cl <- !lt_blank(cl_a) & !lt_blank(cl_b)
  out$x_class_clash <- lt_b(ifelse(both_cl,
    cl_a != cl_b & cl_a %in% LT_CLASS_TERMINAL & cl_b %in% LT_CLASS_TERMINAL, NA))
  # The two classifications as one category, in byte order (not the locale's),
  # so the same pair reads the same on every machine.
  out$x_class_pair <- ifelse(both_cl, vapply(seq_along(cl_a), function(i)
    paste(sort(c(cl_a[i], cl_b[i]), method = "radix"), collapse = "|"),
    character(1)), "")

  # ---- fields from the current exports --------------------------------------
  # Everything below is looked up by identifier in today's processed exports,
  # so it reflects the newest corrections. An identifier that has left the
  # export (DHIS2 reissued it) simply gets blanks.
  ll_n <- ct_n <- 0L
  ll <- NULL
  epi_dir <- tryCatch(resolve_epi_latest_dir(), error = function(e) NA_character_)
  if (!is.na(epi_dir)) {
    # _names.csv because telephone_num is only there. No name column is read:
    # lt_read_cols() loads the listed columns and skips every other.
    ll <- lt_read_cols(file.path(epi_dir, "dhis2_processed_linelist_names.csv"),
                       c("alert_id", "telephone_num", "permanent_residence_village",
                         "vaccine_status",
                         "statut_vaccinal_du_malade_vaccine_contre_mve",
                         "health_worker_position", "type_of_alert",
                         "date_of_symptom_onset", "reporting_date",
                         "date_of_notification", "investigation_datetime",
                         "date_of_hospitalisation_start", "pec_date_dadmission"))
  }
  ct <- NULL
  ct_in <- tryCatch(resolve_ct_matching_input(), error = function(e) NULL)
  if (!is.null(ct_in)) {
    ct_safe <- sub("_names\\.csv$", "_ctsafe.csv", ct_in$names_csv)
    ct <- lt_read_cols(ct_safe, c("contact_id", "source_case_alert_id",
                                  "registration_date", "followup_date",
                                  "vaccine_status"))
  }

  # Plausible-date window from the linelist's reporting_date, as the candidate
  # build does; applied to every date below, contact dates included, so one
  # mistyped year cannot produce a gap of decades.
  rep_d <- if (!is.null(ll)) lt_as_date(ll$reporting_date) else lt_as_date(character(0))
  rep_d <- rep_d[!is.na(rep_d)]
  lo_d <- if (length(rep_d)) min(rep_d) - LT_DATE_BACK_DAYS else as.Date("1900-01-01")
  hi_d <- if (length(rep_d)) max(rep_d) + LT_DATE_FWD_DAYS else as.Date("2999-12-31")
  pd <- function(x) { d <- lt_as_date(x); d[!is.na(d) & (d < lo_d | d > hi_d)] <- NA; d }

  phone_of <- village_of <- vacc_ll <- hw_of <- alert_of <- character(0)
  onset_ll <- death_ll <- alive_ll <- numeric(0)
  if (!is.null(ll) && nrow(ll)) {
    ll_n <- nrow(ll)
    id <- trimws(ll$alert_id)

    ph <- lt_phone(ll$telephone_num)
    n_ids <- tapply(id[!is.na(ph)], ph[!is.na(ph)], function(v) length(unique(v)))
    common <- names(n_ids)[n_ids > LT_PHONE_MAX]
    ph[ph %in% common] <- NA_character_
    phone_of   <- lt_mode_by(id, ph)
    village_of <- lt_mode_by(id, lt_key(ll$permanent_residence_village))
    # Renamed vaccine_status on 2026-10-30; older exports carry the long name.
    vac_raw <- ll$vaccine_status
    old <- lt_blank(vac_raw)
    vac_raw[old] <- ll$statut_vaccinal_du_malade_vaccine_contre_mve[old]
    vacc_ll    <- lt_mode_by(id, lt_vaccine(vac_raw))
    hw_of      <- lt_mode_by(id, ifelse(lt_blank(ll$health_worker_position), NA, "hw"))
    alert_of   <- lt_mode_by(id, ll$type_of_alert)
    onset_ll   <- lt_date_by(id, pd(ll$date_of_symptom_onset), min)
    # When a death alert's death was known: its earliest administrative date.
    adm <- pmin(pd(ll$reporting_date), pd(ll$date_of_notification),
                pd(ll$investigation_datetime), na.rm = TRUE)
    death_ll <- lt_date_by(id, adm, min)
    # Activity only a LIVING person can have: symptom onset and admission. Lab
    # dates are deliberately not used -- post-mortem swabs are routine.
    # Admission: date_of_hospitalisation_start since 2026-10-27, the made-up
    # pec_date_dadmission before (see PC_LL_EVENTS in the candidate build).
    adm_d <- pd(ll$date_of_hospitalisation_start)
    adm_d[is.na(adm_d)] <- pd(ll$pec_date_dadmission)[is.na(adm_d)]
    alive <- pmax(pd(ll$date_of_symptom_onset), adm_d, na.rm = TRUE)
    alive_ll <- lt_date_by(id, alive, max)
    ltlog("  linelist: ", ll_n, " row(s); ", length(common), " phone number(s) ",
          "on more than ", LT_PHONE_MAX, " alert_ids treated as not personal")
    for (nm in c("phone_of", "village_of", "vacc_ll", "hw_of", "alert_of"))
      if (!length(get(nm))) ltlog("  WARNING: ", nm, " is empty -- has a column ",
                                  "been renamed in the export?")
  } else {
    ltlog("  WARNING: no processed linelist -- linelist-derived columns blank")
  }

  vacc_ct <- character(0); reg_ct <- alive_ct <- numeric(0); src_of <- list()
  if (!is.null(ct) && nrow(ct)) {
    ct_n <- nrow(ct)
    cid <- trimws(ct$contact_id)
    vacc_ct  <- lt_mode_by(cid, lt_vaccine(ct$vaccine_status))
    reg      <- pd(ct$registration_date)
    reg_ct   <- lt_date_by(cid, reg, min)
    alive_ct <- lt_date_by(cid, pmax(reg, pd(ct$followup_date), na.rm = TRUE), max)
    okc <- !lt_blank(cid) & !lt_blank(ct$source_case_alert_id)
    src_of <- lapply(split(trimws(ct$source_case_alert_id[okc]), cid[okc]), unique)
    ltlog("  contact tracing: ", ct_n, " row(s) (", basename(ct_in$dir), ")")
  } else {
    ltlog("  WARNING: no processed contact tracing -- contact-derived columns blank")
  }

  getv <- function(map, ids) { v <- unname(map[ids]); v[is.na(ids)] <- NA; v }
  is_ll_a <- type_a == "ll"; is_ll_b <- type_b == "ll"
  is_ct_a <- type_a == "ct_contact"; is_ct_b <- type_b == "ct_contact"
  ll_id <- function(is, ids) ifelse(is, ids, NA_character_)

  # -- phone, village, health worker: linelist on both sides only --
  both_ll <- is_ll_a & is_ll_b
  p_a <- getv(phone_of, ll_id(is_ll_a, id_a)); p_b <- getv(phone_of, ll_id(is_ll_b, id_b))
  out$x_same_phone <- lt_b(ifelse(both_ll, lt_same(p_a, p_b), NA))
  v_a <- getv(village_of, ll_id(is_ll_a, id_a)); v_b <- getv(village_of, ll_id(is_ll_b, id_b))
  out$x_same_village <- lt_b(ifelse(both_ll, lt_same(v_a, v_b), NA))
  # Health worker: 1 both flagged, 0 exactly one, blank when neither (a blank
  # position does not say someone is NOT a health worker).
  h_a <- !is.na(getv(hw_of, ll_id(is_ll_a, id_a)))
  h_b <- !is.na(getv(hw_of, ll_id(is_ll_b, id_b)))
  out$x_health_worker_match <- lt_b(ifelse(both_ll & (h_a | h_b), h_a & h_b, NA))

  # -- vaccination: linelist or contact on either side --
  vac <- function(is_ll, is_ct, ids) ifelse(is_ll, getv(vacc_ll, ll_id(is_ll, ids)),
                                     ifelse(is_ct, getv(vacc_ct, ifelse(is_ct, ids, NA)),
                                            NA_character_))
  out$x_same_vaccine_status <- lt_b(lt_same(vac(is_ll_a, is_ct_a, id_a),
                                            vac(is_ll_b, is_ct_b, id_b)))

  # -- onset / registration gap --
  first_date <- function(is_ll, is_ct, ids)
    ifelse(is_ll, getv(onset_ll, ll_id(is_ll, ids)),
           ifelse(is_ct, getv(reg_ct, ifelse(is_ct, ids, NA)), NA_real_))
  out$x_date_gap_days <- abs(first_date(is_ll_a, is_ct_a, id_a) -
                               first_date(is_ll_b, is_ct_b, id_b))

  # -- death, then living activity on the other record --
  dead_a <- is_ll_a & getv(alert_of, ll_id(is_ll_a, id_a)) %in% "death"
  dead_b <- is_ll_b & getv(alert_of, ll_id(is_ll_b, id_b)) %in% "death"
  alive_date <- function(is_ll, is_ct, ids)
    ifelse(is_ll, getv(alive_ll, ll_id(is_ll, ids)),
           ifelse(is_ct, getv(alive_ct, ifelse(is_ct, ids, NA)), NA_real_))
  after_ab <- ifelse(dead_a, alive_date(is_ll_b, is_ct_b, id_b) -
                       getv(death_ll, ll_id(is_ll_a, id_a)), NA_real_)
  after_ba <- ifelse(dead_b, alive_date(is_ll_a, is_ct_a, id_a) -
                       getv(death_ll, ll_id(is_ll_b, id_b)), NA_real_)
  out$x_n_death_alerts <- if (length(alert_of))
    as.integer(dead_a) + as.integer(dead_b) else NA_integer_
  out$x_days_alive_after_death <- suppressWarnings(pmax(after_ab, after_ba, na.rm = TRUE))

  # -- a contact of themselves: the contact's listed source case IS the other
  #    side (a linelist alert_id or a source case) --
  srcs <- function(ids, is) lapply(seq_along(ids), function(i)
    if (is[i] && !is.null(src_of[[ids[i]]])) src_of[[ids[i]]] else character(0))
  s_a <- srcs(id_a, is_ct_a); s_b <- srcs(id_b, is_ct_b)
  other_case_b <- type_b %in% c("ll", "ct_case"); other_case_a <- type_a %in% c("ll", "ct_case")
  self_ab <- vapply(seq_along(id_a), function(i) id_b[i] %in% s_a[[i]], logical(1))
  self_ba <- vapply(seq_along(id_a), function(i) id_a[i] %in% s_b[[i]], logical(1))
  askable <- (is_ct_a & other_case_b) | (is_ct_b & other_case_a)
  out$x_contact_of_self <- lt_b(ifelse(askable & length(src_of) > 0,
                                       (is_ct_a & self_ab) | (is_ct_b & self_ba), NA))

  # -- kind of link (categorical; same as link_type, kept as a predictor) --
  out$x_link_type <- out$link_type

  # ---- guard: nothing identifying leaves this step ----
  banned <- c("node_a", "node_b", "name", "name_a", "name_b", "telephone_num",
              "permanent_residence_village", "alert_id", "contact_id")
  stopifnot(!any(names(out) %in% banned))

  # ---- write ----
  out <- out[out$link_type != "exact_source_alert", , drop = FALSE]
  out <- out[order(out$link_id), , drop = FALSE]
  day <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")

  # The scoring links: same identity and predictor columns, no label_*, no
  # group_id or review_source (they mean nothing without a label).
  is_sc <- out$label_source %in% "unlabelled"
  score <- out[is_sc, c("link_id", "link_type", "features_run",
                        "features_from_current_run",
                        grep("^x_", names(out), value = TRUE)), drop = FALSE]
  out <- out[!is_sc, , drop = FALSE]
  sc_dated <- file.path(LT_OUT_DIR, paste0("link_scoring_", day, ".csv"))
  lt_write_csv(score, sc_dated)
  sc_latest <- file.path(LT_OUT_DIR, "link_scoring_latest.csv")
  file.copy(sc_dated, paste0(sc_latest, ".tmp"), overwrite = TRUE)
  file.rename(paste0(sc_latest, ".tmp"), sc_latest)
  ltlog("  ", nrow(score), " link(s) to score -> ", sc_dated)

  dated <- file.path(LT_OUT_DIR, paste0("link_training_", day, ".csv"))
  lt_write_csv(out, dated)
  latest <- file.path(LT_OUT_DIR, "link_training_latest.csv")
  file.copy(dated, paste0(latest, ".tmp"), overwrite = TRUE)
  file.rename(paste0(latest, ".tmp"), latest)

  meta <- list(
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    n_links = nrow(out),
    n_scoring_links = nrow(score),
    n_groups = length(unique(out$group_id)),
    candidate_run = if (is.null(cur)) "" else cur,
    linelist_rows = ll_n, contact_rows = ct_n,
    etc_list_date = etc_as_of,
    surety_values = list(
      vote_kept_confirmed = LT_VOTE_KC, vote_kept_unsure = LT_VOTE_KU,
      vote_broken_unsure = LT_VOTE_BU, vote_broken_confirmed = LT_VOTE_BC,
      harmoniser_unsure = LT_HARM_UNSURE, harmoniser_weight = LT_HARM_WEIGHT),
    shared_phone_max_records = LT_PHONE_MAX,
    label_source = as.list(table(out$label_source)),
    surety_bands = as.list(table(cut(out$label_surety,
                                     c(-0.01, 0.05, 0.25, 0.45, 0.55, 0.75, 0.95, 1)))))
  mj <- jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE)
  writeLines(mj, sub("\\.csv$", "_meta.json", dated))
  writeLines(mj, file.path(LT_OUT_DIR, "link_training_latest_meta.json"))

  ltlog("  surety: ", sum(out$label_surety >= 0.95), " >= 0.95, ",
        sum(out$label_surety <= 0.05), " <= 0.05, ",
        sum(out$label_surety > 0.05 & out$label_surety < 0.95), " in between")
  ltlog("  label sources: ",
        paste(names(table(out$label_source)), table(out$label_source),
              sep = " ", collapse = "; "))
  ltlog("  ", nrow(out), " row(s), ", ncol(out), " column(s) -> ", dated)
  invisible(out)
}

link_training <- lt_build()
write_log_file(.lt_log, "link_training_log.txt")

link_training_complete <- TRUE
