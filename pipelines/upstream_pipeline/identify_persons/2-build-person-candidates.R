#!/usr/bin/env Rscript
# 2-build-person-candidates.R
#
# Chain identifiers ACROSS classifications into candidate persons.
#
# Phase 1 gave every row an ID. The two dedup steps found duplicates WITHIN each
# classification. This finds the same person BETWEEN them: one human may hold
# several `alert_id`s, several `contact_id`s and several `source_case_alert_id`s,
# and nothing so far connects those three namespaces.
#
# Outputs, in data/processed/person_matching/<stem>/candidates/:
#   person_candidate_edges.csv     one row per LINK, with its evidence
#   person_candidate_clusters.csv  one row per candidate person, with flags
#   review_sample.csv              stratified calibration sample of clusters
#   review_contact_tracing.csv     clusters with a CT contact / source case
#   review_all.csv                 every cluster with a link to judge
# and the log, in logs/<run>/person_candidates_log.txt.
#
# Identification only. Nothing is merged; no export is modified.
#
# ---------------------------------------------------------------------------
# WHY EDGES ARE THE UNIT, NOT CLUSTERS
# ---------------------------------------------------------------------------
# A cluster is a CONSEQUENCE of its edges, and on this data the consequence is
# often wrong even when every edge is individually defensible.
#
# Measured on LINELIST_01092026 x CONTACT-TRACING_01092026, the largest cluster
# over tiers A+B+C holds 11 identifiers:
#
#   MBALISI TAMBOLI ANNIE / ANNIE TAMBOLI / ANIE CHANTAL TAMBOLI /
#   Tamboli Anni Rosette / ANI CHANTAL / TAMBOLI KOMBE ANNIE / ...
#
# spread across Bunia, Fataki, Lita, Mongbwalu, Nizi and Rwampara. That is
# several distinct people sharing two very common given names. Accept-or-reject
# on the whole cluster can only take all eleven or discard all eleven; neither
# is right. So every edge is emitted separately, with its own score and evidence,
# and the cluster is derived. Breaking an edge splits the cluster.
#
# The same failure at tier A alone, for the record: six linelist rows plus a
# contact chained on the shared given name JEAN DE DIEU — OKELO WANGARA,
# AKENA MBATEKO, LOKWIYA, MBATEKO, MBATEKO, OPIO — ages 28 to 49 across three
# health zones. Restricting to tier A does not make closure safe; it only makes
# it smaller.
#
# ---------------------------------------------------------------------------
# THE THREE POPULATIONS AND THE LINKS BETWEEN THEM
# ---------------------------------------------------------------------------
#   ll         linelist alert_id            35,108 named identifiers
#   ct_contact CT contact_id                 2,539
#   ct_case    CT source_case_alert_id         259
#
#   exact_source_alert  ct_case  == ll        230 links, NO matching needed --
#                                             the two namespaces are the same,
#                                             so 230 of 259 source cases join a
#                                             linelist row for free. This is the
#                                             single largest source of certain
#                                             links in the whole exercise.
#   contact_to_linelist ct_contact ~ ll       fuzzy ONLY. `C-26-S00-CONT-...`
#                                             shares no component with an alert
#                                             ID, so a contact who later becomes
#                                             a case is detectable by name and
#                                             demographics alone. Irreducible.
#   contact_to_source   ct_contact ~ ct_case  fuzzy; a contact who becomes a
#                                             source case for others.
#
# Within-classification edges are READ from the three dedup outputs rather than
# recomputed, so a link score means here exactly what it meant there.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

PC_GLOBAL_MAX_DIST <- match_param("name_weight.global_max_dist")
PC_AGE_TOLERANCE   <- match_param("corroborators.age_tolerance_years")
PC_MIN_TOKEN_CHARS <- match_param("tokenisation.score_min_token_chars")
PC_MAX_TOKENS      <- match_param("tokenisation.max_tokens_per_name")

# Multipliers on log(N), not absolute IDF. An IDF ceiling is log(N), so an
# absolute threshold silently means something different in every corpus -- see
# the note in 3-deduplicate_contacts.R, where copying the linelist's absolute
# 12/15/8 into a corpus a tenth the size made them ~32% stricter and disabled
# the single-token tier outright.
# Used only for the rarity bands on the review tiles (pc_token_band()): the
# point at which ONE token counts as "very rare".
PC_SINGLE_TOKEN_MULT <- match_param("name_weight.single_token_mult")

# Cluster-level plausibility. These do NOT reject anything; they mark clusters
# a reviewer should open first.
PC_MAX_AGE_SPAN   <- 10L
PC_LARGE_CLUSTER  <- 5L




# -------------------------  Helper Functions  ---------------------------------

.pc_log <- character(0)
pclog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .pc_log <<- c(.pc_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

pc_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

pc_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# Dates arrive both as "2026-06-13" and as "2026-06-13 00:00:00.0", so the
# first ten characters are taken before parsing. as.Date() on the full datetime
# string works for some formats and returns NA for others, which would silently
# drop the 2,663 admission dates.
#
# Defined here with the other helpers, not down beside the timeline section
# where it is mostly used: the final-follow-up lookup needs it while the
# populations are being built, which happens much earlier, and a top-level
# statement cannot call a function defined below it.
pc_as_date <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
}

pc_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                  colClasses = "character")
}

# Tokenisation is name_translit_ascii() / name_tokens() in 0-setup.R -- the one
# definition, shared with both dedups and the rarity build. See the note there
# on why the transliteration map is explicit rather than iconv, and why
# apostrophes are deleted rather than split on.
pc_translit_ascii <- name_translit_ascii

pc_tokens <- function(x, min_chars = PC_MIN_TOKEN_CHARS) {
  name_tokens(x, min_chars = min_chars, max_tokens = PC_MAX_TOKENS)
}

pc_sorted <- function(x) {
  vapply(pc_tokens(x), function(tk) paste0(sort(tk), collapse = ""), character(1))
}

pc_agree <- function(a, b) {
  a <- trimws(as.character(a)); b <- trimws(as.character(b))
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  nzchar(a) & nzchar(b) & a == b
}

# IS THIS FIELD COMPARABLE FOR THIS PAIR? One per agreement test, and they must
# stay paired -- see the long note on pc_n_avail() below.
pc_cmp_chr <- function(a, b) !pc_blank(a) & !pc_blank(b)

# Age is compared as a NUMBER, so availability tests the coercion rather than
# non-blankness: an age of "unknown" is not blank, cannot agree, and must not
# be counted as comparable.
pc_cmp_num <- function(a, b) {
  !is.na(suppressWarnings(as.numeric(a))) & !is.na(suppressWarnings(as.numeric(b)))
}

pc_age_agree <- function(a, b) {
  na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
  ok <- !is.na(na) & !is.na(nb)
  res <- rep(FALSE, length(na))
  res[ok] <- abs(na[ok] - nb[ok]) <= PC_AGE_TOLERANCE
  res
}

# Union-find. NOTE the two assignment operators: `<<-` inside find() reaches the
# enclosing function environment, which is correct; the union below is at this
# function's top level, where `<<-` would instead write to the GLOBAL
# environment and leave the local `parent` untouched -- silently returning every
# node as its own component. That exact bug was written and caught once already.
pc_components <- function(a, b) {
  nodes <- unique(c(a, b))
  if (!length(nodes)) return(stats::setNames(character(0), character(0)))
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


# -------------------------  Locate and load  ----------------------------------

epi_processed <- file.path(repo_root, "data", "processed", "dhis2_linelist_processed")
ct_processed  <- file.path(repo_root, "data", "processed", "contact_tracing_processed")

pclog("=== Building cross-classification person candidates ===")

epi_dir <- tryCatch(resolve_epi_latest_dir(epi_processed), error = function(e) NA_character_)
# CT input via the shared resolver: the all-time combined dataset or the latest
# export, per contact_tracing.data.matching_input. ct_dir is also where the CT
# dedup's candidates are read from below, and the dedup resolves the same way,
# so the within-CT links always describe the contacts being matched here.
if (is.na(epi_dir)) {
  stop("Both a processed linelist and a processed contact tracing export are ",
       "required. This step links the two.", call. = FALSE)
}
ct_in <- resolve_ct_matching_input(ct_processed)
ct_dir <- ct_in$dir
epi_stem <- basename(epi_dir); ct_stem <- ct_in$stem

ll <- pc_read(file.path(epi_dir, "dhis2_processed_linelist_names.csv"))
ct <- pc_read(ct_in$names_csv)
if (is.null(ll) || is.null(ct)) {
  stop("Both _names.csv exports are required — names are the only thing linking ",
       "a contact_id to an alert_id.", call. = FALSE)
}
pclog("  linelist: ", epi_stem, " (", nrow(ll), " rows)")
pclog("  contact tracing: ", ct_stem, " (", nrow(ct), " rows)")

# One entry per identifier, most frequent spelling wins.
pc_collapse <- function(df, id_col, cols) {
  if (!id_col %in% names(df)) return(NULL)
  ids <- trimws(as.character(df[[id_col]]))
  keep <- !pc_blank(ids)
  if (!any(keep)) return(NULL)
  idx <- split(which(keep), ids[keep])
  out <- data.frame(id = names(idx), stringsAsFactors = FALSE)
  for (cl in names(cols)) {
    src <- cols[[cl]]
    out[[cl]] <- if (src %in% names(df)) {
      vapply(idx, function(rows) {
        v <- trimws(as.character(df[[src]][rows]))
        v <- v[!pc_blank(v)]
        if (!length(v)) return(NA_character_)
        tb <- table(v)
        names(tb)[which.max(as.numeric(tb))]
      }, character(1))
    } else NA_character_
  }
  out$id <- names(idx)
  out
}

pop_ll <- pc_collapse(ll, "alert_id", list(name = "name", sex = "sex", age = "age",
                                           zone = "health_zone", area = "health_area",
                                           onset = "date_of_symptom_onset",
                                           classification = "final_mve_case_classification",
                                           # `case` (28,971) or `death` (6,029), blank on
                                           # 213 rows. Worth a tile row of its own because
                                           # it is present on 99% of rows where
                                           # final_mve_case_classification is blank on 62%.
                                           alert_type = "type_of_alert"))
pop_ct <- pc_collapse(ct, "contact_id", list(name = "contact_name", sex = "contact_sex",
                                             age = "contact_age", zone = "health_zone",
                                             area = "health_area",
                                             onset = "registration_date",
                                             # Filled in below from the LAST dated
                                             # follow-up, which pc_collapse cannot do.
                                             last_followup = NA,
                                             # A contact has no case classification: it
                                             # is a linelist concept. Structurally
                                             # absent, not missing — the review tile
                                             # omits the row rather than showing a gap.
                                             classification = NA))
pop_cs <- pc_collapse(ct, "source_case_alert_id", list(name = "case_name", sex = NA,
                                                       age = NA, zone = "health_zone",
                                                       area = NA, onset = NA,
                                                       classification = NA,
                                                       alert_type = NA,
                                                       last_followup = NA))
pop_ll$last_followup <- NA_character_
pop_ct$alert_type    <- NA_character_
for (nm in c("pop_ll", "pop_ct", "pop_cs")) {
  p <- get(nm)
  assign(nm, p[!pc_blank(p$name), , drop = FALSE])
}

# Result of the LAST recorded follow-up for each contact.
#
# Not pc_collapse()'s "most frequent value wins", which is right for a name
# spelling and wrong here: a contact followed up 21 times as `not_a_case` and
# once, finally, as `case_confirmed` would be reported as `not_a_case` — hiding
# the single most decisive fact about them.
#
# Taken by MAXIMUM follow-up date rather than the `is_last_followup_day` flag:
# that flag reads "yes" on only 390 of 20,632 rows across 2,539 contacts, so it
# cannot identify the last day for most of them.
#
# 32 contacts end on `contact_became_suspected_case` and 24 on `case_confirmed`
# — those 56 are contacts who became cases, which is exactly the
# cross-namespace link this whole step exists to find, so the tile should say
# so rather than making a reviewer infer it.
if (!is.null(pop_ct) && nrow(pop_ct) &&
    all(c("followup_date", "followup_result") %in% names(ct))) {
  fu_d <- pc_as_date(ct$followup_date)
  fu_r <- trimws(as.character(ct$followup_result))
  fu_c <- trimws(as.character(ct$contact_id))
  keep <- !is.na(fu_d) & nzchar(fu_c)
  if (any(keep)) {
    last_of <- vapply(split(which(keep), fu_c[keep]), function(rows) {
      mx <- max(fu_d[rows])
      res <- unique(fu_r[rows][fu_d[rows] == mx])
      res <- res[!is.na(res) & nzchar(res)]
      if (!length(res)) return(NA_character_)
      # 4 contacts have two different results recorded on their last date.
      # Both are shown rather than one being picked silently, because there is
      # no principled way to rank them and the disagreement is itself worth
      # seeing.
      paste(sort(res), collapse = " / ")
    }, character(1))
    pop_ct$last_followup <- unname(last_of[pop_ct$id])
    pclog("  contacts with a dated final follow-up result: ",
          sum(!is.na(pop_ct$last_followup)), " of ", nrow(pop_ct))
  }
}

# Name of each contact's source case, FOR DISPLAY ONLY on the contact tile.
#
# Not pc_collapse()'s "most frequent wins": a contact listed under two source
# cases genuinely has two, and hiding one would hide a lead. Every distinct
# name is kept, most frequent first, joined with " / ". Not used by the matcher
# in any way -- it changes no score, tier or cluster.
if (!is.null(pop_ct) && nrow(pop_ct) &&
    all(c("contact_id", "case_name") %in% names(ct))) {
  pop_ct$source_case <- NA_character_
  sc_c <- trimws(as.character(ct$contact_id))
  sc_n <- trimws(as.character(ct$case_name))
  keep <- !pc_blank(sc_c) & !pc_blank(sc_n)
  if (any(keep)) {
    sc_of <- vapply(split(sc_n[keep], sc_c[keep]), function(v) {
      tb <- sort(table(v), decreasing = TRUE)
      paste(names(tb), collapse = " / ")
    }, character(1))
    pop_ct$source_case <- unname(sc_of[pop_ct$id])
    pclog("  contacts with a named source case: ",
          sum(!is.na(pop_ct$source_case)), " of ", nrow(pop_ct))
  }
}

# A source case that IS a linelist alert_id inherits that row's case
# classification, FOR DISPLAY ONLY.
#
# 230 of 274 source cases are exactly a linelist alert_id, so most source-case
# tiles can show a real classification instead of a blank. Deliberately NOT
# extended to sex, age or health area: those are corroborator fields, and
# inheriting them would raise pc_n_avail() for contact_to_source_case links
# from at most 1 to at most 4 and change which tier those links reach. That may well be the
# right thing to do — a source case that is a linelist row genuinely has that
# row's sex and age — but it is a change to the matcher, not to a tile, and it
# should be measured on its own rather than arriving as a side effect of
# improving the review interface.
if (!is.null(pop_cs) && nrow(pop_cs)) {
  i <- match(pop_cs$id, pop_ll$id)
  pop_cs$classification <- ifelse(is.na(i), NA_character_, pop_ll$classification[i])
  pclog("  source cases resolvable to a linelist row (for classification): ",
        sum(!is.na(i)), " of ", nrow(pop_cs))
}
pclog("\n  Populations (one entry per identifier, named):")
pclog("    linelist alert_id          ", nrow(pop_ll))
pclog("    CT contact_id              ", nrow(pop_ct))
pclog("    CT source_case_alert_id    ", nrow(pop_cs))


# ---------  Name rarity and collocations over the pooled corpus  -------------
# READ, NOT RECOMPUTED. identify_persons/1-build_name_rarity.R builds both over
# the pooled corpus -- linelist + contact tracing, one entry per identifier --
# before any deduplication runs, and writes them to a running table that every
# matching script reads.
#
# This script used to assemble the same corpus from pop_ll / pop_ct / pop_cs and
# count it again. The two were required to agree, and tests/test_name_score.py
# ran both over the same export to check that they did. Recomputing an identical
# quantity and then testing that it came out identical is a weaker guarantee
# than not recomputing it, and it quietly made the populations loaded here part
# of the definition of "rare" -- so a change to which rows pc_collapse() keeps
# would have moved every score in the file.
#
# Pooling is the point, and is unchanged: a contact-to-linelist link is scored
# against everyone known to the outbreak, not against either file alone. 34% of
# CT name tokens never appear in the linelist, so a linelist-only table would
# score them as maximally rare and inflate confidence on exactly these
# cross-file links.

.pc_nr <- load_name_rarity()

PC_N          <- .pc_nr$n
PC_IDF        <- .pc_nr$idf_vec
PC_IDF_UNSEEN <- .pc_nr$idf_unseen
pc_idf        <- .pc_nr$idf_of

# Every named identifier in the populations this script loaded. NOT the rarity
# corpus any more -- it is only the reference distribution the review tiles read
# a percentile against, below.
pc_all_names <- c(pop_ll$name, pop_ct$name, pop_cs$name)

# Which loaded identifiers contain each token. DISPLAY ONLY: pc_share_all()
# answers "how many people here carry every part of this name", which is a
# statement about the identifiers in front of the reviewer, not about the
# rarity corpus. Kept local for that reason -- the two were the same object
# before and conflating them again would make a tile's count depend on the
# rarity table's corpus rather than on the cluster being reviewed.
.pc_per  <- lapply(pc_tokens(pc_all_names), unique)
.pc_flat <- unlist(.pc_per, use.names = FALSE)
.pc_idx  <- split(rep(seq_along(.pc_per), lengths(.pc_per)), .pc_flat)

# Token frequency for BLOCKING (skip tokens carried by more than 400 people).
# Read from the shared table: blocking on a locally-counted frequency while
# scoring on a pooled one meant the set of pairs ever compared and the weight
# they were then given disagreed about how common a token is.
PC_TOK_FREQ <- stats::setNames(as.numeric(.pc_nr$rarity$n_entries),
                               .pc_nr$rarity$token)

# Collocated token groups, from the same table. The full argument for the 10x
# ratio, the clique rule, and the measurement showing that a true joint rarity
# is WORSE at separating matches from non-matches than the corrected sum, is in
# 1-build_name_rarity.R beside NR_COLLOC_MIN_RATIO. It matters most here:
# `MBATEKO JEAN DE DIEU` scored 20.50 while six identifiers carry every part of
# that name (two of them carry exactly it).
PC_COLLOC     <- .pc_nr$groups
PC_COLLOC_IDF <- .pc_nr$group_idf

# The name weight for a set of SHARED tokens, with collocated groups scored as
# one unit. Used by both the edge builder and pc_name_rarity(), so a name's
# displayed rarity and the weight of a link that rests on it cannot disagree.
pc_weight_tokens <- .pc_nr$weight_tokens

# Read here for the first time. The other two matchers have applied it since it
# was introduced; this script never did, and there is no comment anywhere
# arguing that it should not -- it looks like it was simply not ported. A name
# repeated on BOTH sides was therefore worth 4 nats less here than in either
# dedup, on exactly the pairs (`ZAWADI ZAWADI MARIE`) where the repetition is the
# distinctive thing about the name.
PC_REPEAT_BONUS <- match_param("name_weight.shared_repeat_bonus")

# The two link-score cutoffs. Defined HERE, with the other thresholds, rather
# than beside the code that first uses them: pc_within() reads min_keep and
# runs well before the edge table exists, so a definition further down would
# be resolved only at call time and would not be there yet.
PC_MIN_LINK_SCORE    <- match_param("link_score.min_keep")
PC_AUTO_ACCEPT_SCORE <- match_param("link_score.auto_accept")

# The name-weight-to-score curve, and the two component weights. Up here for
# the same reason: the pair de-duplication ranks duplicates by score and runs
# before the score columns are built, so both have to exist by then.
PC_NAME_SCORE_MID   <- match_param("name_weight.score_midpoint_mult")  * PC_IDF_UNSEEN
PC_NAME_SCORE_STEEP <- match_param("name_weight.score_steepness_mult") * PC_IDF_UNSEEN
PC_SCORE_W <- score_weights()

pc_name_score <- function(w) {
  1 / (1 + exp(-(w - PC_NAME_SCORE_MID) / PC_NAME_SCORE_STEEP))
}


# ---------------------------------------------------------------------------
# PER-NAME RARITY, FOR THE REVIEW TILES
# ---------------------------------------------------------------------------
# How unusual is this whole name, across everyone known to the outbreak?
#
# This is CONTEXT, not evidence. The number that drove a match is the edge's
# `name_weight`, which scores only the tokens the two names SHARE. A node's own
# rarity answers a different question — "is this a name I should expect to see
# twice by chance?" — and it is what a reviewer is implicitly asking when they
# look at `JEAN KAMBALE` next to `ZILONGO`.
#
# Computed here rather than in the Shiny app on purpose: scoring a name needs
# the tokeniser, and a second copy of the tokeniser in the app would be a third
# place for it to drift. The app reads the numbers.
#
# Reported as a PERCENTILE as well as a raw score, because the raw score is not
# interpretable on its own: it is a sum over tokens, so it rises with the number
# of name parts as much as with their rarity. Measured over the 35,105 linelist
# identifiers, the median name scores 16.99 and the range is 3.57 to 56.90 — so
# "17" tells a reviewer nothing, while "rarer than 50% of names" tells them
# exactly what they need.
pc_name_rarity <- function(x) {
  vapply(pc_tokens(x), function(tk) {
    tk <- unique(tk)
    if (!length(tk)) return(NA_real_)
    pc_weight_tokens(tk)
  }, numeric(1))
}

# The corpus distribution the percentile is read against: every named
# identifier in the pooled corpus, one entry each.
PC_RARITY_REF <- sort(pc_name_rarity(pc_all_names))
PC_RARITY_REF <- PC_RARITY_REF[!is.na(PC_RARITY_REF)]

# Percentile of names this name is RARER than. ties.method = "first" via
# findInterval on the sorted reference: cheap, and monotone by construction.
pc_rarity_pct <- function(score) {
  if (!length(PC_RARITY_REF)) return(rep(NA_real_, length(score)))
  out <- 100 * findInterval(score, PC_RARITY_REF) / length(PC_RARITY_REF)
  out[is.na(score)] <- NA_real_
  round(out)
}

pclog("  Per-name rarity: median ", round(stats::median(PC_RARITY_REF), 2),
      ", range ", round(min(PC_RARITY_REF), 2), " to ",
      round(max(PC_RARITY_REF), 2), " over ", length(PC_RARITY_REF), " names")

pclog("\n  Pooled name rarity: ", PC_N, " identifiers, ", length(PC_IDF),
      " tokens, max idf ", round(PC_IDF_UNSEEN, 2))
pclog("    a link is kept when its link score >= ", PC_MIN_LINK_SCORE,
      " (link_score.min_keep), the same rule as both dedups")


# ---------------------------------------------------------------------------
# HOW MANY FIELDS CAN THIS PAIR BE COMPARED ON?
# ---------------------------------------------------------------------------
# PER PAIR, not per population, and the distinction was a real bug rather than
# a refinement. This used to take the two POPULATIONS and ask `any(!pc_blank(
# A[[f]]))` -- "does any record in this population record a sex" -- which is
# true of the linelist whenever a single row has one. So the denominator was a
# constant for the comparison type (4 for contact-to-linelist, 1 for the
# source-case pairings), and a pair where one side's sex was simply missing was
# charged for it: pc_agree() is FALSE on a blank, so the field landed in
# n_avail - n_corr exactly as a contradiction would.
#
# corrob_score() reads 2*n_corr - n_avail, so that is not a rounding error. A
# contact-to-linelist pair agreeing on sex and zone with age and area blank
# scored 2*2 - 4 = 0 -> 0.50, where the two fields it could actually be checked
# on both agreed and it should score 2*2 - 2 = +2 -> 0.79. Worse, a pair
# agreeing on the ONE field it could compare, with three blank, scored -2 ->
# 0.21: punished for what was never recorded.
#
# The config's claim that a blank is "neither credit nor penalty" was therefore
# true only of a field absent from a whole population -- a source case has no
# sex, ever -- and false of ordinary missing data, which is most of it. The
# population-level test was written for the former and quietly did duty for the
# latter.
#
# It still handles the former, because a field NA across all of pop_cs is
# necessarily blank in every pair drawn from it. Nothing is lost by moving the
# question to the pair; the population case is the limit of it.
#
# 2-deduplicate_linelist.R has always computed this per pair, term for term
# against its five agreement tests. This brings the cross-file matcher into
# line with it rather than inventing a third convention.
pc_n_avail <- function(A, B, i, j) {
  sum(c(pc_cmp_chr(A$sex[i],  B$sex[j]),
        pc_cmp_num(A$age[i],  B$age[j]),
        pc_cmp_chr(A$zone[i], B$zone[j]),
        pc_cmp_chr(A$area[i], B$area[j])))
}

# The CEILING for a comparison: how many fields these two populations could ever
# be compared on. Reporting only -- it is what the log line quotes, and it is
# never a denominator. Keeping the old
# population-level test under its own name means the two cannot be confused
# again.
pc_max_avail <- function(A, B, flds = c("sex", "age", "zone", "area")) {
  sum(vapply(flds, function(f) {
    !is.null(A[[f]]) && !is.null(B[[f]]) &&
      any(!pc_blank(A[[f]])) && any(!pc_blank(B[[f]]))
  }, logical(1)))
}



# ---------------------------------------------------------------------------
# CONFIRMED CASE vs NOT A CASE IS A CONTRADICTION
# ---------------------------------------------------------------------------
# final_mve_case_classification takes four values. Two are PROVISIONAL --
# `suspected_case` (175 rows) and `probable_case` (4) -- so a person suspected
# today and confirmed or discarded next week is one person whose classification
# legitimately moved. suspected<->confirmed, suspected<->not_a_case and
# probable<->confirmed are all compatible progressions and are NOT flagged.
#
# `confirmed_case` and `not_a_case` are both terminal and mutually exclusive: a
# lab-confirmed case cannot also be a non-case. That single pairing is 177 of
# the 190 disagreeing links in the calibration run -- 93% -- so the narrow rule
# costs almost nothing in coverage.
#
# WHAT IT DOES: it is a veto on auto-accepting the cluster, nothing more.
# Measured over 39 reviewed clusters, class_mismatch appeared in 5 broken
# clusters and 0 intact ones -- evidence against a merge, and merging a
# confirmed case with a non-case produces one record that is both, which lands
# directly on the case count. So such a cluster always goes to a reviewer,
# however well it scores. (It used to also drop weak clusters from the queue
# altogether, via the retired review buckets.)
#
# Only ever true where BOTH sides are classified, and 61% of linelist rows are
# blank, so this is a sharp rule with limited reach rather than a big filter.
PC_CLASS_TERMINAL <- c("confirmed_case", "not_a_case")

pc_class_contradiction <- function(e) {
  if (!all(c("classification_a", "classification_b") %in% names(e))) return(FALSE)
  a <- trimws(as.character(e$classification_a))
  b <- trimws(as.character(e$classification_b))
  # Exact identifier links are not judgements, so they cannot contradict.
  judged <- e$link_type != "exact_source_alert"
  isTRUE(any(judged & !is.na(a) & !is.na(b) & nzchar(a) & nzchar(b) & a != b &
               a %in% PC_CLASS_TERMINAL & b %in% PC_CLASS_TERMINAL,
             na.rm = TRUE))
}

# -------------------------  Cross-classification links  -----------------------

edge_rows <- list()
add_edge <- function(type_a, id_a, type_b, id_b, link_type,
                     weight = NA_real_, dist = NA_real_, corrob = NA_integer_,
                     n_avail = NA_integer_) {
  edge_rows[[length(edge_rows) + 1L]] <<- data.frame(
    node_a = paste0(type_a, ":", id_a), node_b = paste0(type_b, ":", id_b),
    id_type_a = type_a, id_a = id_a, id_type_b = type_b, id_b = id_b,
    link_type = link_type,
    name_weight = round(weight, 2), name_distance = round(dist, 4),
    n_corroborators = corrob, n_corroborators_available = n_avail,
    stringsAsFactors = FALSE
  )
}

# ---- exact: a source_case_alert_id IS an alert_id ----------------------------
# Same namespace, so this needs no matching and carries no uncertainty. It is
# the largest block of certain links available.
exact_ids <- intersect(pop_cs$id, pop_ll$id)
for (i in exact_ids) {
  add_edge("ct_case", i, "ll", i, "exact_source_alert")
}
pclog("\n  exact source_case_alert_id == alert_id: ", length(exact_ids),
      " of ", nrow(pop_cs), " source cases")

# ---- near misses -------------------------------------------------------------
# Pairs scoring in [near_miss.min_score, link_score.min_keep). NOT candidates:
# they never become edges or clusters. Collected so the Algorithm Tuning tab
# can audit what the cutoff throws away -- see near_miss in
# pipeline_configs.json and the sample built after the tile attributes below.
# Cross-file pairs are caught in pc_link(); within-file ones come from the
# dedups' near_miss files.
PC_NEAR_MISS_MIN <- match_param("near_miss.min_score")
PC_NEAR_MISS_N   <- match_param("near_miss.per_link_type")
pc_is_near_miss <- function(lsc) {
  !is.na(lsc) && lsc >= PC_NEAR_MISS_MIN && lsc < PC_MIN_LINK_SCORE
}
.pc_nm <- new.env(parent = emptyenv())
.pc_nm$rows <- list()
pc_nm_add <- function(node_a, node_b, link_type, link_score, name_weight,
                      corrob, n_avail) {
  .pc_nm$rows[[length(.pc_nm$rows) + 1L]] <- data.frame(
    node_a = node_a, node_b = node_b, link_type = link_type,
    link_score = link_score, name_weight = name_weight,
    n_corroborators = corrob, n_corroborators_available = n_avail,
    stringsAsFactors = FALSE)
}

# ---- fuzzy links between two populations ------------------------------------
pc_link <- function(A, B, type_a, type_b, link_type) {
  if (is.null(A) || is.null(B) || !nrow(A) || !nrow(B)) return(0L)
  tok_a <- pc_tokens(A$name); tok_b <- pc_tokens(B$name)
  srt_a <- pc_sorted(A$name); srt_b <- pc_sorted(B$name)

  # The ceiling, for the log only. Each pair's own denominator is computed
  # inside the loop; this says how many fields the comparison can use at best.
  max_avail <- pc_max_avail(A, B)
  pclog("    ", link_type, ": at most ", max_avail,
        " comparable field(s). A pair missing a field is scored against the ",
        "fields IT carries.")

  # Block on shared tokens, skipping very common ones. Without this the
  # comparison is 2,539 x 35,108.
  freq <- PC_TOK_FREQ
  tok2b <- new.env(parent = emptyenv())
  for (j in seq_len(nrow(B))) {
    for (t in unique(tok_b[[j]])) {
      f <- freq[t]
      if (!is.na(f) && f > 400) next
      cur <- tok2b[[t]]
      tok2b[[t]] <- c(if (is.null(cur)) integer(0) else cur, j)
    }
  }
  n_added <- 0L
  for (i in seq_len(nrow(A))) {
    ta <- unique(tok_a[[i]])
    if (!length(ta)) next
    cand <- unique(unlist(lapply(ta, function(t) tok2b[[t]]), use.names = FALSE))
    if (!length(cand)) next
    for (j in cand) {
      d <- stringdist::stringdist(srt_a[i], srt_b[j], method = "jw", p = 0.1)
      if (is.na(d) || d > PC_GLOBAL_MAX_DIST) next
      tb <- unique(tok_b[[j]])
      # THE SHARED SCORER, not intersect(). Tokens had to be byte-identical
      # here, so this matcher alone could not see `lycie`/`lucie` -- the one
      # place in the pipeline whose whole job is to link records ACROSS files,
      # where spellings are most likely to differ. Raw token lists, not `ta`/
      # `tb`, because the repetition bonus needs the duplicates.
      sc <- name_pair_score(tok_a[[i]], tok_b[[j]], pc_idf,
                            groups       = PC_COLLOC,
                            group_idf    = PC_COLLOC_IDF,
                            repeat_bonus = PC_REPEAT_BONUS,
                            max_tokens   = PC_MAX_TOKENS)
      if (!sc$n_matched) next
      w <- sc$weight
      corrob <- sum(c(pc_agree(A$sex[i], B$sex[j]),
                      pc_age_agree(A$age[i], B$age[j]),
                      pc_agree(A$zone[i], B$zone[j]),
                      pc_agree(A$area[i], B$area[j])))
      # One term per corroborator above, in the same order.
      n_avail <- pc_n_avail(A, B, i, j)
      # A field that agreed but was not counted as comparable means the two
      # tests disagree about what blank is.
      stopifnot(corrob <= n_avail)
      # THE LINK SCORE DECIDES, as in both dedups. There used to be a second
      # gate here -- pc_tier(), a minimum name strength that a cross-file
      # pair had to clear before its score was even looked at -- which the
      # dedups had already dropped. Computed with the same arithmetic and
      # rounding as the link_score column below (name weight rounded to 2 dp,
      # each half to 4), so a pair is never kept here and dropped there.
      .np <- round(PC_SCORE_W[["name"]] * pc_name_score(round(w, 2)), 4)
      .cp <- round(PC_SCORE_W[["corroborator"]] * corrob_score(corrob, n_avail), 4)
      if (pc_is_near_miss(round(.np + .cp, 4)))
        pc_nm_add(paste0(type_a, ":", A$id[i]), paste0(type_b, ":", B$id[j]),
                  link_type, round(.np + .cp, 4), round(w, 2), corrob, n_avail)
      if (is.na(.np + .cp) || round(.np + .cp, 4) < PC_MIN_LINK_SCORE) next
      add_edge(type_a, A$id[i], type_b, B$id[j], link_type, w, d, corrob,
               n_avail)
      n_added <- n_added + 1L
    }
  }
  n_added
}

n_ctll <- pc_link(pop_ct, pop_ll, "ct_contact", "ll", "contact_to_linelist")
pclog("  contact_id ~ alert_id (fuzzy): ", n_ctll, " links")
n_ctcs <- pc_link(pop_ct, pop_cs, "ct_contact", "ct_case", "contact_to_source_case")
pclog("  contact_id ~ source_case_alert_id (fuzzy): ", n_ctcs, " links")

# SOURCE CASE ~ LINELIST, BY NAME. The pairing that had no fuzzy path.
#
# A source case IS a linelist case; the two are the same person by definition.
# Until now the only way to say so was exact string equality of
# source_case_alert_id and alert_id, which held for 242 of 256. The other 14
# could reach the linelist only indirectly, through a contact they happened to
# share -- and they are precisely the cases DHIS2 gave no usable ID for, which
# is why they carry an assigned CI-/CX- ID and can never equal an alert_id at
# all. The one pairing whose members are certainly the same person was the one
# pairing with no name matching.
#
# RUN OVER ALL 256, not just the unmatched 14. Two reasons. A source case that
# already has its exact edge will fuzzy-match the same linelist row, and the
# pair de-duplication below keeps the better-scoring one, so the redundant edge
# costs nothing. And a source case matching a DIFFERENT linelist row by name is
# worth seeing: it means the linelist holds another record of that case, which
# is a duplicate this graph exists to surface.
#
# pop_cs carries only a health zone -- no sex, age or area -- so pc_n_avail()
# gives at most 1 (0 where either side's zone is blank), exactly as for
# contact_to_source_case: these links rest mostly on the name.
n_csll <- pc_link(pop_cs, pop_ll, "ct_case", "ll", "source_case_to_linelist")
pclog("  source_case_alert_id ~ alert_id (fuzzy): ", n_csll, " links")


# -------------------------  Within-classification edges  ----------------------
# Read from the dedup outputs rather than recomputed, so a link score means the
# same thing here as it did there.

# The two dedup scripts name three of these columns differently
# (name_dist vs name_distance, corroborators vs n_corroborators), so read
# whichever is present rather than assuming. Getting this wrong would silently
# fill the evidence columns with NA and strip the review interface of the very
# numbers a reviewer needs to judge a link.
pc_col <- function(d, ...) {
  for (nm in c(...)) if (nm %in% names(d)) return(d[[nm]])
  rep(NA_character_, nrow(d))
}

pc_within <- function(path, col_a, col_b, type, link_type, n_avail_fallback) {
  d <- pc_read(path)
  if (is.null(d) || !nrow(d)) {
    pclog("    (none: ", basename(path), " not present)")
    return(0L)
  }
  keep <- !pc_blank(d[[col_a]]) & !pc_blank(d[[col_b]])
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(0L)
  w  <- suppressWarnings(as.numeric(pc_col(d, "name_weight")))
  dd <- suppressWarnings(as.numeric(pc_col(d, "name_dist", "name_distance")))
  cc <- suppressWarnings(as.integer(pc_col(d, "corroborators", "n_corroborators")))
  # ON THE SCORE. The dedups apply link_score.min_keep themselves, so in
  # practice every row here already qualifies; re-reading it means this holds
  # whatever produced the file. A file with no link_score at all predates the
  # score and is not used: re-run that dedup step.
  ls <- suppressWarnings(as.numeric(pc_col(d, "link_score")))
  if (all(is.na(ls))) {
    pclog("      (no link_score column in ", basename(path),
          " -- not used; re-run that dedup step)")
    return(0L)
  }
  scored <- !is.na(ls) & ls >= PC_MIN_LINK_SCORE
  if (!all(scored)) {
    pclog("      (", sum(!scored), " of ", length(scored), " row(s) below ",
          PC_MIN_LINK_SCORE, " and not made into edges)")
    d <- d[scored, , drop = FALSE]
    w <- w[scored]; dd <- dd[scored]; cc <- cc[scored]
    if (!nrow(d)) return(0L)
  }
  # The dedup scripts now record how many fields they compared. Older outputs
  # do not, so fall back to the count for that script's comparison rather than
  # leaving it NA — an NA denominator would make every within-file link
  # unclassifiable by fraction downstream.
  na <- suppressWarnings(as.integer(pc_col(d, "n_corroborators_available")))
  if (all(is.na(na))) {
    pclog("      (no n_corroborators_available column — assuming ",
          n_avail_fallback, "; re-run the dedup step to record it)")
    na <- rep(as.integer(n_avail_fallback), nrow(d))
  }
  for (k in seq_len(nrow(d))) {
    add_edge(type, d[[col_a]][k], type, d[[col_b]][k], link_type,
             w[k], dd[k], cc[k], na[k])
  }
  nrow(d)
}

pclog("\n  Within-classification edges, read from the dedup outputs:")
# Fallbacks: the linelist dedup compares 5 fields (sex, zone, area, age,
# onset), the CT contact pass 5 (contact_sex, contact_age, zone, registration,
# source case), the CT source-case pass 1. Used only when reading a dedup
# output written before that count was recorded.
#
# The CT contact figure was 4 until the source case stopped being a blocking
# key and became a corroborator. A file written before that carries 4 in its
# own column and is read correctly; this fallback is for files older still.
n_w1 <- pc_within(file.path(epi_dir, "duplicate_candidates.csv"),
                  "alert_id_a", "alert_id_b", "ll", "within_linelist", 5L)
pclog("    within linelist: ", n_w1)
n_w2 <- pc_within(file.path(ct_dir, "contact_duplicate_candidates.csv"),
                  "contact_id_a", "contact_id_b", "ct_contact",
                  "within_ct_contact", 5L)
pclog("    within CT contacts: ", n_w2)
n_w3 <- pc_within(file.path(ct_dir, "source_case_duplicate_candidates.csv"),
                  "source_case_alert_id_a", "source_case_alert_id_b",
                  "ct_case", "within_ct_source_case", 1L)
pclog("    within CT source cases: ", n_w3)

edges <- do.call(rbind, edge_rows)
if (is.null(edges) || !nrow(edges)) {
  stop("No candidate links found at all — check that the dedup steps have run.",
       call. = FALSE)
}

# Drop self-edges. `RDC-AAA-BBB-26-0001234-MVE` appears on TWO linelist rows, so
# the linelist dedup correctly emits it as a duplicate pair — but both sides are
# the same identifier, and this graph links identifiers, not rows. Left in, it
# becomes a one-node "cluster", which is not a candidate person and would be an
# empty card in the review interface.
n_self <- sum(edges$node_a == edges$node_b)
if (n_self > 0) {
  pclog("\n  Dropping ", n_self, " self-edge(s) — same identifier on both sides ",
        "(two rows sharing one ID is a within-row duplicate, not a link).")
  edges <- edges[edges$node_a != edges$node_b, , drop = FALSE]
}

# Deduplicate: the same pair can arrive from two sources (e.g. an exact
# source==alert link and a fuzzy contact match landing on the same two
# identifiers). Keep the most certain.
key <- node_pair_key(edges$node_a, edges$node_b, "||")
# Keep the BEST-SCORING duplicate.
#
# The scores are not computed until after this point, so the two components
# are combined here rather than read off a column. Worth the repetition: the
# alternative is scoring first and de-duplicating second, which would leave
# the quantile log and the reject count describing rows that are about to be
# thrown away.
#
# exact_source_alert has no components and scores 1 by assertion, so it is
# given 1 here too -- otherwise the one link type that is certain would sort
# last among its own duplicates.
.pc_rank <- ifelse(
  edges$link_type == "exact_source_alert", 1,
  round(PC_SCORE_W[["name"]] *
          ifelse(is.na(edges$name_weight), 0,
                 pc_name_score(as.numeric(edges$name_weight))) +
        PC_SCORE_W[["corroborator"]] *
          corrob_score(edges$n_corroborators,
                       edges$n_corroborators_available), 4))
.pc_rank[is.na(.pc_rank)] <- 0
ord <- order(-.pc_rank)
edges <- edges[ord, , drop = FALSE][!duplicated(key[ord]), , drop = FALSE]

pclog("  TOTAL edges: ", nrow(edges))


# -------------------------  Name weight as a score out of 0.5  ----------------
# Applied HERE, once, rather than in each producer, because this is the first
# point at which every link type is in one table. The within-file edges carry a
# weight computed by the two dedup scripts and the cross-file ones a weight
# computed above -- all three now against the same pooled corpus with the same
# tokeniser, gate and collocation table, so one curve is the right thing to
# apply to all of them. Doing it per-producer would be three places to keep in
# step for no gain.
#
# SAME TWO CONFIG KEYS as the linelist, and the same log(N), so the number in
# this column equals the one duplicate_candidates.csv already carries for the
# same pair. It is recomputed rather than read across so that an edge from a
# producer that does not write the column still gets one.
#
# UNFITTED outside the linelist -- see the note in 3-deduplicate_contacts.R.
#
# Exact identifier links (link_type exact_source_alert) have no name weight at
# all (the identifier strings are
# equal; no name was compared), so they get NA rather than the score of a
# weight of zero, which would read as "the names disagree completely".
.pc_w <- suppressWarnings(as.numeric(edges$name_weight))
.pc_s <- ifelse(is.na(.pc_w), NA_real_, pc_name_score(.pc_w))
edges$name_score  <- round(.pc_s, 4)
edges$name_points <- round(PC_SCORE_W[["name"]] * .pc_s, 4)

# The corroborator half, from the two counts every producer records.
# corrob_score() is in 0-setup.R; see the config for why every field is
# currently worth 1.
.pc_c <- corrob_score(edges$n_corroborators, edges$n_corroborators_available)
edges$corrob_score  <- round(.pc_c, 4)
edges$corrob_points <- round(PC_SCORE_W[["corroborator"]] * .pc_c, 4)

# ONE NUMBER PER LINK, out of 1: what decides whether a link is kept
# (link_score.min_keep) and whether a cluster is auto-accepted (its weakest
# link against link_score.auto_accept).
#
# An exact identifier link scores 1 BY DEFINITION, not by arithmetic. It is the one link
# type where no name and no field was compared -- the two identifier strings
# are simply equal -- so its two halves stay NA and the total is asserted
# instead. Given a score of 1 rather than left NA so that a downstream filter
# on link_score needs no special case and cannot silently drop the strongest
# links in the file by treating a missing value as a low one.
#
# So `link_score == name_points + corrob_points` holds on every row EXCEPT
# these. Worth a test that says so, rather than a reader discovering it.
edges$link_score <- round(edges$name_points + edges$corrob_points, 4)
# Keyed on the link type: a link made because two identifier strings are equal.
edges$link_score[edges$link_type == "exact_source_alert"] <- 1

pclog("  Link score out of 1 (name ", PC_SCORE_W[["name"]],
      " + corroborator ", PC_SCORE_W[["corroborator"]], "): ",
      sum(!is.na(edges$link_score)), " of ", nrow(edges), " edge(s) scored; ",
      sum(edges$link_type == "exact_source_alert"),
      " exact identifier match(es) set to 1")
if (any(!is.na(edges$link_score))) {
  .pc_q <- stats::quantile(edges$link_score, c(0, .25, .5, .75, 1),
                           na.rm = TRUE)
  pclog("    min ", round(.pc_q[1], 3), " | q1 ", round(.pc_q[2], 3),
        " | median ", round(.pc_q[3], 3), " | q3 ", round(.pc_q[4], 3),
        " | max ", round(.pc_q[5], 3))
}


# -------------------------  Reject the weakest links  -------------------------
# BEFORE THE COMPONENTS ARE BUILT, and that ordering is the whole point.
#
# A cluster is a connected component of the edge set, so every edge kept here
# can merge two groups of identifiers. Filtering after clustering would leave
# clusters chained together through links this line has just called too weak
# to believe -- the picture a reviewer opens would rest on edges that are not
# in the file.
#
# The two dedup scripts apply the same cutoff to their own outputs, so the
# within-file edges arriving here have already passed it. Re-applying costs
# nothing and means this holds whatever produced the row.
.pc_keep <- !is.na(edges$link_score) & edges$link_score >= PC_MIN_LINK_SCORE
pclog("  Rejecting link_score < ", PC_MIN_LINK_SCORE, ": ",
      sum(!.pc_keep), " of ", nrow(edges), " edge(s) dropped, ",
      sum(.pc_keep), " kept")
edges <- edges[.pc_keep, , drop = FALSE]
if (!nrow(edges)) {
  stop("Every candidate link scored below link_score.min_keep (",
       PC_MIN_LINK_SCORE, "). Nothing to cluster.", call. = FALSE)
}


# -------------------------  Cluster  ------------------------------------------

comp <- pc_components(edges$node_a, edges$node_b)
edges$cluster_id <- match(unname(comp[edges$node_a]),
                          sort(unique(unname(comp))))
edges <- edges[order(edges$cluster_id, -edges$link_score, -edges$name_weight), , drop = FALSE]
edges$edge_id <- seq_len(nrow(edges))

# THE CLUSTER'S SCORE IS ITS WEAKEST LINK.
#
# Not the mean, and not the strongest. A cluster asserts that every identifier
# in it is one person, and it is held together by its edges -- so it is exactly
# as believable as the least believable thing holding it together. Averaging
# would let a chain of four certain links carry a fifth nobody believes, which
# is the shape of every over-merge in this file: A~B and B~C are solid, A~C is
# asserted by nobody and is what the reviewer would have rejected.
#
# Consequence worth stating: adding an identifier to a cluster can only lower
# its score, never raise it. That is the intended behaviour -- a bigger claim
# needs more evidence, not the average of what is already there.
.pc_cluster_min <- tapply(edges$link_score, edges$cluster_id, min, na.rm = TRUE)
edges$cluster_link_score <- round(
  as.numeric(.pc_cluster_min[as.character(edges$cluster_id)]), 4)


# -------------------------  Link type  ----------------------------------------
# Each link is described by two columns: `link_type` says which two
# populations the link joins, and `link_score` says how strong it is.
pclog("\n  Link types:")
for (k in sort(unique(edges$link_type))) {
  n <- sum(edges$link_type == k)
  pclog("    ", formatC(k, width = -30), n,
        "  (", round(100 * n / nrow(edges), 1), "%)",
        "  median score ",
        round(stats::median(edges$link_score[edges$link_type == k],
                            na.rm = TRUE), 3))
}

# Attributes for both endpoints, so the review interface can render a tile
# without joining back to the exports.
# How many entries in the pooled corpus carry EVERY token of this name.
#
# This is the number a reviewer actually wants, and the one the percentile
# cannot give them. `NDALAMBO LUCIE` sits at the 47th percentile, which
# reads as unremarkable, while only its own cluster carries that name.
# `MBATEKO JEAN DE DIEU` scored 20.50 -- above the tier-A bar, reading as
# distinctive -- while six entries share it exactly.
#
# It is a poor MATCHING score, deliberately not used as one: 82% of pooled
# entries have a combination nobody else shares, so it saturates and cannot
# rank. That is exactly what makes it a good thing to show, because a reviewer
# does not need it to rank, only to inform.
#
# COUNTED OVER IDENTIFIERS, WHICH IS NOT THE SAME AS PEOPLE. The corpus holds
# one entry per alert_id / contact_id / source_case_alert_id, so one person
# known to both files contributes two or three entries. A raw pooled count of 3
# for a name only one person has would therefore be true about identifiers and
# actively misleading about people -- which is the whole failure this column
# replaces. The cluster's own members are subtracted below.
#
# THE PREDICATE MUST BE THE SAME ON BOTH SIDES OF THE SUBTRACTION. Getting
# that wrong, then over-correcting, cost two attempts and both failures are
# worth recording because each looked right.
#
# Attempt 1 counted corpus entries CONTAINING every token of the name, and
# subtracted only cluster members whose name was IDENTICAL. Mismatched
# predicates: `BOSEMBO ANSELME` counted two `BOSEMBO ANSELME TALIANGA` records as
# sharing its name and, because neither was identical, reported both as
# strangers while they sat on the tiles either side. 843 of 1,075 nonzero
# counts were made entirely of the identifier's own cluster-mates.
#
# Attempt 2 made both sides require an IDENTICAL token set. Exact, symmetric,
# provably never negative -- and useless: 5,813 of 5,869 identifiers came out
# at zero, because the matcher puts identically-named identifiers in the same
# cluster by construction, so they are all subtracted away. Worse, `LA JOIE`
# read "no one else in these files has this name" when 28 records begin with
# it, which is the precise hazard the column exists to flag.
#
# So: CONTAINMENT on both sides. How many identifiers outside this cluster
# carry every part of this name. `LA JOIE` gets 25 (29 in the corpus, 4 of them
# already in its own cluster), `MBATEKO JEAN DE DIEU` gets 2 (6 in the corpus, 4
# in cluster 1824), and `NDALAMBO LUCIE` correctly gets 0 because its three
# identifiers are all on screen. 232 of the 5,869 identifiers in the graph are
# nonzero -- against attempt 1's 1,075, of which 843 were the identifier's own
# cluster-mates, and attempt 2's 56.
#
# NOT 28 and 4. Those are attempt 1's figures and an earlier draft of this
# comment quoted them, which is worth leaving a scar about: they differ from
# attempt 3's precisely because attempt 3 subtracts containment rather than
# equality, so a comment citing them describes a rule the code below does not
# implement.
#
# Asymmetric by design, in this direction: the SHORTER name is warned about the
# longer ones. `BOSEMBO ANSELME` is told that `BOSEMBO ANSELME TALIANGA` exists
# somewhere, because a two-token name really could be either record; the
# reverse is suppressed, because a record carrying the extra `TALIANGA` is not
# at risk of being mistaken for the barer name. One consequence to know: two
# tiles in the same cluster can therefore give different answers to what looks
# like the same question.
pc_name_key <- function(x) {
  vapply(pc_tokens(x), function(tk) {
    tk <- sort(unique(tk))
    if (!length(tk)) return(NA_character_)
    paste(tk, collapse = " ")
  }, character(1))
}

# Corpus identifiers whose token set CONTAINS every token of this name.
pc_share_all <- function(keys) {
  vapply(keys, function(k) {
    if (is.na(k)) return(NA_real_)
    tk <- strsplit(k, " ", fixed = TRUE)[[1]]
    as.numeric(length(Reduce(intersect, lapply(tk, function(t) .pc_idx[[t]]))))
  }, numeric(1), USE.NAMES = FALSE)
}

pc_attr_block <- function(prefix, pop) {
  if (is.null(pop) || !nrow(pop)) return(NULL)
  rar <- pc_name_rarity(pop$name)
  data.frame(node = paste0(prefix, ":", pop$id), name = pop$name,
             sex = pop$sex, age = pop$age, zone = pop$zone, area = pop$area,
             onset = pop$onset, classification = pop$classification,
             alert_type = pop$alert_type, last_followup = pop$last_followup,
             # Only contacts carry it; NULL for the other populations.
             source_case = if (is.null(pop$source_case)) NA_character_
                           else pop$source_case,
             name_rarity = round(rar, 2),
             name_rarity_pct = pc_rarity_pct(rar),
             name_key = pc_name_key(pop$name),
             stringsAsFactors = FALSE)
}
attr_of <- do.call(rbind, list(
  pc_attr_block("ll", pop_ll),
  pc_attr_block("ct_contact", pop_ct),
  pc_attr_block("ct_case", pop_cs)
))
attr_of <- attr_of[!duplicated(attr_of$node), , drop = FALSE]
lk <- function(node, col) {
  i <- match(node, attr_of$node)
  # as.character() so the numeric rarity columns come back as strings like the
  # rest; the review interface renders these, it does not compute with them.
  ifelse(is.na(i), NA_character_, as.character(attr_of[[col]][i]))
}
# NO "how many others could this be confused with" COLUMN, though it was the
# number originally proposed for the tile.
#
# It cannot be computed from this file. Every candidate pair the matcher
# generates becomes an edge, and clusters are the connected components of the
# edge set -- so an identifier's near-identical-name partners are ALL in its
# own cluster, by construction. The count would therefore only ever restate
# the cluster's size, never warn that a stranger elsewhere in the corpus
# carries the same name.
#
# name_share_all_outside does that job instead, without a threshold: how many
# identifiers outside this cluster carry every part of this name. Nonzero means
# a stranger elsewhere in the corpus could be confused with this record; zero
# means the only records containing this name are the ones on screen.
attr_of$name_share_all <- pc_share_all(attr_of$name_key)

.pc_cluster_of <- stats::setNames(
  c(edges$cluster_id, edges$cluster_id), c(edges$node_a, edges$node_b))
# Both endpoints, not just node_a: 2,737 identifiers never appear on the left.
# First-wins de-duplication is safe because a cluster is a connected component,
# so every edge touching an identifier carries the same cluster_id.
.pc_cluster_of <- .pc_cluster_of[!duplicated(names(.pc_cluster_of))]
.pc_key_of <- stats::setNames(attr_of$name_key, attr_of$node)
.pc_cluster_members <- split(names(.pc_cluster_of), unname(.pc_cluster_of))

# Cluster members carrying every token of this node's name -- the SAME
# containment predicate as pc_share_all(), applied to the cluster instead of
# the corpus. Clusters are small (largest 14 identifiers), so a plain loop is
# both fast enough and legible.
attr_of$name_share_all_outside <- local({
  # local(), not a bare brace: a top-level block leaks its working variables,
  # and `cl` here would collide with the loop variable named `cl` immediately
  # below.
  cl <- unname(.pc_cluster_of[attr_of$node])
  keys <- attr_of$name_key
  same <- vapply(seq_along(keys), function(i) {
    # An unusable name has no tokens, so nothing can contain it. NA, not 0:
    # pc_name_key() must return NA and not "" for these, because
    # all(character(0) %in% x) is TRUE and an empty key would match every
    # cluster member while its corpus count was zero -- a guaranteed negative.
    if (is.na(keys[i])) return(NA_real_)
    if (is.na(cl[i])) return(1)     # in no cluster: its own audience is itself
    mem <- .pc_cluster_members[[as.character(cl[i])]]
    # split() names its groups as.character() of the same integers, so this
    # lookup cannot miss while cl[i] is non-NA. If it ever did, [[ ]] on a list
    # returns NULL rather than erroring and the node would silently fail to
    # subtract even itself, so the miss is made loud instead.
    if (is.null(mem)) {
      stop("cluster ", cl[i], " has no member list, so ", attr_of$node[i],
           " cannot subtract its own cluster.", call. = FALSE)
    }
    mk <- .pc_key_of[mem]
    tk <- strsplit(keys[i], " ", fixed = TRUE)[[1]]
    sum(vapply(mk, function(k)
      !is.na(k) && all(tk %in% strsplit(k, " ", fixed = TRUE)[[1]]),
      logical(1)))
  }, numeric(1))
  out <- attr_of$name_share_all - same
  # NOT clamped with pmax(). Every cluster member counted here also satisfies
  # the corpus predicate, so this cannot go negative; if it ever does the two
  # sides have stopped agreeing and that is worth stopping for rather than
  # rounding away.
  if (any(!is.na(out) & out < 0)) {
    stop("name_share_all_outside came out negative, which is impossible ",
         "unless pc_share_all() and the cluster containment test disagree.",
         call. = FALSE)
  }
  out
})

for (cl in c("name", "sex", "age", "zone", "area", "classification",
             "alert_type", "last_followup", "source_case",
             "name_rarity", "name_rarity_pct", "name_share_all_outside")) {
  edges[[paste0(cl, "_a")]] <- lk(edges$node_a, cl)
  edges[[paste0(cl, "_b")]] <- lk(edges$node_b, cl)
}

# -------------------------  Near-miss sample  ---------------------------------
# For the Algorithm Tuning tab: pairs that scored just under link_score.min_keep
# and so were never made into links. Reviewers judge them same / different
# there; the share judged "same" is how much the cutoff misses.
#
# Pool = cross-file near misses caught in pc_link() + the dedups' near_miss
# files. Pairs already linked by ANY route are removed (an exact source-case
# link can cover a pair that also scored 0.35 by name), as are self-pairs.
#
# DRAWN BY A HASH OF THE link_id, lowest first, per link type. Not a seeded
# shuffle: the pool changes every night, and a reshuffle would swap most of
# the sample each build, stranding the judgements already made. With a hash a
# pair keeps its place for as long as it stays in the band.
pc_nm_within <- function(path, col_a, col_b, type, link_type) {
  d <- pc_read(path)
  if (is.null(d) || !nrow(d) || !all(c(col_a, col_b) %in% names(d))) return(NULL)
  d <- d[!pc_blank(d[[col_a]]) & !pc_blank(d[[col_b]]), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  data.frame(
    node_a = paste0(type, ":", trimws(d[[col_a]])),
    node_b = paste0(type, ":", trimws(d[[col_b]])),
    link_type = link_type,
    link_score = suppressWarnings(as.numeric(pc_col(d, "link_score"))),
    name_weight = suppressWarnings(as.numeric(pc_col(d, "name_weight"))),
    n_corroborators = suppressWarnings(as.integer(pc_col(d, "corroborators", "n_corroborators"))),
    n_corroborators_available = suppressWarnings(as.integer(pc_col(d, "n_corroborators_available"))),
    stringsAsFactors = FALSE)
}

pc_hash <- function(x) {
  vapply(as.character(x), function(s) {
    h <- 0
    for (cp in utf8ToInt(enc2utf8(s))) h <- (h * 31 + cp) %% 2147483647
    h
  }, numeric(1), USE.NAMES = FALSE)
}

near_misses <- local({
  pool <- do.call(rbind, c(
    .pc_nm$rows,
    list(pc_nm_within(file.path(epi_dir, "near_miss_candidates.csv"),
                      "alert_id_a", "alert_id_b", "ll", "within_linelist"),
         pc_nm_within(file.path(ct_dir, "contact_near_miss_candidates.csv"),
                      "contact_id_a", "contact_id_b", "ct_contact",
                      "within_ct_contact"),
         pc_nm_within(file.path(ct_dir, "source_case_near_miss_candidates.csv"),
                      "source_case_alert_id_a", "source_case_alert_id_b",
                      "ct_case", "within_ct_source_case"))))
  if (is.null(pool) || !nrow(pool)) return(NULL)
  pool <- pool[pool$node_a != pool$node_b &
               !is.na(pool$link_score) &
               pool$link_score >= PC_NEAR_MISS_MIN &
               pool$link_score <  PC_MIN_LINK_SCORE, , drop = FALSE]
  if (!nrow(pool)) return(NULL)
  pool$link_id <- node_pair_key(pool$node_a, pool$node_b)
  pool <- pool[order(-pool$link_score), , drop = FALSE]
  pool <- pool[!duplicated(pool$link_id), , drop = FALSE]
  pool <- pool[!pool$link_id %in% node_pair_key(edges$node_a, edges$node_b), ,
               drop = FALSE]
  if (!nrow(pool)) return(NULL)
  # Band size per link type, carried on every row so a rate can be weighted.
  pool$n_in_band_type <- as.integer(table(pool$link_type)[pool$link_type])
  pool$hash <- pc_hash(pool$link_id)
  s <- do.call(rbind, lapply(split(pool, pool$link_type), function(p)
    utils::head(p[order(p$hash), , drop = FALSE], PC_NEAR_MISS_N)))
  s$hash <- NULL
  s
})

if (is.null(near_misses)) {
  pclog("\n  Near misses (", PC_NEAR_MISS_MIN, " to under ", PC_MIN_LINK_SCORE,
        "): none found. If the dedups have not been re-run since near misses ",
        "were added, their near_miss files do not exist yet.")
} else {
  pclog("\n  Near misses (", PC_NEAR_MISS_MIN, " to under ", PC_MIN_LINK_SCORE,
        "), sampled up to ", PC_NEAR_MISS_N, " per link type for Algorithm Tuning:")
  .nm_t <- near_misses[!duplicated(near_misses$link_type), , drop = FALSE]
  for (k in seq_len(nrow(.nm_t)))
    pclog("    ", .nm_t$link_type[k], ": ", sum(near_misses$link_type == .nm_t$link_type[k]),
          " sampled of ", .nm_t$n_in_band_type[k])
  # The same tile columns as the edges, so the tab can draw a pair with the
  # shared graph code. id_type_* for the same reason.
  near_misses$id_type_a <- sub(":.*$", "", near_misses$node_a)
  near_misses$id_type_b <- sub(":.*$", "", near_misses$node_b)
  near_misses$cluster_id <- paste0("NM-", seq_len(nrow(near_misses)))
  near_misses$edge_id <- paste0("nm", seq_len(nrow(near_misses)))
  for (cl in c("name", "sex", "age", "zone", "area", "classification",
               "alert_type", "last_followup", "source_case",
               "name_rarity", "name_rarity_pct", "name_share_all")) {
    near_misses[[paste0(cl, "_a")]] <- lk(near_misses$node_a, cl)
    near_misses[[paste0(cl, "_b")]] <- lk(near_misses$node_b, cl)
  }
  # Each side's current cluster, if it has one: "is this person already linked
  # to someone else?" is part of judging a near miss.
  near_misses$in_cluster_a <- unname(.pc_cluster_of[near_misses$node_a])
  near_misses$in_cluster_b <- unname(.pc_cluster_of[near_misses$node_b])
}

# Empty, for the review interface to fill. An edge is the unit of decision:
# breaking one splits its cluster.
edges$review_decision <- NA_character_
edges$review_reason   <- NA_character_
edges$reviewer        <- NA_character_

pclog("  Clusters: ", length(unique(edges$cluster_id)))


# -------------------------  Cluster-level flags  ------------------------------
# Advisory only. They mark clusters to open FIRST, and are computed
# independently of the evidence that built the cluster -- an 11-member cluster
# spanning six health zones is suspect however good each individual link looked.

members <- unique(rbind(
  data.frame(cluster_id = edges$cluster_id, node = edges$node_a,
             id_type = edges$id_type_a, stringsAsFactors = FALSE),
  data.frame(cluster_id = edges$cluster_id, node = edges$node_b,
             id_type = edges$id_type_b, stringsAsFactors = FALSE)
))

# Split once: `edges[edges$cluster_id == cid, ]` inside the loop would rescan
# all ~3,000 edges for each of ~2,500 clusters.
edges_by_cluster <- split(seq_len(nrow(edges)), edges$cluster_id)

clusters <- do.call(rbind, lapply(split(members, members$cluster_id), function(m) {
  cid <- m$cluster_id[1]
  e   <- edges[edges_by_cluster[[as.character(cid)]], , drop = FALSE]
  sx  <- lk(m$node, "sex");  sx <- sx[!pc_blank(sx)]
  ag  <- suppressWarnings(as.numeric(lk(m$node, "age"))); ag <- ag[!is.na(ag)]
  zn  <- lk(m$node, "zone"); zn <- zn[!pc_blank(zn)]
  nmz <- lk(m$node, "name")

  rp  <- suppressWarnings(as.numeric(lk(m$node, "name_rarity_pct")))
  mrp <- if (any(is.na(rp))) NA_real_ else min(rp)

  # The widest age gap in the cluster, and NA -- never 0 -- when it cannot be
  # measured. `ag` has already dropped missing ages, so a cluster where nobody
  # (or only one identifier) has an age would otherwise compute a span of 0 and
  # sail through the age arm of the floor on no evidence at all. That is the
  # exact direction this must not fail in, so the two cases are kept distinct:
  # `age_span` stays 0 for the existing flags and the review tiles, and
  # `age_gap` is what the auto-accept rule reads.
  agp <- if (length(ag) > 1) max(ag) - min(ag) else NA_real_

  # A confirmed case and a non-case in the same cluster: a veto on
  # auto-accepting it (see auto_accept below). Computed once here so the
  # decision and its reason cannot be given different inputs.
  ccon <- pc_class_contradiction(e)

  # THE CLUSTER'S SCORE: its weakest link. Computed here rather than inline so
  # the decision, the reason and the column cannot be handed different numbers
  # -- the same reason mrp and ccon are lifted out above.
  #
  # na.rm because exact identifier links carry no component scores; their
  # link_score is 1 by assertion, so they never lower a minimum anyway. A
  # cluster of nothing but exact links therefore scores 1, which is right.
  lsc_min <- suppressWarnings(min(e$link_score, na.rm = TRUE))
  if (!is.finite(lsc_min)) lsc_min <- NA_real_

  data.frame(
    cluster_id = cid,
    n_identifiers = nrow(m),
    n_alert_id = sum(m$id_type == "ll"),
    n_contact_id = sum(m$id_type == "ct_contact"),
    n_source_case_alert_id = sum(m$id_type == "ct_case"),
    n_edges = nrow(e),
    # THE WEAKEST LINK HOLDING IT TOGETHER. Recomputed from this cluster's own
    # edges rather than read off the column, so the two would have to be wrong
    # in the same way to agree. A cluster is as believable as the least
    # believable thing holding it together -- see the note where the column is
    # built.
    link_score_min = round(lsc_min, 4),
    link_score_max = round(suppressWarnings(max(e$link_score, na.rm = TRUE)), 4),
    # "One of these links is two identical identifier strings" -- a fact
    # about the cluster a reviewer wants, read off the link type.
    has_exact_link = any(e$link_type == "exact_source_alert"),
    # Lowest name-rarity percentile in the cluster -- the rarity floor is
    # applied to the WEAKEST name, not the average, because one generic name
    # is enough to make a merge doubtful however distinctive the others are.
    #
    # NA if any identifier could not be scored, which blocks the A1 rule
    # rather than silently accepting on partial evidence.
    min_name_rarity_pct = mrp,
    # THE DECISION, and now the only one: a cluster is accepted without a
    # human opening it when its WEAKEST link clears the bar and nothing in it
    # contradicts itself.
    #
    # Replaces pc_review_bucket() and the eleven buckets on 2026-09-23. The
    # measurement is in the README ("what the tiers see that the link score
    # does not"); in short, the buckets and the score openly disagreed. A1
    # accepted 195 clusters scoring 0.4-0.5 unreviewed while P5_routine sent
    # 359 clusters from that same band to review -- three fates for one weight
    # of evidence, decided by a rule that does not read it. Neither A1
    # condition did: "every judged link is tier A" is satisfied by links
    # scoring 0.41, and the floor tested name rarity or age separately rather
    # than the evidence as a whole.
    #
    # THE CONTRADICTION IS KEPT as a veto, and it is the one thing the score
    # cannot see. A cluster holding both a confirmed case and a non-case is not
    # weakly evidenced -- it may be very strongly evidenced -- it is making a
    # claim that lands on the case count. 7 clusters above 0.8 carry one. They
    # go to review however well they score.
    auto_accept = !is.na(lsc_min) && lsc_min >= PC_AUTO_ACCEPT_SCORE && !ccon,
    # Why it was accepted or held, in one sentence, so the queue explains
    # itself without a reader reconstructing the rule.
    #
    # There IS a weakest-link column now (link_score_min above), which there
    # deliberately was not before: PC_LINK_CLASSES was an enumeration rather
    # than a risk ordering, so its highest index was not "weakest". A score is
    # an ordering, so the minimum over a cluster's links means something.
    decision_reason = if (ccon) {
      "a confirmed case and a non-case cannot be one person"
    } else if (is.na(lsc_min)) {
      "no link in this cluster carries a score"
    } else if (lsc_min >= PC_AUTO_ACCEPT_SCORE) {
      paste0("every link scores at least ", round(lsc_min, 2))
    } else {
      paste0("weakest link scores ", round(lsc_min, 2), ", under ",
             PC_AUTO_ACCEPT_SCORE)
    },
    # Which populations this cluster joins. Replaces `link_classes`, which
    # listed L0-L6; the link type is the fact that class was derived from.
    link_types = paste(sort(unique(e$link_type)), collapse = " | "),
    n_distinct_sexes = length(unique(sx)),
    age_span = if (length(ag) > 1) max(ag) - min(ag) else 0L,
    # What the auto-accept floor read: NA where fewer than two identifiers
    # carry an age, so "unknown" is never mistaken for "agrees".
    age_gap_measured = agp,
    # Recorded so the rule is auditable from the file alone: which clusters
    # were demoted, and which of those would otherwise have been merged.
    class_contradiction = ccon,
    n_health_zones = length(unique(zn)),
    flag_multiple_sexes = length(unique(sx)) > 1,
    flag_wide_age_span  = length(ag) > 1 && (max(ag) - min(ag)) > PC_MAX_AGE_SPAN,
    flag_many_zones     = length(unique(zn)) > 2,
    flag_large_cluster  = nrow(m) > PC_LARGE_CLUSTER,
    names_in_cluster = paste(unique(nmz), collapse = " | "),
    review_decision = NA_character_, reviewer = NA_character_,
    stringsAsFactors = FALSE
  )
}))
# Ordered as the REVIEW QUEUE, and the file is written in that order so it
# reads the same way the interface walks it.
#
#   0  held for review, contradiction     a claim about the case count
#   1  held for review, weak link         ordered strongest first
#   2  accepted without review            audited from the back
#
# THE STRONGEST HELD CLUSTER LEADS, which is the opposite of the instinct to
# show the worst first. A cluster at 0.79 is one a reviewer can settle and is
# one decision away from a merge; a cluster at 0.41 is almost always two
# people with a shared surname, and a queue that opens with two hundred of
# those trains a reviewer to click through. The eleven buckets encoded this
# by kind (cross-namespace first, routine last); a score encodes it by
# distance from the bar, which is the same intent without the taxonomy.
#
# CONTRADICTIONS LEAD EVERYTHING. They are not weakly evidenced -- most score
# well -- and what they need is a judgement no score can supply.
#
# The accepted tail stays IN the file rather than being removed from it, for
# the reason the old bucket order also kept it: a reviewer can walk it, and an
# override there is recorded as evidence against the rule.
clusters$queue_rank <- ifelse(
  clusters$class_contradiction, 0L,
  ifelse(clusters$auto_accept, 2L, 1L))

clusters <- clusters[order(clusters$queue_rank,
                           -clusters$link_score_min,
                           -clusters$n_identifiers,
                           clusters$cluster_id), , drop = FALSE]

n_auto   <- sum(clusters$auto_accept)
n_ccon   <- sum(clusters$class_contradiction)
n_review <- nrow(clusters) - n_auto
n_veto   <- sum(clusters$class_contradiction &
                  !is.na(clusters$link_score_min) &
                  clusters$link_score_min >= PC_AUTO_ACCEPT_SCORE)

pclog("\n  Review queue:")
pclog("    -> ", n_auto, " cluster(s) accepted without review (every link >= ",
      PC_AUTO_ACCEPT_SCORE, ")")
pclog("    -> ", n_review, " cluster(s) held for review")
pclog("    -> of those, ", n_ccon, " hold a confirmed case and a non-case; ",
      n_veto, " would otherwise have been accepted on score alone")
pclog("    Clusters are ordered strongest-held-first: a cluster just under ",
      "the bar is one a reviewer can settle, where the weakest are almost ",
      "all two people sharing a surname. Contradictions lead regardless of ",
      "score. Accepted clusters keep their place at the back so the rule ",
      "stays auditable.")


# -------------------------  Calibration sample  -------------------------------
# A DEFINED SET OF CLUSTERS FOR HUMANS TO LABEL, so the scoring can be tuned
# against judgement rather than against itself. Every threshold in this
# pipeline was set against the share of pairs with 4-5 corroborator fields
# agreeing, which was independent evidence until those same fields went into
# the score. Real keep/break decisions are not.
#
# STRATIFIED BY SCORE BAND. A flat shuffle would spend about half its draws in
# 0.4-0.5, which holds 1,952 of 4,192 clusters and is the band whose answer is
# least in doubt, and would return roughly fifty labels above 0.8, where a
# wrong auto-accept is most expensive. Equal draws per band buy precision
# where the decision actually sits. THE BAND MUST BE CARRIED WITH EVERY LABEL:
# population rates are only recoverable afterwards by weighting each band by
# its true size, and that weighting is impossible if the band is lost.
#
# SEEDED, and the seed lives in the config, so "the first 500" names one set
# of clusters rather than whatever a session happened to show. The draw is
# shuffled after stratifying so reviewers do not work band by band and read
# the difficulty off the order.
#
# Auto-accepted clusters are excluded: they are not in the queue, so nobody
# would reach them.
PC_SAMPLE_SEED     <- match_param("review_sample.seed")
PC_SAMPLE_PER_BAND <- match_param("review_sample.per_band")
PC_SAMPLE_BAND_W   <- match_param("review_sample.band_width")

# The draw itself is deferred to pc_write_sample(), called with the other
# writes: out_dir is not built until the run stem is resolved, near the end of
# this script, and the sample has to land beside the files it describes.
pc_write_sample <- function(out_dir) {
  elig <- clusters[!clusters$auto_accept & !is.na(clusters$link_score_min), ,
                   drop = FALSE]
  if (!nrow(elig)) {
    pclog("\n  Calibration sample: no eligible clusters.")
    return(invisible(NULL))
  }
  band <- floor(elig$link_score_min / PC_SAMPLE_BAND_W) * PC_SAMPLE_BAND_W

  # set.seed() moves the global generator, which nothing else here uses --
  # but restoring it means adding a sample can never shift another result.
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(PC_SAMPLE_SEED)
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)

  picked <- unlist(lapply(split(seq_len(nrow(elig)), band), function(ix) {
    if (length(ix) <= PC_SAMPLE_PER_BAND) ix
    else sample(ix, PC_SAMPLE_PER_BAND)
  }), use.names = FALSE)
  picked <- sample(picked)          # interleave the bands

  out <- data.frame(
    sample_order = seq_along(picked),
    cluster_id = elig$cluster_id[picked],
    band = sprintf("%.1f", band[picked]),
    link_score_min = elig$link_score_min[picked],
    n_identifiers = elig$n_identifiers[picked],
    n_edges = elig$n_edges[picked],
    # The size of the band this cluster was drawn FROM, so a rate measured on
    # the sample can be weighted back to the population without recounting.
    band_population = as.integer(table(band)[as.character(band[picked])]),
    stringsAsFactors = FALSE
  )
  pc_write_csv(out, file.path(out_dir, "review_sample.csv"))

  pclog("\n  Calibration sample: ", nrow(out), " cluster(s), seed ",
        PC_SAMPLE_SEED, ", up to ", PC_SAMPLE_PER_BAND, " per ",
        PC_SAMPLE_BAND_W, " band")
  # Population counts read from the SAME table the CSV column is built from.
  # This printed "80 of 0" for two bands, because it compared `band` to
  # as.numeric(k) and floor(0.65 / 0.1) * 0.1 is 0.6000000000000001, which is
  # not 0.6. The draw and the CSV were always right -- both go through
  # as.character(), which rounds the representation away -- so the bug was
  # only ever in this line. Float equality on a derived number, doing what it
  # always does.
  pop <- table(band)
  tb  <- table(out$band)
  for (k in names(tb))
    pclog("    ", k, "-", sprintf("%.1f", as.numeric(k) + PC_SAMPLE_BAND_W),
          ": ", as.numeric(tb[k]), " of ",
          if (k %in% names(pop)) as.numeric(pop[k]) else NA,
          " in the population")
  pclog("    -> review_sample.csv. Carry `band` through to the labels: ",
        "without it the sample cannot be weighted back to the population.")
  invisible(NULL)
}

# -------------------------  Review sets  --------------------------------------
# THREE SETS A REVIEWER CAN BE POINTED AT, chosen in the app by
# matching.review_queue.set:
#
#   sample           review_sample.csv           the calibration draw above
#   contact_tracing  review_contact_tracing.csv  clusters holding a CT contact
#                                                or source case
#   all              review_all.csv              every cluster with a link to
#                                                judge
#
# "A LINK TO JUDGE" is the same test the review app applies: not auto-accepted,
# and at least one link that is not exact_source_alert. A cluster whose only
# link is a source case matched to a linelist row BY ID has nothing a human
# can add -- both sides carry the same identifier string -- so it is left out
# of every set. A cluster with an exact link AND a fuzzy one stays in: the
# fuzzy link still needs a decision.
#
# Both files keep the clusters' own order (contradictions first, then
# strongest held first), and carry only identifiers and counts -- no names.
pc_write_review_sets <- function(out_dir) {
  judge <- !clusters$auto_accept &
    clusters$link_types != "exact_source_alert"
  has_ct <- clusters$n_contact_id > 0 | clusters$n_source_case_alert_id > 0

  cols <- c("cluster_id", "link_score_min", "n_identifiers", "n_edges",
            "n_alert_id", "n_contact_id", "n_source_case_alert_id",
            "class_contradiction", "link_types")
  # A contact matched to a linelist record is the link the CT review exists
  # for -- it says a traced contact became a case -- so those clusters lead the
  # CT set. Within each group the clusters' own order is kept (contradictions
  # first, then strongest held first). The review tab applies the same order.
  has_c2l <- grepl("contact_to_linelist", clusters$link_types, fixed = TRUE)
  one <- function(keep, file, c2l_first = FALSE) {
    ix  <- which(keep)
    if (c2l_first) ix <- ix[order(!has_c2l[ix], seq_along(ix))]
    out <- clusters[ix, cols, drop = FALSE]
    out <- cbind(queue_order = seq_len(nrow(out)), out,
                 has_contact_linelist_link = has_c2l[ix])
    pc_write_csv(out, file.path(out_dir, file))
    nrow(out)
  }
  n_ct  <- one(judge & has_ct, "review_contact_tracing.csv", c2l_first = TRUE)
  n_all <- one(judge, "review_all.csv")

  pclog("\n  Review sets:")
  pclog("    contact_tracing: ", n_ct, " of ", sum(has_ct),
        " cluster(s) holding a CT contact or source case (",
        sum(has_ct & !judge), " dropped: exact ID link only); ",
        sum(judge & has_ct & has_c2l), " with a contact-linelist link, listed first")
  pclog("    all:             ", n_all, " cluster(s) with a link to judge")
  pclog("    -> review_contact_tracing.csv, review_all.csv. Pick the set in ",
        "matching.review_queue.set.")
  invisible(NULL)
}

# WHAT AN AUTO-ACCEPT CUTOFF WOULD TAKE, at every tenth. Printed as a
# cumulative count rather than a histogram because that is the question being
# asked of it: pick a number, and this says how many clusters it accepts and
# how many identifiers that commits you to merging.
pclog("\n  Clusters by their weakest link (the cluster's score):")
for (.b in seq(0.4, 1.0, by = 0.1)) {
  .at <- clusters$link_score_min >= .b - 1e-9
  pclog("    >= ", format(.b, nsmall = 1), ": ", sum(.at), " cluster(s), ",
        sum(clusters$n_identifiers[.at]), " identifier(s)")
}
pclog("    A cluster scores its WEAKEST link, so these are clusters where ",
      "EVERY link clears the bar. Nothing is accepted on this yet.")

pclog("\n  Cluster sizes:")
tb <- table(clusters$n_identifiers)
for (k in names(tb)) pclog("    ", k, " identifiers: ", as.numeric(tb[k]))
pclog("  Flagged for priority review:")
pclog("    more than one recorded sex: ", sum(clusters$flag_multiple_sexes))
pclog("    age span > ", PC_MAX_AGE_SPAN, " years: ", sum(clusters$flag_wide_age_span))
pclog("    spanning > 2 health zones: ", sum(clusters$flag_many_zones))
pclog("    more than ", PC_LARGE_CLUSTER, " identifiers: ", sum(clusters$flag_large_cluster))

pclog("\n  Clusters spanning more than one classification (the point of this step):")
multi <- clusters[(clusters$n_alert_id > 0) +
                  (clusters$n_contact_id > 0) +
                  (clusters$n_source_case_alert_id > 0) > 1, , drop = FALSE]
pclog("    ", nrow(multi), " of ", nrow(clusters))


# -------------------------  Node name tokens  ---------------------------------
# One row per (identifier, name token), with that token's rarity.
#
# This is what lets the review interface show WHY a cluster exists. A cluster
# held together by a token 1,072 people share is a different proposition from
# one held together by a token nobody else has, and the graph cannot express
# that: both look like a line between two tiles.
#
# The tokens come from the same pc_tokens() the matching used, and the IDF from
# the same PC_IDF, so the panel reports the numbers that actually produced the
# edges rather than a second opinion about them.
#
# CONFIDENTIALITY: unlike person_candidate_node_dates.csv, this file DOES
# contain patient names -- a name token IS a name fragment, and a token with
# n_entries = 1 is one person's name. Handle it exactly like
# person_candidate_edges.csv and dhis2_processed_linelist_names.csv. It must
# not go into any ctsafe or shared export.
#
# Bands as fractions of log(N), the convention used for every other threshold
# here. PC_SINGLE_TOKEN_MULT (0.76) is already the point at which ONE token is
# considered rare enough to propose a match on its own, so it is the natural
# boundary for "very rare"; half of log(N) separates uncommon from common.
PC_TOKEN_UNCOMMON_MULT <- 0.50

pc_token_band <- function(idf) {
  ifelse(idf >= PC_SINGLE_TOKEN_MULT * PC_IDF_UNSEEN, "very rare",
         ifelse(idf >= PC_TOKEN_UNCOMMON_MULT * PC_IDF_UNSEEN,
                "uncommon", "common"))
}

pclog("\n  Name tokens per identifier, for the rarity panel:")
pclog("    bands: very rare >= ", round(PC_SINGLE_TOKEN_MULT * PC_IDF_UNSEEN, 2),
      " | uncommon >= ", round(PC_TOKEN_UNCOMMON_MULT * PC_IDF_UNSEEN, 2),
      " | max possible ", round(PC_IDF_UNSEEN, 2))

# For the panel's context: a token's FREQUENCY RANK among every distinct token
# in the pooled corpus (1 = the most common), and the corpus size itself, so
# "1,072 in corpus" can be read against how many people there are in total.
# Ties share the best rank. An unseen token ranks after every seen one.
PC_IDF_SORTED <- sort(as.numeric(PC_IDF))
pc_freq_rank <- function(idf) findInterval(idf - 1e-9, PC_IDF_SORTED) + 1L

tok_rows <- list()
for (blk in list(list(p = "ll", pop = pop_ll),
                 list(p = "ct_contact", pop = pop_ct),
                 list(p = "ct_case", pop = pop_cs))) {
  pop <- blk$pop
  if (is.null(pop) || !nrow(pop)) next
  toks <- lapply(pc_tokens(pop$name), unique)
  keep <- lengths(toks) > 0
  if (!any(keep)) next
  nd <- rep(paste0(blk$p, ":", pop$id)[keep], lengths(toks[keep]))
  tk <- unlist(toks[keep], use.names = FALSE)
  idf <- pc_idf(tk)
  tok_rows[[length(tok_rows) + 1L]] <- data.frame(
    node = nd, id_type = blk$p, token = tk,
    idf = round(idf, 4),
    # How many identifiers in the WHOLE pooled corpus carry this token. The
    # count a reviewer actually wants: "1,072 people share this name" lands
    # where "idf 3.57" does not.
    n_entries = round(PC_N / exp(idf)),
    rarity_band = pc_token_band(idf),
    freq_rank = pc_freq_rank(idf),
    n_tokens_corpus = length(PC_IDF_SORTED),
    corpus_n = PC_N,
    stringsAsFactors = FALSE)
}
node_tokens <- do.call(rbind, tok_rows)
if (is.null(node_tokens)) {
  node_tokens <- data.frame(node = character(0), id_type = character(0),
                            token = character(0), idf = numeric(0),
                            n_entries = numeric(0), rarity_band = character(0),
                            freq_rank = integer(0), n_tokens_corpus = integer(0),
                            corpus_n = numeric(0),
                            stringsAsFactors = FALSE)
}
# Only identifiers in the graph are needed; the rest are in no cluster.
# Plus the near-miss sample's identifiers, which the Algorithm Tuning tab draws
# with the same panels.
node_tokens <- node_tokens[node_tokens$node %in%
                             unique(c(edges$node_a, edges$node_b,
                                      near_misses$node_a, near_misses$node_b)), ,
                           drop = FALSE]
node_tokens <- node_tokens[order(node_tokens$node, -node_tokens$idf), , drop = FALSE]

pclog("    ", nrow(node_tokens), " token instance(s), ",
      length(unique(node_tokens$token)), " distinct token(s)")
for (b in c("very rare", "uncommon", "common"))
  pclog("      ", formatC(b, width = -12),
        sum(node_tokens$rarity_band == b), " instance(s)")


# -------------------------  Node date events  ---------------------------------
# One row per (identifier, event, date), long format, for the review timeline.
#
# LONG, not wide: the alternative is another nine `_a`/`_b` column pairs on the
# edge table, which would double its width to carry data that is per NODE, not
# per link -- and a contact has up to 26 follow-up dates, which no fixed set of
# columns can hold.
#
# Written by the pipeline rather than read from the exports by the app, for the
# same reason the rarity score is: this script already has both exports open and
# knows how to parse their dates, and a second date parser in the Shiny app
# would be a second thing to keep in step.
#
# Only nodes that appear in the candidate graph are written; the other ~31,000
# linelist identifiers are in no cluster and would never be plotted.
#
# CONFIDENTIALITY: this file holds identifiers and dates, NO names. That makes
# it less sensitive than person_candidate_edges.csv, not insensitive -- dates
# of onset are patient data. It stays in person_matching/ with the rest.

# `kind` drives how a marker is drawn; `label` is what a reviewer reads.
#
# reporting_date, date_of_notification and investigation_datetime are all kept
# even though they look redundant: measured on this export they differ from one
# another in 52.5% and 53.5% of rows respectively, so collapsing them would
# lose real signal about when a case actually surfaced.
PC_LL_EVENTS <- list(
  list(col = "date_of_symptom_onset",     lab = "Symptom onset",   kind = "onset"),
  list(col = "date_of_sample_collection", lab = "Sample collected", kind = "lab"),
  list(col = "lab_analysis_date",         lab = "Lab result",      kind = "lab"),
  list(col = "reporting_date",            lab = "Reported",        kind = "admin"),
  list(col = "date_of_notification",      lab = "Notified",        kind = "admin"),
  list(col = "investigation_datetime",    lab = "Investigated",    kind = "admin"),
  # Admission / discharge: cleaning names them date_of_hospitalisation_start /
  # _end since 2026-10-27 (the "MVE - PEC - Date d'admission" / "Date de
  # sortie" columns, which replaced the HO-1 hospitalisation dates on 15 Jul,
  # were added to the rename map). Exports cleaned before that still carry the
  # made-up names pec_date_dadmission / pec_date_de_sortie. The first column
  # of `col` that the export has is used.
  list(col = c("date_of_hospitalisation_start", "pec_date_dadmission"),
       lab = "Admitted",   kind = "care"),
  list(col = c("date_of_hospitalisation_end", "pec_date_de_sortie"),
       lab = "Discharged", kind = "care")
)
# `first_seen_date` is deliberately NOT here.
#
# It is when the pipeline first saw the row, not something that happened to the
# patient, and it was the latest event on 87% of linelist identifiers -- so it
# sat at the right-hand end of nearly every lane, drawing the eye and earning a
# label, while saying nothing about whether two identifiers are one person. It
# was 5,474 of 33,419 events (16%) for no decision value. Provenance belongs in
# the registry, not on a clinical timeline.

pclog("\n  Node date events for the review timeline:")

# ---------------------------------------------------------------------------
# PLAUSIBLE DATE WINDOW
# ---------------------------------------------------------------------------
# A single absurd date destroys the timeline for its whole cluster. The axis is
# scaled to the range of the events on it, so one onset recorded as 1997-05-04
# spreads 29 years across the panel and squeezes the ten real events into a few
# pixels. Measured on this export, 13 events are affected -- 13 of 33,434, but
# they ruin 13 of 2,658 clusters outright, and every one of them is a typo
# rather than data (an `investigation_datetime` of 261-02-26, onsets in 2020
# and 2021 for a 2026 outbreak).
#
# The window is DERIVED from the export rather than hard-coded to 2026, so it
# stays right for the next outbreak. reporting_date is the anchor because it is
# 100% populated and is when the row entered the system: an onset up to a year
# before the first report is clinically plausible, one before that is not.
#
# These events are dropped from the TIMELINE TABLE only. Nothing is changed in
# the export, and every dropped value is logged so it reads as a data-quality
# finding rather than a silent deletion.
PC_DATE_BACK_DAYS <- 365L
PC_DATE_FWD_DAYS  <- 31L

pc_anchor <- pc_as_date(ll$reporting_date)
pc_anchor <- pc_anchor[!is.na(pc_anchor)]
if (length(pc_anchor)) {
  PC_DATE_MIN <- min(pc_anchor) - PC_DATE_BACK_DAYS
  PC_DATE_MAX <- max(pc_anchor) + PC_DATE_FWD_DAYS
} else {
  # No usable reporting_date at all: accept everything rather than invent a
  # window, and say so.
  PC_DATE_MIN <- as.Date("1900-01-01")
  PC_DATE_MAX <- as.Date("2999-12-31")
  pclog("    WARNING: no usable reporting_date, so no date plausibility check")
}
pclog("    plausible window (from reporting_date): ", format(PC_DATE_MIN),
      " to ", format(PC_DATE_MAX))

# Near-miss identifiers too, for the Algorithm Tuning tab's timeline.
nodes_in_graph <- unique(c(edges$node_a, edges$node_b,
                          near_misses$node_a, near_misses$node_b))
ev <- list()
.pc_rejected <- list()
pc_add_events <- function(node, id_type, id, label, kind, dates) {
  dates <- dates[!is.na(dates)]
  if (!length(dates)) return(invisible(NULL))
  ok <- dates >= PC_DATE_MIN & dates <= PC_DATE_MAX
  if (any(!ok)) {
    .pc_rejected[[length(.pc_rejected) + 1L]] <<- data.frame(
      node = node, event = label,
      date = format(sort(unique(dates[!ok]))), stringsAsFactors = FALSE)
    dates <- dates[ok]
    if (!length(dates)) return(invisible(NULL))
  }
  # format(), not as.character(): as.character() on a Date with a year below
  # 1000 drops the leading zero, so 0261-02-26 is written as "261-02-26" --
  # which is no longer ISO 8601 and breaks anything that reads it back
  # strictly. It cannot happen now the window excludes such dates, but the
  # formatting should not depend on that.
  ev[[length(ev) + 1L]] <<- data.frame(
    node = node, id_type = id_type, id = id, event = label, kind = kind,
    date = format(sort(unique(dates)), "%Y-%m-%d"), stringsAsFactors = FALSE)
  invisible(NULL)
}

# ---- linelist identifiers ----
ll_by_id <- split(seq_len(nrow(ll)), trimws(as.character(ll$alert_id)))
n_ll_ev <- 0L
for (nd in nodes_in_graph[startsWith(nodes_in_graph, "ll:")]) {
  id <- sub("^ll:", "", nd)
  rws <- ll_by_id[[id]]
  if (is.null(rws)) next
  for (spec in PC_LL_EVENTS) {
    col <- intersect(spec$col, names(ll))[1]
    if (is.na(col)) next
    pc_add_events(nd, "ll", id, spec$lab, spec$kind,
                  pc_as_date(ll[[col]][rws]))
    n_ll_ev <- n_ll_ev + 1L
  }
}

# ---- CT contacts ----
# Follow-ups are emitted individually: the spread of follow-up dates IS the
# information (median 7 per contact, up to 26), and a contact under follow-up
# on a date another identifier was in hospital is two different people.
ct_by_contact <- split(seq_len(nrow(ct)), trimws(as.character(ct$contact_id)))
for (nd in nodes_in_graph[startsWith(nodes_in_graph, "ct_contact:")]) {
  id <- sub("^ct_contact:", "", nd)
  rws <- ct_by_contact[[id]]
  if (is.null(rws)) next
  pc_add_events(nd, "ct_contact", id, "Registered", "onset",
                pc_as_date(ct$registration_date[rws]))
  if ("last_contact_with_source_date" %in% names(ct))
    pc_add_events(nd, "ct_contact", id, "Last contact with source", "exposure",
                  pc_as_date(ct$last_contact_with_source_date[rws]))
  if ("followup_date" %in% names(ct))
    pc_add_events(nd, "ct_contact", id, "Follow-up", "followup",
                  pc_as_date(ct$followup_date[rws]))
}

# ---- CT source cases ----
# A source case has no dates of its own; it is known only through the contacts
# who name it. So its lane shows the window over which it was named as a
# source, and nothing else.
#
# Deliberately NOT given its linelist dates, even for the 230 that resolve to
# an alert_id: that linelist identifier is almost always its own node in the
# same cluster (that is what the EXACT link is), so copying them here would
# draw the same dates on two lanes and invent an agreement between them.
#
# `Contact exposure reported` -- its contacts' last_contact_with_source_date --
# is deliberately NOT emitted either. It described when other people said they
# were exposed to this person, which is about the contacts' timelines rather
# than this identifier's, and it put 335 dots on 233 lanes that answered no
# question a reviewer was asking. The same dates are still on each contact's
# own lane as `Last contact with source`, where they belong.
#
# Consequence worth knowing: 175 of the 233 source-case lanes now hold a single
# event and so have no span line. That is honest -- one date is one date -- and
# no lane is left empty, because every source case has a first registration.
ct_by_source <- split(seq_len(nrow(ct)), trimws(as.character(ct$source_case_alert_id)))
for (nd in nodes_in_graph[startsWith(nodes_in_graph, "ct_case:")]) {
  id <- sub("^ct_case:", "", nd)
  rws <- ct_by_source[[id]]
  if (is.null(rws)) next
  reg <- pc_as_date(ct$registration_date[rws])
  reg <- reg[!is.na(reg)]
  if (length(reg)) {
    pc_add_events(nd, "ct_case", id, "First named as source", "onset", min(reg))
    if (max(reg) != min(reg))
      pc_add_events(nd, "ct_case", id, "Last named as source", "admin", max(reg))
  }
}

node_dates <- do.call(rbind, ev)
if (is.null(node_dates)) {
  node_dates <- data.frame(node = character(0), id_type = character(0),
                           id = character(0), event = character(0),
                           kind = character(0), date = character(0),
                           stringsAsFactors = FALSE)
}
node_dates <- node_dates[order(node_dates$node, node_dates$date), , drop = FALSE]

pclog("    ", nrow(node_dates), " dated event(s) across ",
      length(unique(node_dates$node)), " of ", length(nodes_in_graph),
      " identifiers in the graph")
without <- setdiff(nodes_in_graph, node_dates$node)
if (length(without))
  pclog("    ", length(without), " identifier(s) have no usable date at all")

# Every rejected value, listed. These are data-quality findings, not noise:
# each one is a mistyped date sitting in the export, and naming them is how
# they get fixed upstream.
rejected <- do.call(rbind, .pc_rejected)
if (!is.null(rejected) && nrow(rejected)) {
  pclog("    ", nrow(rejected), " event(s) EXCLUDED as implausible dates ",
        "(outside the window above). Kept in the export, dropped only from ",
        "the timeline, where one would flatten a whole cluster's axis:")
  for (k in seq_len(nrow(rejected)))
    pclog("      ", rejected$date[k], "  ", rejected$event[k], "  ",
          rejected$node[k])
}


# -------------------------  Write  --------------------------------------------

stem <- paste0(epi_stem, "__", ct_stem)
out_dir <- pm_run_dir(stem, "candidates")   # data/processed/person_matching/<stem>/candidates/
# Created here rather than relying on the first pc_write_csv() to do it: the
# writes below are not all pc_write_csv(), and on a new stem (a new export
# pair) the directory does not exist yet, so a bare writeLines() into it fails
# with "cannot open the connection".
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# The tuning parameters this run used, recorded beside its output — every link
# score and auto-accept decision depends on them, and the
# config file they came from is editable and unversioned.
writeLines(match_params_json(), file.path(pm_run_dir(stem), "matching_params.json"))

pc_write_csv(edges, file.path(out_dir, "person_candidate_edges.csv"))
pc_write_csv(clusters, file.path(out_dir, "person_candidate_clusters.csv"))
pc_write_csv(node_dates, file.path(out_dir, "person_candidate_node_dates.csv"))
pc_write_csv(node_tokens, file.path(out_dir, "person_candidate_node_tokens.csv"))
pc_write_sample(out_dir)
# Always written, header-only when there are none, so the tab can tell "no
# near misses in this build" from "this build predates them".
pc_write_csv(
  if (is.null(near_misses)) data.frame(link_id = character(0), node_a = character(0),
                                       node_b = character(0), link_type = character(0),
                                       link_score = numeric(0))
  else near_misses,
  file.path(out_dir, "person_candidate_near_misses.csv"))
pc_write_review_sets(out_dir)

meta <- paste0(
  '{\n  "stem": "', stem, '",\n  "epi_run": "', epi_stem,
  '",\n  "ct_run": "', ct_stem, '",\n  "n_edges": ', nrow(edges),
  ',\n  "n_clusters": ', nrow(clusters),
  ',\n  "n_to_review": ', n_review,
  ',\n  "n_auto_accepted": ', n_auto,
  ',\n  "built_at": "', format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), '"\n}\n')
# Recorded in the person_candidates section of data/processed/latest.json,
# which the ledger, the registry and the app read to find the current run.
write_latest("person_candidates", jsonlite::fromJSON(meta, simplifyVector = FALSE))

pclog("\n  Written: ", file.path(out_dir, "person_candidate_edges.csv"))
pclog("  NOTE: person_candidate_edges.csv, person_candidate_clusters.csv, ",
      "person_candidate_near_misses.csv and ",
      "person_candidate_node_tokens.csv CONTAIN PATIENT NAMES (a name token ",
      "is a name fragment). Handle them like ",
      "dhis2_processed_linelist_names.csv. person_candidate_node_dates.csv ",
      "holds identifiers and dates only.")
pclog("\n  NOTHING is merged. Every cluster is a proposal; every edge is ",
      "separately reviewable, and breaking one splits its cluster.")

write_log_file(.pc_log, "person_candidates_log.txt")

person_candidate_edges <- edges
person_candidates_complete <- TRUE


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, c("person_candidate_edges",
                                       "person_candidates_complete"))
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
