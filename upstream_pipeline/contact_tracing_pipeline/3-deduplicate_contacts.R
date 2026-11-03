#!/usr/bin/env Rscript
# 3-deduplicate_contacts.R
#
# Identify duplicate individuals WITHIN the contact tracing export, in two
# passes:
#
#   1. contacts     — the same person holding two `contact_id`s
#   2. source cases — the same person holding two `source_case_alert_id`s
#
# Identification only. Nothing is removed, merged or resolved, exactly as in
# epi_linelist_pipeline/2-deduplicate_linelist.R.
#
# Outputs, beside the dataset matching reads (combined/ in the default
# "combined" mode; the latest export's folder in "latest" mode):
#   contact_duplicate_candidates.csv
#   source_case_duplicate_candidates.csv
# and the log, in logs/<run>/<stem>_ct_dedup_log.txt.
#
# ---------------------------------------------------------------------------
# THE TRAP: A `contact_id` IS NOT A PERSON
# ---------------------------------------------------------------------------
# A contact_id identifies a person x SOURCE CASE. `lokuli ambe roger` holds two
# of them because he was a contact of two different cases — that is correct
# data, not duplication.
#
# So "same person, two contact_ids" is the WRONG test. Measured on
# CONTACT-TRACING_01092026:
#
#   name-similar pairs, DIFFERENT source case   979   <- expected, NOT duplicates
#   name-similar pairs, SAME source case         79   <- candidate duplicates
#
# Deduplicating on name alone would therefore be 93% false positives. The same
# source case is required, and it is also the natural blocking key: pairs can
# only form within one source case's contact list, so no IDF-based blocking is
# needed here at all. (The linelist dedup needs it because it compares 35,213
# rows against each other with no such partition.)
#
# Requiring exact name equality finds only 14 of the 79. Most are typos and
# name-order flips — `MBOLOKO EZEKIEL` / `EZEKIEL MBOLOK` — so fuzzy matching
# is doing the real work.
#
# ---------------------------------------------------------------------------
# WHY SOURCE CASES NEED THEIR OWN PASS
# ---------------------------------------------------------------------------
# Only 2 of 268 case names carry two source_case_alert_ids, so this pass is
# small. It cannot be delegated to the linelist dedup, though, and the reason is
# structural rather than incidental:
#
#   bolamba rasiko        the linelist dedup DOES find it (tier B) — both IDs
#                          are linelist rows
#   charlotte mabele sanza  NOT found, because one of its two IDs has no
#                          linelist row at all
#
# The linelist dedup only ever sees linelist rows, and 12 well-formed source
# IDs have none. Those are invisible to it by construction.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("contact_tracing_cleaning_complete")) {
  source("pipelines/contact_tracing_pipeline/1-colname-standard.R", local = FALSE)
} else if (!contact_tracing_cleaning_complete) {
  source("pipelines/contact_tracing_pipeline/1-colname-standard.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

CTD_GLOBAL_MAX_DIST   <- match_param("name_weight.global_max_dist")   # JW distance ceiling on the whole name
CTD_AGE_TOLERANCE_YEARS <- match_param("corroborators.age_tolerance_years")
CTD_MIN_TOKEN_CHARS   <- match_param("tokenisation.score_min_token_chars")
CTD_MAX_TOKENS        <- match_param("tokenisation.max_tokens_per_name")


# ---------------------------------------------------------------------------
# SHARED REPETITION
# ---------------------------------------------------------------------------
# Both names repeating THE SAME token is evidence, even though the repetition
# itself adds nothing to either name's rarity. Ported from
# 2-deduplicate_linelist.R, where it was measured: 5 of 18 candidate pairs
# involving a repetition share the same repeated token, against 0.08 predicted
# by chance -- a 63x likelihood ratio, 95% interval on the size [+4.4, +7.2],
# so the bonus is set at the CONSERVATIVE end.
#
# One-sided repetition earns nothing: it is a transcription habit, not a
# discriminator. Agreement counts in, disagreement does not count out.
#
# NOTE: this fires on NO pair of CONTACT-TRACING_01092026. It is carried here
# so the two dedups score a name the same way, not because it changes today's
# output.
CTD_SHARED_REPEAT_BONUS <- match_param("name_weight.shared_repeat_bonus")

# ---------------------------------------------------------------------------
# WHY THERE IS NO COLLOCATION CORRECTION HERE
# ---------------------------------------------------------------------------
# 2-deduplicate_linelist.R and 1-build_name_rarity.R both correct for tokens
# that always travel together -- `de`+`dieu`+`jean` co-occur far more often
# than independence predicts, so summing the parts of JEAN DE DIEU claims 15.29
# where the honest joint figure is 6.76. That correction is DELIBERATELY NOT
# ported to this script, and the reason is measured rather than stylistic.
#
# At the linelist's thresholds (ratio >= 10, together >= 8) detection finds
# exactly ONE group on this corpus -- `dieu`+`merci` (MERCI DIEU), 8 entries,
# ratio 146 -- and it touches 0 of the 36 candidate pairs. So a verbatim port
# would be inert here, not beneficial: an absolute count of 8 in a corpus a
# fourteenth the linelist's size is the CTD_*_MULT problem over again, and it is
# doing almost nothing.
#
# (Corpus size: ctd_build_idf() sets n to the length of the collapsed vector, so
# N = 2,813 -- 2,539 contact_ids plus 274 source_case_alert_ids, of which 15 are
# the nameless CX- cases and contribute no tokens. 2,798 entries actually carry
# a name. Every figure below is unchanged whichever of the two is used: the same
# single strict group, the same 10 groups at together >= 3, the same 8 pairs
# dropped, and a floor of 9.13 either way.)
#
# Lowering the floor until it does fire makes it detect the WRONG THING. At
# together >= 3 there are 10 groups, and the strongest are:
#
#     odongo+patrice    ratio 933, together 3, inflation 6.84
#     katembo+salumu    ratio  46, together 3, inflation 3.83
#     tusiime+ruth      ratio  36, together 4, inflation 3.57
#     grace+mbalisi     ratio  15, together 3, inflation 2.72
#
# `ODONGO PATRICE` is ONE PERSON holding three contact_ids under a single
# source case -- the duplicate this pass exists to find. These are not tokens
# that travel together across a population; they are one individual recorded
# repeatedly, and their co-occurrence count IS the duplication.
#
# So the correction would penalise exactly the pairs it should surface.
# Measured on this export it cuts ODONGO PATRICE / ODONGO PATRICE from 13.68 to
# 6.84, under CTD_MIN_NAME_WEIGHT (9.13), removing it from the candidate file
# altogether -- and does the same to 7 more of the 31 contact pairs
# (TUSIIME RUTH, GRACE MBALISI, BWAMBALE MOISE, KATUSIIME DAVID, SALAMA KAVIRA and
# two further ODONGO PATRICE pairs), every one an exact or near-exact name
# match under one source case. 8 of 31 destroyed.
#
# The same circularity exists on the linelist side and is swamped there: 35,108
# entries, with the known duplicate identifiers about 0.6% of the corpus. Here
# it dominates.
#
# The pooled groups from 1-build_name_rarity.R are the linguistically correct
# ones (de+dieu+jean, passe+tout, fille+ma, joie+la) and would be safe -- they
# also affect 0 of the 36 candidate pairs in this
# export -- but reading them would restore the dependency this script
# deliberately shed (see the note above ctd_build_idf), for no change in
# output. Revisit only if a CT export ever grows large enough for
# population-level collocation to be detectable from CT alone.
#
# tests/test_name_score.py pins this refusal --
# test_ct_dedup_has_no_collocation_constants and
# test_ct_internal_collocation_would_destroy_real_duplicates -- so it cannot be
# "fixed" back in without the measurement being redone.


# -------------------------  Helper Functions  ---------------------------------

.ctd_log <- character(0)
ctdlog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .ctd_log <<- c(.ctd_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

ctd_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

ctd_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# Tokenisation is name_translit_ascii() / name_tokens() in 0-setup.R -- the one
# definition, shared with the linelist dedup, the rarity build and the candidate
# builder. This script used to carry a verbatim copy, held in step by tests that
# compared it character for character after comment stripping.
ctd_translit_ascii <- name_translit_ascii

ctd_tokens <- function(x, min_chars = CTD_MIN_TOKEN_CHARS) {
  name_tokens(x, min_chars = min_chars, max_tokens = CTD_MAX_TOKENS)
}


# Which tokens a name repeats. Mirrors tok_repeat in 2-deduplicate_linelist.R.
#
# The repetition is discarded from the WEIGHT -- ctd_name_weight() scores a
# de-duplicated token set, so `ZAWADI ZAWADI MARIE` cannot count `zawadi` twice --
# and re-introduced by CTD_SHARED_REPEAT_BONUS as what it actually is: evidence
# about the writing, not extra evidence about the name.
ctd_repeated_tokens <- function(tk) unique(tk[duplicated(tk)])

# Sorted-token form, so name ORDER never matters: MBOLOKO EZEKIEL and EZEKIEL
# MBOLOK must compare as near-identical, and they are 2 of the 79.
ctd_sorted <- function(x) {
  vapply(ctd_tokens(x), function(tk) paste0(sort(tk), collapse = ""),
         character(1))
}

ctd_jw <- function(a, b) stringdist::stringdist(a, b, method = "jw", p = 0.1)


# Corroborators are ADDITIVE EVIDENCE, never gates. Established on the linelist
# side and it holds here: age agrees within 2 years for only 68% of
# near-certain duplicate pairs (95% CI 60-76%), so requiring it would discard
# about a third of true matches. Disagreement is inert, not negative evidence.
# Likewise sex: 15 of the 79 contact candidates disagree on it, and the
# linelist work found mixed-sex tier-A pairs to be among the most convincing
# duplicates in the file.
ctd_agree <- function(a, b) {
  a <- trimws(as.character(a)); b <- trimws(as.character(b))
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  nzchar(a) & nzchar(b) & a == b
}

ctd_age_agree <- function(a, b) {
  na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
  ok <- !is.na(na) & !is.na(nb)
  res <- rep(FALSE, length(na))
  res[ok] <- abs(na[ok] - nb[ok]) <= CTD_AGE_TOLERANCE_YEARS
  res
}

# ---------------------------------------------------------------------------
# IS THIS FIELD COMPARABLE FOR THIS PAIR?
# ---------------------------------------------------------------------------
# One per agreement test above, and they must stay paired: a field counted as
# available but never comparable silently costs the pair a point, because
# corrob_score() reads 2*n_corr - n_avail and a field that cannot agree lands
# in the deficit exactly as a contradiction would.
#
# THESE REPLACE THE PER-PASS CONSTANTS CTD_PASS1_N_AVAIL / CTD_PASS2_N_AVAIL,
# and the distinction is not cosmetic. Those constants asked "can this PASS
# compare sex at all", which is true whenever any contact in the export records
# one -- so a pair where one side's sex is blank was charged for the blank as
# though the two had been compared and disagreed. The config's claim that a
# blank is "neither credit nor penalty" was true only of a field absent from a
# whole population (a source case has no sex, ever) and false of ordinary
# missing data, which is most of it.
#
# 2-deduplicate_linelist.R has always computed this per pair, term for term
# against its own five agreement tests. This brings the CT passes into line
# with it rather than inventing a third convention.
ctd_cmp_chr <- function(a, b) !ctd_blank(a) & !ctd_blank(b)

# Age is compared AS A NUMBER, so availability must test the coercion and not
# merely non-blankness: an age recorded as "unknown" is not blank, cannot
# agree, and must not be counted as comparable.
ctd_cmp_num <- function(a, b) {
  !is.na(suppressWarnings(as.numeric(a))) & !is.na(suppressWarnings(as.numeric(b)))
}


# -------------------------  Name rarity (IDF)  --------------------------------
# WHICH CORPUS THIS SCRIPT SCORES AGAINST IS AN OPEN QUESTION, AND THE ONLY
# PLACE IN THE PIPELINE WHERE "USE THE POOLED TABLE EVERYWHERE" IS NOT CLEARLY
# RIGHT. It is selected by matching.corpus.ct_dedup in pipeline_configs.json.
#
# The pipeline now builds token rarity ONCE, over the pooled corpus, before any
# deduplication runs (identify_persons/1-build_name_rarity.R). The linelist
# dedup and the candidate builder both read it, and for them the change is a
# consistency fix that moves almost nothing. Here it is neither free nor
# obviously an improvement, for a reason that has nothing to do with corpus
# size:
#
#   A candidate pair in this script is ALREADY confined to one source case's
#   contact list (median 9 contacts). Two contacts of the same source case
#   sharing even a common name is strong evidence; two of 35,213 unrelated
#   linelist rows sharing it is not. The name weight carries the whole burden of
#   a prior that the blocking has already supplied.
#
# So scoring against the pooled table, with thresholds unchanged, DROPPED 11
# pairs with identical names under the same source case — including
# `NEEMA ROSALIE` twice under one source, same age, sex, health zone and
# health area, registered two days apart. Those belong in a review queue.
#
# That is a threshold problem, not a rarity problem: the right fix is one shared
# notion of "rare" plus a threshold here that reflects the stronger prior, not a
# second, smaller corpus that inflates the evidence to compensate. Until that
# threshold is recalibrated and re-measured against the 31 known CT duplicate
# pairs, this defaults to `ct_internal` — preserving measured behaviour — and
# `pooled` is available to measure against.
#
# What was checked about CT-internal, rather than assumed:
#
#   * It does not saturate more than the pooled corpus. 71% of CT tokens occur
#     exactly once against 69% pooled — essentially identical, so the "CT is too
#     small to discriminate" worry does not survive measurement.
#   * The failure mode that would matter is a token that is rare in CT but
#     common overall, which CT-internal would over-weight. Of 1,815 tokens
#     appearing once in CT, only 10 appear 50+ times in the linelist — 0.5%.
#
# Either way the TOKENISER is now shared (name_tokens() in 0-setup.R), so the
# two corpora can no longer disagree about what a token even is.
CTD_CORPUS <- pipeline_configs$matching$corpus$ct_dedup
if (is.null(CTD_CORPUS) || !CTD_CORPUS %in% c("ct_internal", "pooled")) {
  stop("matching.corpus.ct_dedup must be \"ct_internal\" or \"pooled\"; got: ",
       if (is.null(CTD_CORPUS)) "(missing from pipeline_configs.json)"
       else paste0("\"", paste(CTD_CORPUS, collapse = "\", \""), "\""),
       call. = FALSE)
}

ctd_build_idf <- function(names_vec) {
  per <- lapply(ctd_tokens(names_vec), unique)
  flat <- unlist(per, use.names = FALSE)
  n <- length(per)
  if (!length(flat) || n == 0L) return(NULL)
  tb <- table(flat)
  list(vec = stats::setNames(log(n / as.numeric(tb)), names(tb)),
       unseen = log(n), n = n)
}


# -------------------------  Locate the run  -----------------------------------

ct_processed_dir <- file.path(repo_root, "data", "processed",
                              "contact_tracing_processed")

# Each target is one dataset to deduplicate, with its outputs written beside
# it. ONLY THE DATASET MATCHING READS: in "combined" mode (the default) that is
# the all-time dataset in combined/, and nothing else -- person candidates
# reads combined/'s outputs (both go through resolve_ct_matching_input(), so
# they cannot disagree). The per-export pass this script also used to run in
# combined mode wrote files no step read, so it was dropped. In "latest" mode
# the target is the latest export (or the exports the app selected).
if (identical(pipeline_configs$contact_tracing$data$matching_input, "combined")) {
  .ctd_in <- resolve_ct_matching_input(ct_processed_dir)
  ctd_targets <- list(list(stem = .ctd_in$stem, run_dir = .ctd_in$dir,
                           names_csv = .ctd_in$names_csv))
} else {
  ctd_stems <- if (exists("ct_force_stems") && length(ct_force_stems) > 0) {
    ct_force_stems
  } else {
    basename(resolve_ct_latest_dir(ct_processed_dir))
  }
  ctd_targets <- lapply(ctd_stems, function(s) list(
    stem = s, run_dir = file.path(ct_processed_dir, s),
    names_csv = file.path(ct_processed_dir, s, "contact_tracing_processed_names.csv")))
}

ctdlog("=== Identifying duplicates within the contact tracing export ===")
ctdlog("Runs to process: ", paste(vapply(ctd_targets, `[[`, "", "stem"),
                                  collapse = ", "))



for (.ctd_t in ctd_targets) {

stem      <- .ctd_t$stem
run_dir   <- .ctd_t$run_dir
names_csv <- .ctd_t$names_csv

if (!file.exists(names_csv)) {
  ctdlog("  Skipping ", stem, " — no contact_tracing_processed_names.csv ",
         "(names are required to recognise a duplicate).")
  next
}

df <- utils::read.csv(names_csv, stringsAsFactors = FALSE, na.strings = "",
                      colClasses = "character")
ctdlog("\n--- ", stem, " (", nrow(df), " rows) ---")

# One entry per identifier. Where an identifier carries several spellings, the
# most frequent wins.
ctd_collapse <- function(id_col, cols) {
  if (!id_col %in% names(df)) return(NULL)
  ids <- trimws(as.character(df[[id_col]]))
  keep <- !ctd_blank(ids)
  if (!any(keep)) return(NULL)
  idx <- split(which(keep), ids[keep])
  out <- data.frame(identifier = names(idx), stringsAsFactors = FALSE)
  for (cl in cols) {
    out[[cl]] <- if (cl %in% names(df)) {
      vapply(idx, function(rows) {
        v <- trimws(as.character(df[[cl]][rows]))
        v <- v[!ctd_blank(v)]
        if (!length(v)) return(NA_character_)
        tb <- table(v)
        names(tb)[which.max(as.numeric(tb))]
      }, character(1))
    } else NA_character_
  }
  out
}

# Corpus = every named identifier in THIS export (contacts and source cases),
# or the pooled running table, per CTD_CORPUS. See the long note above.
if (identical(CTD_CORPUS, "pooled")) {
  .ctd_nr <- load_name_rarity()
  idf_info <- list(vec = .ctd_nr$idf_vec, unseen = .ctd_nr$idf_unseen,
                   n = .ctd_nr$n, source = paste0("pooled (", .ctd_nr$stem, ")"))
} else {
  .ctd_nr <- NULL
  idf_info <- ctd_build_idf(c(
    if ("contact_name" %in% names(df)) ctd_collapse("contact_id", "contact_name")$contact_name,
    if ("case_name" %in% names(df)) ctd_collapse("source_case_alert_id", "case_name")$case_name
  ))
  if (!is.null(idf_info)) idf_info$source <- "this export"
}
if (is.null(idf_info)) {
  ctdlog("    No usable names in this export — skipping.")
  next
}

idf_of <- function(tk) {
  v <- idf_info$vec[tk]                 # single bracket: a miss gives NA
  v[is.na(v)] <- idf_info$unseen
  unname(v)
}

# Collocated groups, available only on the pooled branch because that is where
# they are counted. The carve-out comment above argued against CT-INTERNAL
# detection -- it destroyed 8 of 31 known duplicate pairs, because `ODONGO
# PATRICE` recurring under one source case IS the duplication, not two tokens
# that travel together in a population. The POOLED groups are a different
# object: measured over 47,000 identifiers, they are the linguistic ones
# (de+dieu+jean, passe+tout, la+joie), and the same comment records that they
# touched 0 of the 36 candidate pairs in the last export. Applied now that this
# script reads the pooled corpus, so a name is worth the same here as anywhere
# else. If a CT export ever grows enough for that to bite, the measurement to
# repeat is the 31-pair one.
CTD_COLLOC     <- if (is.null(.ctd_nr)) list()      else .ctd_nr$groups
CTD_COLLOC_IDF <- if (is.null(.ctd_nr)) numeric(0)  else .ctd_nr$group_idf


ctdlog("    Name rarity: ", idf_info$source, " — ", idf_info$n,
       " named identifiers, ", length(idf_info$vec), " tokens, max idf ",
       round(idf_info$unseen, 2))
if (!identical(CTD_CORPUS, "pooled")) {
  ctdlog("      (matching.corpus.ct_dedup = \"ct_internal\": this is the ONLY ",
         "matching step not scoring against the shared pooled table. See the ",
         "note above ctd_build_idf — 11 identical-name pairs under the same ",
         "source case fall below threshold on the pooled corpus until the ",
         "threshold here is recalibrated for the source-case blocking prior.)")
}

# The name weight as a number out of score_weights.name. Same function and the
# same two config keys as the linelist, scaled by this run's log(N) -- which,
# now that this script reads the pooled table, IS the linelist's log(N). So the
# curves are not merely alike, they are identical.
#
# THE MIDPOINT IS NOT CALIBRATED FOR THIS SCRIPT. It was fitted on 73,359
# linelist pairs against the share of each weight bin with 4-5 corroborators
# agreeing; nothing equivalent has been measured here. Two reasons to expect it
# to move: CT has its own prior, and the proxy it would be fitted against is
# not the same one (the denominator here is 5 for contacts and 1 for source
# cases, so "4-5 agreeing" does not mean what it means on the linelist).
# Until then this is deliberately the linelist's curve, so the two are at least
# wrong in the same direction rather than incomparably wrong.
CTD_NAME_SCORE_MID   <- match_param("name_weight.score_midpoint_mult")  * idf_info$unseen
CTD_NAME_SCORE_STEEP <- match_param("name_weight.score_steepness_mult") * idf_info$unseen
CTD_SCORE_W <- score_weights()

ctd_name_score <- function(w) {
  1 / (1 + exp(-(w - CTD_NAME_SCORE_MID) / CTD_NAME_SCORE_STEEP))
}

# One link score out of 1, and the cutoff below which a pair is not written.
# Same two helpers and the same config key as the linelist dedup, so a pair
# scoring 0.39 is rejected here for the same reason it is rejected there.
CTD_MIN_LINK_SCORE <- match_param("link_score.min_keep")
ctdlog("    A pair is kept when its link score >= ", CTD_MIN_LINK_SCORE,
       " (link_score.min_keep), the same rule as the other two matchers")
# Pairs scoring at least this but under min_keep are NEAR MISSES: not
# candidates, written to separate near_miss files for the Algorithm Tuning tab
# to audit what the cutoff throws away (see near_miss in pipeline_configs.json).
CTD_NEAR_MISS_MIN <- match_param("near_miss.min_score")
ctd_is_near_miss <- function(lsc) {
  !is.na(lsc) && lsc >= CTD_NEAR_MISS_MIN && lsc < CTD_MIN_LINK_SCORE
}
ctd_link_score <- function(w, n_corrob, n_avail) {
  round(round(CTD_SCORE_W[["name"]] * ctd_name_score(w), 4) +
          round(CTD_SCORE_W[["corroborator"]] *
                  corrob_score(n_corrob, n_avail), 4), 4)
}

ctdlog("    Name score (midpoint ", round(CTD_NAME_SCORE_MID, 2),
       ", steepness ", round(CTD_NAME_SCORE_STEEP, 2),
       ", UNFITTED for CT — the linelist's curve): weight 12 -> ",
       round(ctd_name_score(12), 3), " | 18 -> ", round(ctd_name_score(18), 3))

# Weight of the name evidence for a pair: sum of the IDF of tokens they share,
# plus CTD_SHARED_REPEAT_BONUS where both names repeat the same token.
# Shared RARE tokens are what carry information; shared common ones carry
# almost none, which is the whole reason rarity is used rather than a raw
# string distance.
#
# Takes the RAW token lists as well, because the bonus is the one thing that
# needs to know a token appeared twice. The score itself never does:
# intersect() returns unique values, and both call sites pass de-duplicated
# sets besides, so a repeated token cannot inflate the sum the way it did on
# the linelist side before unique() was applied there.
#
# BOTH JOBS LIVE IN ONE FUNCTION so the two passes cannot drift. A bonus
# applied at one call site and forgotten at the other would show up as source
# cases and contacts disagreeing about what a name is worth, which is precisely
# the class of bug the copied tokeniser comment warns about.
#
# The raw arguments DEFAULT to the de-duplicated ones, so a caller that has
# only unique tokens gets the old behaviour rather than a wrong bonus: a set
# with no duplicates yields no repeated tokens and the bonus is 0.
# NOW THE SHARED SCORER, not intersect(). This used to require tokens to be
# byte-identical, so `PASCIE LUTAMBA` and `PACIENCE LUTAMBA` shared one token where
# the linelist would have matched both -- the same two names worth different
# amounts depending on which script asked. name_pair_score() in 0-setup.R does
# the token budget, the greedy assignment, the dependence correction and the
# repetition bonus in one place; see its header.
#
# Still returns a bare number, because both call sites want the weight and read
# `shared_repeat` separately for the output column.
ctd_name_weight <- function(ta, tb, ta_raw = ta, tb_raw = tb) {
  name_pair_score(ta_raw, tb_raw, idf_of,
                  groups       = CTD_COLLOC,
                  group_idf    = CTD_COLLOC_IDF,
                  repeat_bonus = CTD_SHARED_REPEAT_BONUS,
                  max_tokens   = CTD_MAX_TOKENS)$weight
}

# TRUE only when both sides repeat the SAME token. One-sided repetition earns
# nothing -- see CTD_SHARED_REPEAT_BONUS.
ctd_shared_repeat <- function(ta_raw, tb_raw) {
  length(intersect(ctd_repeated_tokens(ta_raw),
                   ctd_repeated_tokens(tb_raw))) > 0L
}



# =============================  PASS 1: CONTACTS  =============================
# THE SAME SOURCE CASE IS NO LONGER REQUIRED. It is a corroborator now, and the
# blocking key is a shared name token, as in the other two matchers.
#
# One person is routinely a contact of several cases -- two members of one
# household fall ill, and everyone around them is registered once under each.
# Registered twice, they are issued two contact_ids, and two contact_ids is
# exactly what this pass exists to find. Requiring one source case made that
# duplicate unreachable: the pair was never compared, so there was no weak score
# to review, just nothing at all.
#
# The measurement that justified the old key -- 979 name-similar pairs across
# source cases against 79 within, "93% false positives" -- was name similarity
# ALONE, with none of the four corroborators consulted, and it was an argument
# about what to accept rather than what to look at. Those 979 pairs are now
# scored like any others and ordered below the ones that agree on more.
#
# Being contacts of the same case is still real evidence, so it is counted as
# such. A pair inside one source case gets a corroborator for it; a pair across
# two has to earn its score elsewhere.

ctdlog("\n  PASS 1: contacts (same person, any source case)")
nm_rows_c <- list()   # reset per export, so one stem's near misses never land in the next

# UP TO FIVE comparable fields: contact_sex, contact_age, health_zone,
# registration_date, source_case_alert_id. How many a given pair actually gets
# is computed per pair inside the loop by ctd_cmp_chr()/ctd_cmp_num(), which
# must match the corrob expression below term for term —
# tests/test_cluster_rules.py checks them, because an n_avail larger
# than the truth silently RAISES the bar (a field that is never compared can
# never agree) and one smaller silently lowers it.
#
# What each pair could compare is recorded in n_corroborators_available.

pop <- ctd_collapse("contact_id", c("contact_name", "contact_sex", "contact_age",
                                    "source_case_alert_id", "health_zone",
                                    "registration_date"))
contact_pairs <- NULL

if (is.null(pop) || !nrow(pop)) {
  ctdlog("    No usable contact_id / contact_name pairs.")
} else {
  pop <- pop[!ctd_blank(pop$contact_name), , drop = FALSE]
  ctdlog("    Distinct contacts with a name: ", nrow(pop))

  toks <- ctd_tokens(pop$contact_name)
  srt  <- ctd_sorted(pop$contact_name)

  # Blocking: an inverted index from token to the rows carrying it, then every
  # within-token pair, de-duplicated because a pair sharing two tokens appears
  # under both. Same rule as the linelist and the cross-file matcher.
  .idx <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(pop))) {
    for (t in unique(toks[[i]])) {
      cur <- .idx[[t]]
      .idx[[t]] <- c(if (is.null(cur)) integer(0) else cur, i)
    }
  }
  pair_mat <- do.call(rbind, lapply(ls(.idx), function(t) {
    g <- sort(.idx[[t]])
    n <- length(g)
    if (n < 2L) return(NULL)
    cbind(a = rep(g[-n], rev(seq_len(n - 1L))),
          b = unlist(lapply(seq_len(n - 1L), function(k) g[(k + 1L):n]),
                     use.names = FALSE))
  }))
  pair_mat <- if (is.null(pair_mat)) matrix(integer(0), ncol = 2) else
    unique(pair_mat)
  ctdlog("    Candidate pairs after blocking on a shared token: ",
         nrow(pair_mat), "  (all possible pairs would be ",
         format(choose(nrow(pop), 2), big.mark = ",", scientific = FALSE), ")")

  rows <- list()
  nm_rows_c <- list()   # near misses, pass 1
  if (nrow(pair_mat)) {
    for (k in seq_len(nrow(pair_mat))) {
        a <- pair_mat[k, 1L]; b <- pair_mat[k, 2L]
        d <- ctd_jw(srt[a], srt[b])
        if (is.na(d) || d > CTD_GLOBAL_MAX_DIST) next
        ta_raw <- toks[[a]]; tb_raw <- toks[[b]]
        # n_tok below must count DISTINCT tokens, or `ANNIE ANNIE` would look
        # like a two-token name and escape the single-token rule.
        ta <- unique(ta_raw); tb <- unique(tb_raw)
        w  <- ctd_name_weight(ta, tb, ta_raw, tb_raw)
        # The source case is compared, not assumed. ctd_agree() is false when
        # either side is blank, so a contact with no source case recorded
        # simply fails to earn this one.
        same_src <- ctd_agree(pop$source_case_alert_id[a],
                              pop$source_case_alert_id[b])
        corrob <- sum(c(ctd_agree(pop$contact_sex[a], pop$contact_sex[b]),
                        ctd_age_agree(pop$contact_age[a], pop$contact_age[b]),
                        ctd_agree(pop$health_zone[a], pop$health_zone[b]),
                        ctd_agree(pop$registration_date[a], pop$registration_date[b]),
                        same_src))
        # One term per corroborator above, in the same order.
        n_avail <- sum(c(ctd_cmp_chr(pop$contact_sex[a], pop$contact_sex[b]),
                         ctd_cmp_num(pop$contact_age[a], pop$contact_age[b]),
                         ctd_cmp_chr(pop$health_zone[a], pop$health_zone[b]),
                         ctd_cmp_chr(pop$registration_date[a], pop$registration_date[b]),
                         ctd_cmp_chr(pop$source_case_alert_id[a],
                                     pop$source_case_alert_id[b])))
        # A field that agreed but was not counted as comparable means the two
        # tests disagree about what blank is. Cheap, and it would catch an edit
        # to one without the other.
        stopifnot(corrob <= n_avail)
        # The link score decides what exists: below
        # link_score.min_keep a pair is not a weak candidate, it is not a
        # candidate, and it is rejected here rather than downstream so the
        # cross-file graph never sees it.
        lsc <- ctd_link_score(w, corrob, n_avail)
        if (ctd_is_near_miss(lsc)) {
          nm_rows_c[[length(nm_rows_c) + 1L]] <- data.frame(
            contact_id_a = pop$identifier[a], contact_id_b = pop$identifier[b],
            name_weight = round(w, 2), name_distance = round(d, 4),
            link_score = lsc, n_corroborators = corrob,
            n_corroborators_available = n_avail, stringsAsFactors = FALSE)
        }
        if (is.na(lsc) || lsc < CTD_MIN_LINK_SCORE) next
        rows[[length(rows) + 1L]] <- data.frame(
          run = stem,
          contact_id_a = pop$identifier[a], contact_id_b = pop$identifier[b],
          # No names, sex, age or zone: the only reader of this file
          # (2-build-person-candidates.R) uses the IDs and the scores, and the
          # pairs are judged on clusters in the Person Review tab. A contact's
          # details are one lookup away, by contact_id, in the dataset this
          # pass read (contact_tracing_combined_names.csv in combined mode).
          # BOTH sides now, because they can differ. The old single
          # `source_case_alert_id` column was only meaningful while the two were
          # required to be equal.
          source_case_alert_id_a = pop$source_case_alert_id[a],
          source_case_alert_id_b = pop$source_case_alert_id[b],
          same_source_case = as.logical(same_src),
          name_distance = round(d, 4), name_weight = round(w, 2),
          name_score  = round(ctd_name_score(w), 4),
          name_points = round(CTD_SCORE_W[["name"]] * ctd_name_score(w), 4),
          corrob_score = round(corrob_score(corrob, n_avail), 4),
          corrob_points = round(CTD_SCORE_W[["corroborator"]] *
                                  corrob_score(corrob, n_avail), 4),
          # The two ROUNDED halves added, so the total equals the parts shown
          # beside it. A rounded sum of exact halves differs by up to 1e-4.
          link_score = lsc,
          n_corroborators = corrob,
          # Carried so the corroborator count can be read as a fraction. Without it,
          # "2 corroborators" is uninterpretable: it is strong out of 4 and
          # meaningless out of 1.
          n_corroborators_available = n_avail,
          # Carried rather than left implicit in name_weight: a reviewer
          # comparing two weights should be able to see that 4 of the
          # difference is the repetition bonus and not shared rarity.
          shared_repeat = ctd_shared_repeat(ta_raw, tb_raw),
          stringsAsFactors = FALSE
        )
    }
  }
  contact_pairs <- if (length(rows)) do.call(rbind, rows) else NULL
}

if (is.null(contact_pairs)) {
  ctdlog("    No candidate contact duplicates.")
} else {
  contact_pairs <- contact_pairs[order(-contact_pairs$link_score,
                                       -contact_pairs$name_weight), , drop = FALSE]
  ctdlog("    Candidate duplicate contacts: ", nrow(contact_pairs))
  ctdlog("      earning the shared-repetition bonus (+",
         CTD_SHARED_REPEAT_BONUS, "): ", sum(contact_pairs$shared_repeat))
  # The number this change exists to produce: pairs the old source-case key
  # made unreachable. If it is zero,
  # either the data has no such duplicates or the blocking did not widen.
  ctdlog("      across DIFFERENT source cases (unreachable before): ",
         sum(!contact_pairs$same_source_case), " of ", nrow(contact_pairs))
  ctd_write_csv(contact_pairs,
                file.path(run_dir, "contact_duplicate_candidates.csv"))
}

# Near misses from pass 1, written even when empty so a reader can tell
# "none in the band" from "this dedup predates near misses".
ctd_write_csv(
  if (length(nm_rows_c)) do.call(rbind, nm_rows_c) else
    data.frame(contact_id_a = character(0), contact_id_b = character(0),
               name_weight = numeric(0), name_distance = numeric(0),
               link_score = numeric(0), n_corroborators = integer(0),
               n_corroborators_available = integer(0)),
  file.path(run_dir, "contact_near_miss_candidates.csv"))


# ==========================  PASS 2: SOURCE CASES  ============================
# No partition to block on here, but there are only a few hundred source cases,
# so every pair is compared directly.

ctdlog("\n  PASS 2: source cases (same person, two source_case_alert_ids)")
nm_rows_s <- list()

# ONE comparable field AT MOST. A source case is known here only through the
# contacts who name it, so it has no sex, no age and no onset of its own — only
# the contact's health zone, which is not even the source case's own zone.
#
# So these pairs rest mostly on the name: the corroborator half of the link
# score can only move on one field.
#
# AND ZERO WHERE THE ZONE IS BLANK.
# Such a pair now has nothing comparable, n_avail is 0, and corrob_score()
# returns the neutral 0.5 its centring was designed to give — where before it
# scored 2*0 - 1 = -1 and came out at 0.34, penalised for a field neither record
# carried.

spop <- ctd_collapse("source_case_alert_id", c("case_name", "health_zone"))
source_pairs <- NULL

if (is.null(spop) || !nrow(spop)) {
  ctdlog("    No usable source_case_alert_id / case_name pairs.")
} else {
  spop <- spop[!ctd_blank(spop$case_name), , drop = FALSE]
  ctdlog("    Distinct source cases with a name: ", nrow(spop))

  stoks <- ctd_tokens(spop$case_name)
  ssrt  <- ctd_sorted(spop$case_name)
  rows <- list()
  nm_rows_s <- list()   # near misses, pass 2
  n <- nrow(spop)
  if (n > 1L) {
    for (i in seq_len(n - 1L)) {
      for (j in seq(i + 1L, n)) {
        d <- ctd_jw(ssrt[i], ssrt[j])
        if (is.na(d) || d > CTD_GLOBAL_MAX_DIST) next
        ta_raw <- stoks[[i]]; tb_raw <- stoks[[j]]
        ta <- unique(ta_raw); tb <- unique(tb_raw)
        w  <- ctd_name_weight(ta, tb, ta_raw, tb_raw)
        # health_zone here is the CONTACT's zone, not the source case's — a
        # weak corroborator, deliberately not a gate. See the note in
        # 2-assign-contact-ids.R.
        corrob <- sum(ctd_agree(spop$health_zone[i], spop$health_zone[j]))
        # One term, matching the one corroborator above.
        n_avail <- sum(ctd_cmp_chr(spop$health_zone[i], spop$health_zone[j]))
        stopifnot(corrob <= n_avail)
        # Below the link-score cutoff rejected, as in pass 1.
        lsc <- ctd_link_score(w, corrob, n_avail)
        if (ctd_is_near_miss(lsc)) {
          nm_rows_s[[length(nm_rows_s) + 1L]] <- data.frame(
            source_case_alert_id_a = spop$identifier[i],
            source_case_alert_id_b = spop$identifier[j],
            name_weight = round(w, 2), name_distance = round(d, 4),
            link_score = lsc, n_corroborators = corrob,
            n_corroborators_available = n_avail, stringsAsFactors = FALSE)
        }
        if (is.na(lsc) || lsc < CTD_MIN_LINK_SCORE) next
        rows[[length(rows) + 1L]] <- data.frame(
          run = stem,
          source_case_alert_id_a = spop$identifier[i],
          source_case_alert_id_b = spop$identifier[j],
          # No names or zones, as in pass 1. A source case's details are one
          # lookup away by source_case_alert_id.
          name_distance = round(d, 4), name_weight = round(w, 2),
          name_score  = round(ctd_name_score(w), 4),
          name_points = round(CTD_SCORE_W[["name"]] * ctd_name_score(w), 4),
          corrob_score = round(corrob_score(corrob, n_avail), 4),
          corrob_points = round(CTD_SCORE_W[["corroborator"]] *
                                  corrob_score(corrob, n_avail), 4),
          # Rounded halves, as in pass 1.
          link_score = lsc,
          n_corroborators = corrob,
          n_corroborators_available = n_avail,
          shared_repeat = ctd_shared_repeat(ta_raw, tb_raw),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  source_pairs <- if (length(rows)) do.call(rbind, rows) else NULL
}

if (is.null(source_pairs)) {
  ctdlog("    No candidate duplicate source cases.")
} else {
  source_pairs <- source_pairs[order(-source_pairs$link_score,
                                     -source_pairs$name_weight), , drop = FALSE]
  ctdlog("    Candidate duplicate source cases: ", nrow(source_pairs))
  ctdlog("      earning the shared-repetition bonus (+",
         CTD_SHARED_REPEAT_BONUS, "): ", sum(source_pairs$shared_repeat))
  ctd_write_csv(source_pairs,
                file.path(run_dir, "source_case_duplicate_candidates.csv"))
}

ctd_write_csv(
  if (length(nm_rows_s)) do.call(rbind, nm_rows_s) else
    data.frame(source_case_alert_id_a = character(0),
               source_case_alert_id_b = character(0),
               name_weight = numeric(0), name_distance = numeric(0),
               link_score = numeric(0), n_corroborators = integer(0),
               n_corroborators_available = integer(0)),
  file.path(run_dir, "source_case_near_miss_candidates.csv"))

ctdlog("\n  NOTHING has been removed, merged or resolved. The CT CSVs are ",
       "unchanged; these are review tables only.")

write_log_file(.ctd_log, paste0(stem, "_ct_dedup_log.txt"))

}  # end stem loop

contact_tracing_dedup_complete <- TRUE


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, "contact_tracing_dedup_complete")
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
