#!/usr/bin/env Rscript
# 2-deduplicate_linelist.R
#
# IDENTIFY candidate duplicate individuals WITHIN the epi linelist.
#
# This script does not remove, merge, or resolve anything. It only writes a
# review table of candidate pairs. The linelist CSVs are never modified.
#
# Run after pipelines/epi_linelist_pipeline/1-colname-standard.R.
#
# Output (in the latest processed linelist folder):
#   duplicate_candidates.csv    one row per candidate pair: the two records'
#                               row numbers and alert IDs, and the evidence.
#                               No names or other personal details.
#   dedup_scoring_meta.json     which corpus and thresholds produced the scores
#   logs/<run>/<stem>_dedup_log.txt  what the run did, mirroring the processing log
#
# NOTE FOR ANYONE COMPARING OUTPUT TO A RUN BEFORE 2026-09-23. Three things
# changed in how a name scores, and all three move numbers:
#   * two tokens count as the same token on an EDIT BUDGET scaled to token
#     length, not a flat Jaro-Winkler ceiling of 0.15;
#   * a matched token is worth its full IDF -- the distance penalty is gone,
#     having been measured to penalise the more reliable evidence;
#   * ONE score decides both file membership and tier, where the plain sum used
#     to gate the file and the corrected sum set the tier.
# Each is argued at the point of use below.
#
#
# ---------------------------------------------------------------------------
# WHY IT WORKS THIS WAY
# ---------------------------------------------------------------------------
# Everything below is calibrated against LINELIST_01092026 (35,213 rows).
#
# 1. A name-distance threshold alone is not enough. Two rows can carry
#    *identical* normalised names and still be different people, because names
#    here are drawn from a small, highly repetitive pool: "kavira" appears in
#    1,028 rows, "mave" in 978, "kambale" in 927, while 15,954 tokens appear in
#    exactly one row. Of the 2,635 candidate pairs whose names are identical
#    (Jaro-Winkler distance exactly 0), those with common names show symptom-
#    onset agreement *below* chance (0.7x) — i.e. they are coincidental
#    collisions — while those with rare names run at 3.7x chance. A distance
#    metric cannot express that difference, since both score 0.
#
#    So name evidence is scored by TOKEN RARITY (inverse document frequency),
#    not by string distance alone. Distance is still used, as a cheap first
#    gate with good behaviour: on a labelled test (621 pairs with strong
#    non-name evidence as positives, 26,010 single-common-token pairs with
#    agreeing demographics as negatives) a global cut of 0.10 retains 90.8% of
#    positives and only 4.2% of negatives.
#
# 2. Names are compared under two normalisations because token order varies
#    constantly in this data ("Zawadi Marie jeanne" / "MARIE-JEANNE ZAWADI",
#    "BALOSA DIEU-MERCI" / "DIEUMERCI BALOSA"). 453 rows are only reachable by
#    allowing reordering. The better (smaller) of the two distances is used.
#
# 3. AGE IS NEVER REQUIRED. Measured on 127 pairs that are near-certainly the
#    same person but were identified without using age at all (identical
#    sorted-token name + same health area + same symptom-onset date), age
#    agrees within 2 years for only 68% (95% CI 60-76%), disagrees by more than
#    10 years for 12.7%, and is off by as much as 44 years. Requiring age
#    agreement would discard roughly a third of genuine duplicates. Age is one
#    optional corroborator; disagreement is NEVER read as evidence that two
#    rows are different people.
#
# 4. MIN_NAME_WEIGHT is derived, not tuned. The four non-name corroborators are
#    independent of the name score, so they calibrate it. Against a chance
#    baseline from 60,000 random same-province pairs (same zone 11.1%, same
#    area 1.8%, age within 2 8.5%, onset within 7d 16.1%):
#
#      name weight   n      same zone   same area   age +/-2   onset <=7d
#       8-10       7,475       2.4x        3.0x       1.8x        0.8x
#      10-12       5,161       2.5x        4.7x       2.1x        1.0x
#      12-15       2,257       4.8x       14.6x       4.4x        1.9x
#      15-20       1,052       6.7x       26.0x       7.5x        3.1x
#        20+         346       6.9x       27.7x       8.9x        4.0x
#
#    Symptom onset is the corroborator least contaminated by name or geography.
#    Below weight 12 it sits at or below chance, meaning those pairs are name
#    collisions rather than duplicates; from 12 upward it rises to 4x. Weight
#    12 is therefore where independent evidence begins to exist.
#
# 5. Blocking is by shared name token within province (see BLOCK_* below).
#    Comparing all 35,108 named rows pairwise would be 616 million
#    comparisons; requiring a shared token reduces that to ~1.7 million with
#    little loss, since a typo rarely corrupts every token of a name.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("epi_linelist_cleaning_complete")) {
  source("pipelines/epi_linelist_pipeline/1-colname-standard.R", local = FALSE)
} else if (!epi_linelist_cleaning_complete) {
  source("pipelines/epi_linelist_pipeline/1-colname-standard.R", local = FALSE)
}

# SEPARATELY from the cleaning guard above, because a caller that has already
# run (or stubbed) the cleaning step skips that branch entirely -- and 0-setup.R
# was previously reached ONLY through it. That was invisible while the Shiny app
# was the only caller, since global.R sources 0-setup.R itself; it broke the
# moment this script was run standalone, because match_param() below is defined
# there. Same form as every other pipeline script, and a no-op when the branch
# above already loaded it.
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------
# Values shared with the other matching scripts now come from
# pipeline_configs.json's `matching` block via match_param(), because they were
# duplicated in two to four scripts each and kept in step only by tests that
# regexed the source. The DERIVATION of each still lives here, at the point of
# use — the JSON says what a key means, never why the number is what it is.
# Script-local tunables stay as literals below.
#
# See the rationale block above before changing any of them; the two that matter
# most are GLOBAL_MAX_DIST (recall) and MIN_NAME_WEIGHT (precision).

# Jaro-Winkler ceiling on the whole normalised name. First-pass gate only.
GLOBAL_MAX_DIST <- match_param("name_weight.global_max_dist")

# WHEN TWO TOKENS COUNT AS THE SAME TOKEN is no longer a Jaro-Winkler ceiling.
# It is an edit budget that grows with token length -- name_tokens_match() in
# 0-setup.R, configured at matching.token_match. The full argument and the
# measurement are there. The short version: a fixed JW ceiling lets a long token
# absorb a typo three times more easily than a short one, so it was admitting
# `de`/`dieu` while rejecting `voctorine`/`victorine`.
#
# The WHOLE-NAME gate below (GLOBAL_MAX_DIST) is a separate test, still
# Jaro-Winkler, and unchanged. It runs first, and it is what makes the token
# rule safe -- see the note on the removed distance penalty further down.


# Tokens shorter than this are ignored when building blocks (initials, particles).
# Script-local: blocking is this script's own concern.
BLOCK_MIN_TOKEN_CHARS <- 3

# Tokens shorter than this are ignored when scoring.
SCORE_MIN_TOKEN_CHARS <- match_param("tokenisation.score_min_token_chars")

# A token appearing in more than this many rows is too common to block on
# across a whole province, so pairs sharing only that token must additionally
# share a health zone. 14 tokens exceed this in LINELIST_01092026.
BLOCK_COMMON_TOKEN_ROWS <- 400

# Never compare more than this many tokens per name (guards against a junk
# free-text value blowing up the token cross-product).
MAX_TOKENS_PER_NAME <- match_param("tokenisation.max_tokens_per_name")

# Corroborator tolerances. The onset window is script-local — no other script
# compares onset, because only the linelist records it.
AGE_TOLERANCE_YEARS <- match_param("corroborators.age_tolerance_years")
ONSET_TOLERANCE_DAYS <- 7


# ---- Token dependence -------------------------------------------------------
# Summing token IDF assumes the tokens are independent. They are not: `de` and
# `dieu` appear together 48 times where independence predicts 0.6. The full
# argument, the measured alternatives and why the ratio is 10 rather than 8 or
# 19 are all in identify_persons/1-build_name_rarity.R beside
# NR_COLLOC_MIN_RATIO. The short version is that summing the parts of
# `JEAN DE DIEU` gives 15.29 against an honest joint figure of 6.70 here, and
# 15 is the tier-A bar.
#
# NO LONGER DETECTED HERE. This script used to detect groups from the linelist's
# own corpus, because it ran first and had to stay runnable on the 11 export
# dates with no CT counterpart. It now reads the pooled groups from the running
# table, which is built before any deduplication and is always present.
#
# The change is small by construction: the linelist is 93% of the pooled corpus,
# and the two detections found 28 and 30 groups agreeing on 27, with the same
# single triple. Every disagreement was a threshold near-miss -- `jean`+`luc`
# co-occurs 8 times pooled and 7 in the linelist, one short of the floor.
# The thresholds themselves live in matching.collocation and are read by
# 1-build_name_rarity.R, which is now the only place they are applied.

# Both names repeating THE SAME token is evidence, even though the repetition
# itself adds nothing to the name's rarity.
#
# Measured: 18 candidate pairs involve a repetition; 5 share the same repeated
# token where chance predicts 0.08 (the population rate of repeating is
# 154/35,106 = 0.44%). That is a 63x likelihood ratio, +6.0 bits, Poisson
# p ~ 1e-8 for the effect existing -- but n = 5, and the 95% interval on the
# size is [+4.4, +7.2]. So the bonus is set at the CONSERVATIVE end.
#
# One-sided repetition earns nothing. `DENISE KIRANDA DENISE` / `DENISE KIRANDA`
# and `SIFA LONGO` / `sifa longo longo` are plainly the same people, so a
# repetition on one side only is a transcription habit, not a discriminator:
# agreement counts in, disagreement does not count out.
SHARED_REPEAT_BONUS <- match_param("name_weight.shared_repeat_bonus")


# -------------------------  Helper Functions  ---------------------------------

# 1-colname-standard.R ends by clearing everything not in keep_objects, which
# includes its own normalize_key() and write_processed_csv(). Both are needed
# here, so they are redefined rather than depended on.
#
# dedup_normalize_key() differs from normalize_key() in one way, deliberately:
# it maps NA to "" instead of propagating it, because every use below tests the
# result with nzchar() and an NA there would silently poison a comparison.
dedup_normalize_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- translit_ascii(x)
  gsub("[^a-z0-9]", "", x)
}

write_processed_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(
    df, path, sep = ",", row.names = FALSE, col.names = TRUE,
    quote = TRUE, na = "", qmethod = "double"
  )
}

# Deterministic accent folding, replacing iconv(to = "ASCII//TRANSLIT").
#
# iconv's TRANSLIT behaviour is supplied by the platform's C library and is NOT
# portable: glibc renders "Gisèle" as "Gisele", macOS commonly yields "Gis?le",
# and some builds return NA. Because the "?" then becomes a token separator,
# one name normalises to a single token on Linux and two on macOS, changing the
# token-rarity weights and the candidate set. Running this pipeline on two
# analysts' machines produced tier counts differing by up to 26% (tier S: 202
# vs 149) for identical input, which for an identity-matching pipeline is a
# correctness problem, not a cosmetic one.
#
# The map itself is name_translit_ascii() in 0-setup.R — the single definition,
# shared with the CT dedup, the rarity build and the candidate builder. This is
# a local alias so the rest of this script reads unchanged.
translit_ascii <- name_translit_ascii

# Normalise a name to one string, no whitespace. Same approach as
# prep_name() in contact_tracing_pipeline/4-build-id-lookup.R, kept
# deliberately consistent so scores are comparable between the two tasks.
dedup_prep_concat <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- translit_ascii(x)
  x <- gsub("['`’]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("[[:space:]]+", "", x)
}

# Split into tokens, sort alphabetically, rejoin. Makes the comparison
# insensitive to name order.
dedup_prep_sorted <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- translit_ascii(x)
  x <- gsub("['`’]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  vapply(strsplit(trimws(x), "[[:space:]]+"), function(tk) {
    tk <- tk[nzchar(tk)]
    if (!length(tk)) return("")
    paste0(sort(tk), collapse = "")
  }, character(1))
}

# Tokenise a name, dropping tokens below min_chars. name_tokens() in 0-setup.R
# is the single definition; this script calls it with two different min_chars
# (scoring and blocking), which is why the alias keeps that argument.
dedup_tokens <- function(x, min_chars) {
  name_tokens(x, min_chars = min_chars, max_tokens = MAX_TOKENS_PER_NAME)
}

# All unordered pairs from a vector of row ids. Vectorised; avoids combn(),
# which is slow on the larger blocks.
dedup_make_pairs <- function(v) {
  v <- sort(unique(v))
  m <- length(v)
  if (m < 2) return(NULL)
  i <- rep.int(seq_len(m - 1L), times = rev(seq_len(m - 1L)))
  j <- unlist(lapply(seq_len(m - 1L), function(k) (k + 1L):m), use.names = FALSE)
  cbind(a = v[i], b = v[j])
}

# ---- Collocation detection --------------------------------------------------
# NOT DETECTED HERE ANY MORE. Collocated token groups are built once, over the
# pooled corpus, by identify_persons/1-build_name_rarity.R, and read below from
# its running table. This script previously carried verbatim copies of
# nr_pair_counts() and nr_collocation_cliques(), held in step by tests that
# compared them character for character.


# Local logging (log_msg() from 1-colname-standard.R is removed from the
# global environment when that script cleans up, so this script keeps its own).
.dedup_log <- character(0)
dlog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .dedup_log <<- c(.dedup_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}
# -------------------------  Load the linelist  --------------------------------

epi_dir <- resolve_epi_latest_dir()
names_csv <- file.path(epi_dir, "dhis2_processed_linelist_names.csv")

if (file.exists(names_csv)) {
  ll_path <- names_csv
} else {
  stop("No processed linelist found in ", epi_dir,
       ".\nRun pipelines/epi_linelist_pipeline/1-colname-standard.R first.",
       call. = FALSE)
}

ll <- utils::read.csv(ll_path, stringsAsFactors = FALSE, na.strings = "")
dlog("=== Linelist deduplication (identification only) ===")
dlog("Source: ", ll_path)
dlog("Rows: ", nrow(ll))

if (!"name" %in% names(ll)) {
  stop("The processed linelist has no 'name' column, so duplicate individuals ",
       "cannot be identified. dhis2_processed_linelist_names.csv is required ",
       "(the de-identified CSV is not sufficient).", call. = FALSE)
}

ll$.row_id <- seq_len(nrow(ll))

# Everything below tolerates absent optional columns.
get_col <- function(nm, default = NA_character_) {
  if (nm %in% names(ll)) as.character(ll[[nm]]) else rep(default, nrow(ll))
}

nm_raw   <- get_col("name")
prov     <- dedup_normalize_key(get_col("province"))
hzone    <- dedup_normalize_key(get_col("health_zone"))
harea    <- dedup_normalize_key(get_col("health_area"))
# NA must become "" here, not stay NA: nzchar(NA) is TRUE and NA == NA is NA,
# so an NA sex would make c_sex (and therefore the whole corroborator count)
# NA and silently drop the pair out of every tier.
sex1     <- toupper(substr(trimws(get_col("sex")), 1, 1))
sex1[is.na(sex1)] <- ""
age_num  <- suppressWarnings(as.numeric(get_col("age")))
onset    <- suppressWarnings(as.Date(get_col("date_of_symptom_onset")))

nm_concat <- dedup_prep_concat(nm_raw)
nm_sorted <- dedup_prep_sorted(nm_raw)

# A TOKEN COUNTS ONCE PER NAME, however many times it is written.
#
# 154 names repeat a token -- `ZAWADI ZAWADI MARIE`, `Christian Christian`,
# `TUMAINI BAHATI TUMAINI`. Without unique() the matcher pairs both copies
# against the other side and adds that token's IDF twice, so
# `ZAWADI ZAWADI MARIE` scored 24.30 where its honest score is 14.53. One word,
# counted as two independent pieces of evidence, and enough to clear the tier-A
# bar on its own.
#
# It also inflated name_rarity for those 154 identifiers, which feeds the 50th
# percentile floor on the auto-accept rules -- a cluster could be accepted
# without review on a score that double-counted one word.
#
# unique() here rather than in the scoring, so n_tok, n_shorter and the
# `n_unmatched == 0` rule all see the same de-duplicated set, and so this
# matches 1-build_name_rarity.R, which has always done exactly this.
tok_score_raw <- dedup_tokens(nm_raw, SCORE_MIN_TOKEN_CHARS)
tok_score <- lapply(tok_score_raw, unique)
tok_block <- lapply(dedup_tokens(nm_raw, BLOCK_MIN_TOKEN_CHARS), unique)
n_tok     <- lengths(tok_score)

# Which tokens a name repeats, kept for the shared-repetition bonus below. The
# repetition is discarded from the score above and re-introduced there as what
# it actually is: evidence about the writing, not extra evidence about the name.
tok_repeat <- lapply(tok_score_raw, function(tk) unique(tk[duplicated(tk)]))

usable <- nzchar(nm_concat) & n_tok > 0
dlog("Rows with a usable name: ", sum(usable), " (", sum(!usable), " blank/unusable)")


# ---------------  Token rarity and collocations: READ, NOT COMPUTED  ----------
# IDF = log(N / entries containing the token). A token in 1 entry scores ~10.5;
# a token in 1,000 entries scores ~3.5.
#
# BOTH NOW COME FROM THE RUNNING TABLE built by
# identify_persons/1-build_name_rarity.R, over the POOLED corpus (linelist +
# contact tracing, one entry per identifier), before any deduplication runs.
#
# This script used to count its own, over its own rows. That was defensible
# while the rarity table was a downstream artefact — 11 of 51 linelist export
# dates have no CT counterpart, so there was no table it could rely on — but it
# meant "rare" had three different definitions in one pipeline, and the CT
# dedup's version saturated badly. The running table is always present, so the
# reason is gone. See the header of 1-build_name_rarity.R.
#
# WHAT CHANGES NUMERICALLY, for this script specifically: the corpus grows from
# this export's named rows to the pooled identifier count, so log(N) moves from
# 10.4661 to about 10.5429, and with it all four thresholds below (they are
# multipliers on log(N), which is exactly why they were expressed that way).
# Pooling shifts individual token IDF by a median of 0.077, with 44 of 22,929
# tokens moving more than 1.0 — the linelist is 93% of the pool. The
# calibrated 12 / 15 / 8 this script was tuned with become 12.09 / 15.11 / 8.06.
#
# The OTHER change is per-entry vs per-row counting. The pooled table counts
# once per identifier; this script counted once per usable row. On the linelist
# those are near enough the same thing (one row per alert_id) that the shift is
# inside the 0.077 median above, but they are not identical, and the pooled
# definition is the correct one — see rule 2 in 1-build_name_rarity.R.

.nr <- load_name_rarity()

idf_vec    <- .nr$idf_vec
IDF_UNSEEN <- .nr$idf_unseen
N_named    <- .nr$n
idf_of     <- .nr$idf_of

dlog("Name rarity: read from the running pooled table (", .nr$stem, ", built ",
     .nr$built_at, ")")
dlog("  ", format(N_named, big.mark = ","), " identifiers, ",
     format(length(idf_vec), big.mark = ","), " distinct tokens, ",
     "max idf ", round(IDF_UNSEEN, 4))
dlog("  Tokens in exactly one entry: ",
     sum(.nr$rarity$n_entries == 1))



# ------------------  Name weight as a score out of score_weights.name  --------
#
# The weight is a sum of log(N/df): evidence in NATS, which is a log-odds. The
# logistic is the map from a log-odds to [0, 1], so this is a change of units
# rather than a new rule -- nothing about which pairs beat which is altered.
#
# It cannot exceed 1, approaches it asymptotically, and so needs no clamp. That
# also means no pair is ever scored a flat maximum: two very strong names stay
# distinguishable, which a hard cap would throw away.
#
# NOT A FLOOR. Every pair gets a score, including the 68,464 that used to be
# deleted. A weight of 4 scores about 0.00002 -- indistinguishable from zero in
# practice, but still ordered, which a clamp at zero would not be.
NAME_SCORE_MID   <- match_param("name_weight.score_midpoint_mult")  * IDF_UNSEEN
NAME_SCORE_STEEP <- match_param("name_weight.score_steepness_mult") * IDF_UNSEEN
SCORE_W <- score_weights()

# Worth knowing, since the two were calibrated independently and against
# different things: the midpoint lands within a few hundredths of
# COMBINED_A_WEIGHT. That is a coincidence of this corpus, not a constraint --
# they are separate keys and either can move without the other.
dedup_name_score <- function(w) {
  1 / (1 + exp(-(w - NAME_SCORE_MID) / NAME_SCORE_STEEP))
}

dlog("  Name score: midpoint ", round(NAME_SCORE_MID, 3),
     " | steepness ", round(NAME_SCORE_STEEP, 3),
     " | weight of the name component ", SCORE_W[["name"]])
dlog("    weight 12 -> ", round(dedup_name_score(12), 3),
     " | 15 -> ", round(dedup_name_score(15), 3),
     " | 18 -> ", round(dedup_name_score(18), 3),
     " | 25 -> ", round(dedup_name_score(25), 3))
dlog("  Corroborator score (every field worth 1, a null hypothesis — see the ",
     "config): 0 of 5 -> ", round(corrob_score(0, 5), 3),
     " | 3 of 5 -> ", round(corrob_score(3, 5), 3),
     " | 5 of 5 -> ", round(corrob_score(5, 5), 3),
     " | nothing comparable -> ", round(corrob_score(0, 0), 3))

# Collocated token groups, from the same table. Each group is scored by the
# frequency of the WHOLE group, which needs no independence assumption: it is
# counted. See COLLOC_MIN_RATIO above and the long rationale in
# 1-build_name_rarity.R.
colloc_tokens <- .nr$groups
colloc_idf    <- .nr$group_idf

dlog("\nCOLLOCATIONS (read, detected over the pooled corpus)")
dlog("  Groups: ", length(colloc_tokens),
     " (", sum(lengths(colloc_tokens) >= 3L), " larger than a pair)")
for (i in seq_len(min(5L, length(colloc_tokens)))) {
  g <- colloc_tokens[[i]]
  dlog("    ", paste(g, collapse = "+"), "  idf ", round(colloc_idf[i], 2),
       " vs ", round(sum(idf_of(g)), 2), " summed")
}


# -------------------------  Blocking  -----------------------------------------
# A pair is only ever compared if it shares a name token. THAT IS THE WHOLE
# RULE NOW.
#
# There used to be a second requirement -- same province, or same health zone
# for tokens carried by more than BLOCK_COMMON_TOKEN_ROWS rows. It was never a
# statement about identity, only a way of keeping the comparison count down,
# and it silently made one class of duplicate impossible to find: a person
# recorded in Ituri and again in Nord-Kivu was never compared, so no score for
# that pair existed to be too low. In an outbreak with population movement that
# is the wrong thing to be certain about. The geographic fields remain as
# CORROBORATORS, which is where a disagreement about place should be weighed --
# it counts against the pair rather than deleting it unseen.
#
# BLOCK_COMMON_TOKEN_ROWS is therefore unused. Kept defined, because it is the
# obvious lever if the pair count turns out to be unworkable: skipping tokens
# above that frequency outright is what the cross-file matcher already does,
# and a pair sharing nothing but `kambale` was never going to clear the gate.
#
# EXPECT THE PAIR COUNT TO RISE SHARPLY, and check it before the run finishes
# rather than after -- the line below reports it. One common token carried by
# 1,200 rows contributes ~720,000 pairs on its own once province no longer
# splits it.
dlog("\nBLOCKING")
blk_row <- unlist(lapply(which(usable), function(i) rep.int(i, length(unique(tok_block[[i]])))),
                  use.names = FALSE)
blk_tok <- unlist(lapply(which(usable), function(i) unique(tok_block[[i]])),
                  use.names = FALSE)

if (!length(blk_tok)) {
  stop("No blockable name tokens found — nothing to compare.", call. = FALSE)
}

blk_tok_freq <- table(blk_tok)

dlog("  Blocking on the shared token alone; province and health zone no ",
     "longer restrict which pairs are compared.")
dlog("  Distinct blocking tokens: ", length(blk_tok_freq),
     " | carried by more than ", BLOCK_COMMON_TOKEN_ROWS, " rows: ",
     sum(as.numeric(blk_tok_freq) > BLOCK_COMMON_TOKEN_ROWS),
     " | largest: ", max(as.numeric(blk_tok_freq)), " rows")

groups <- split(blk_row, blk_tok)
groups <- groups[lengths(groups) > 1L]
pair_mat <- do.call(rbind, lapply(groups, dedup_make_pairs))

if (is.null(pair_mat) || !nrow(pair_mat)) {
  dlog("  No candidate pairs after blocking — nothing to review.")
  pair_mat <- matrix(integer(0), ncol = 2, dimnames = list(NULL, c("a", "b")))
}
pair_mat <- unique(pair_mat)
dlog("  Candidate pairs after blocking: ", nrow(pair_mat),
     "  (all possible pairs would be ",
     format(choose(sum(usable), 2), big.mark = ",", scientific = FALSE), ")")


# -------------------------  Global name-distance gate  ------------------------

dlog("\nNAME DISTANCE GATE (Jaro-Winkler <= ", GLOBAL_MAX_DIST, ")")
ia <- pair_mat[, 1]; ib <- pair_mat[, 2]

d_concat <- stringdist::stringdist(nm_concat[ia], nm_concat[ib], method = "jw", p = 0.1)
d_sorted <- stringdist::stringdist(nm_sorted[ia], nm_sorted[ib], method = "jw", p = 0.1)
g_dist <- pmin(d_concat, d_sorted, na.rm = TRUE)

keep <- !is.na(g_dist) & g_dist <= GLOBAL_MAX_DIST
ia <- ia[keep]; ib <- ib[keep]; g_dist <- g_dist[keep]
dlog("  Pairs passing the gate: ", length(ia))

if (!length(ia)) {
  dlog("Nothing to review.")
  write_log_file(.dedup_log, paste0(basename(epi_dir), "_dedup_log.txt"))
  epi_linelist_dedup_complete <- TRUE
} else {


# -------------------------  Token-level name scoring  -------------------------
# Every token of the shorter name must find a partner in the longer name, and
# at least two must match. Each matched pair contributes the IDF of the MORE
# COMMON of the two tokens, at full value.
#
# ---------------------------------------------------------------------------
# WHY THE MORE COMMON TOKEN, NOT THE RARER
# ---------------------------------------------------------------------------
# A misspelling is, by construction, rare: `kamnbale` appears once in 35,000
# records not because it is distinctive but because a finger slipped. Scoring on
# the rarer side would read that slip as a unique surname. Measured over the
# 1,246 fuzzy matches in the last run, taking the rarer token would add 1,874
# of inflation -- 1.5 per match, and 4+ on 84 of them, which is enough to move a
# pair two tiers on its own. `kambale`/`kamnbale` would jump from 3.6 to 10.5.
# The matcher would be systematically rewarding bad data entry.
#
# (This comment used to say "the rarer of the two", which is the opposite of
# what pmin() does and of what the `lucie`/`lycie` example shows. Corrected
# 2026-09-23; the code was always right.)
#
# ---------------------------------------------------------------------------
# WHY NO DISTANCE PENALTY ANY MORE
# ---------------------------------------------------------------------------
# A matched token used to be scaled by how far apart the two spellings were:
# full IDF at an identical spelling, falling linearly to half at the gate. That
# is removed, because it points the wrong way. Measured on LINELIST_01092026,
# holding token rarity constant, as a ratio of matches in pairs where 4-5
# independent fields agree to matches where 0-1 do:
#
#     token rarity          exact     1 edit    2 edits
#     common (idf<6)         1.09       2.56       2.75
#     mid    (6-8)           1.39       2.47       2.88
#     rare   (8-10)          3.86       4.86       4.83
#     very rare (10+)          --      12.17       8.00
#
# A near-miss spelling holds up BETTER than an exact match at every rarity
# level, not worse. The reason is the whole-name gate above: by the time two
# tokens are compared, the full names are already within GLOBAL_MAX_DIST of each
# other, and conditional on that a one-letter difference is almost certainly one
# person's name transcribed twice. An exact match carries no such guarantee --
# two different people share `masika` or `kavira` constantly.
#
# So the penalty was taxing the more reliable evidence. Note this finding is
# CONDITIONAL ON THE WHOLE-NAME GATE. It is not a general claim that misspelled
# matches beat exact ones; if GLOBAL_MAX_DIST ever loosens, re-measure.
#
# These two rules read as though they disagree -- a misspelled match is worth as
# much as an exact one, yet the misspelled form itself is assumed to carry no
# rarity. They answer different questions. Whether the pair is the same person:
# the near-miss is good evidence. How much information the token carries: the
# typo carries none, because it is a typo.

dlog("\nTOKEN MATCHING")
na_tok <- n_tok[ia]; nb_tok <- n_tok[ib]

# Flatten the token lists so a token can be addressed by (row, index).
tok_flat <- unlist(tok_score, use.names = FALSE)
tok_off  <- c(0L, cumsum(n_tok)[-length(n_tok)])

# One row per (pair, token_a, token_b) combination.
pid   <- rep.int(seq_along(ia), na_tok * nb_tok)
x_idx <- unlist(mapply(function(a, b) rep.int(seq_len(a), rep.int(b, a)),
                       na_tok, nb_tok, SIMPLIFY = FALSE), use.names = FALSE)
y_idx <- unlist(mapply(function(a, b) rep.int(seq_len(b), a),
                       na_tok, nb_tok, SIMPLIFY = FALSE), use.names = FALSE)

t_a <- tok_flat[tok_off[ia][pid] + x_idx]
t_b <- tok_flat[tok_off[ib][pid] + y_idx]

# Edit distance, not Jaro-Winkler. `dist` is now a COUNT OF EDITS: reported in
# the output for the reviewer, and used below only to break ties in the greedy
# assignment. Which pairs are ACCEPTED is name_tokens_match() on the next line.
#
# The metric comes from the config so this and name_tokens_match() cannot
# disagree about what an edit is -- computing the gate with one metric and
# reporting another would be a quiet lie in the review file.
t_d <- stringdist::stringdist(
  t_a, t_b,
  method = pipeline_configs$matching$token_match$metric)

idf_a <- idf_of(t_a); idf_b <- idf_of(t_b)

cand <- data.frame(
  pid = pid, x_idx = x_idx, y_idx = y_idx,
  # The MORE COMMON surface form, carried so the collocation lookup below asks
  # about the same token whose IDF was used. `lucie`/`lycie` scores as `lucie`.
  t_min = ifelse(idf_a <= idf_b, t_a, t_b),
  min_idf = pmin(idf_a, idf_b),
  dist = t_d,
  stringsAsFactors = FALSE
)
cand <- cand[name_tokens_match(t_a, t_b), , drop = FALSE]
# Full IDF. No distance scaling -- see WHY NO DISTANCE PENALTY above.
cand$weight <- cand$min_idf

# Greedy best-first assignment within each pair: take the heaviest token match,
# then the heaviest remaining match that reuses neither token, and so on. Each
# token may be used at most once per side.
#
# Done as repeated vectorised sweeps rather than a per-pair loop. In any sweep,
# a row that is BOTH the heaviest remaining row for its left token and the
# heaviest remaining for its right token cannot conflict with another such row
# (they would have to share a token, and then one of them would not be the
# heaviest for it), so all such rows can be taken at once. Removing every row
# that reuses a taken token and repeating reproduces greedy exactly, in at most
# MAX_TOKENS_PER_NAME sweeps.
#
# NOTE: a single !duplicated() pass over the sorted rows is NOT equivalent —
# duplicated() also counts rows that were themselves rejected, so a token whose
# best partner was taken by someone else would be dropped rather than falling
# back to its next-best. That under-counts tokens_matched and would wrongly fail
# the "every token of the shorter name matched" rule below.
#
# `dist` IS A TIE-BREAK, AND IT IS LOAD-BEARING NOW. While a matched token was
# scaled by distance, the exact partner always outweighed the near one and the
# ordering sorted itself out. With the penalty gone they weigh the SAME: both
# `marie`/`marie` and `marie`/`marte` are worth idf(`marie`), because the weight
# is the more common side either way. Without this key the winner would be
# decided by token position, so a name could match its typo in preference to its
# exact counterpart. Fewest edits first, then position for determinism.
cand <- cand[order(cand$pid, -cand$weight, cand$dist, cand$x_idx, cand$y_idx), ,
             drop = FALSE]
cand$key_x <- cand$pid * 16L + cand$x_idx
cand$key_y <- cand$pid * 16L + cand$y_idx

taken <- vector("list", MAX_TOKENS_PER_NAME)
rem <- cand
for (sw in seq_len(MAX_TOKENS_PER_NAME)) {   # `sw`, not `sweep` — base::sweep
  if (!nrow(rem)) break
  best_for_x <- !duplicated(rem$key_x)
  best_for_y <- !duplicated(rem$key_y)
  take <- best_for_x & best_for_y
  if (!any(take)) break
  taken[[sw]] <- rem[take, , drop = FALSE]
  used_x <- rem$key_x[take]; used_y <- rem$key_y[take]
  rem <- rem[!(rem$key_x %in% used_x | rem$key_y %in% used_y), , drop = FALSE]
}
matched <- do.call(rbind, taken[!vapply(taken, is.null, logical(1))])

name_weight <- rep(0, length(ia))
n_match     <- rep(0L, length(ia))
best_idf    <- rep(0, length(ia))

if (!is.null(matched) && nrow(matched)) {
  agg_wt   <- tapply(matched$weight, matched$pid, sum)
  agg_n    <- tapply(matched$weight, matched$pid, length)
  agg_best <- tapply(matched$min_idf, matched$pid, max)
  slot <- as.integer(names(agg_wt))
  name_weight[slot] <- as.numeric(agg_wt)
  n_match[slot]     <- as.integer(agg_n)
  best_idf[slot]    <- as.numeric(agg_best)
}

n_shorter  <- pmin(na_tok, nb_tok)
n_unmatched <- n_shorter - n_match

dlog("  Pairs where every token of the shorter name matched: ", sum(n_unmatched == 0))


# -------------------------  Dependence correction  ----------------------------
# Replace the summed parts of a collocated group with the group's own IDF.
# A token belongs to at most one group, so the loop claims rows as it goes,
# largest group first.
colloc_reduction <- rep(0, length(ia))
n_colloc_pairs <- 0L
if (!is.null(matched) && nrow(matched) && length(colloc_tokens)) {
  claimed <- rep(FALSE, nrow(matched))
  for (gi in seq_along(colloc_tokens)) {
    g <- colloc_tokens[[gi]]
    # Which pairs have EVERY token of this group still unclaimed? intersect()
    # over the group's tokens, not a %in% test, because a pair holding two of
    # three tokens must not be treated as holding the group.
    pid_sets <- lapply(g, function(t) matched$pid[!claimed & matched$t_min == t])
    hit <- Reduce(intersect, pid_sets)
    if (!length(hit)) next
    sel <- !claimed & matched$pid %in% hit & matched$t_min %in% g
    if (!any(sel)) next
    summed <- tapply(matched$weight[sel], matched$pid[sel], sum)
    # The group replaces its parts at full value. It used to be scaled by the
    # mean distance of its member tokens, mirroring the per-token discount --
    # that discount is gone, so the mirror goes with it and this is now a plain
    # substitution: drop what the parts scored, add what the whole group is
    # worth.
    slot <- as.integer(names(summed))
    colloc_reduction[slot] <- colloc_reduction[slot] +
      (as.numeric(summed) - colloc_idf[gi])
    claimed[sel] <- TRUE
    n_colloc_pairs <- n_colloc_pairs + length(slot)
  }
}

# -------------------------  Shared repetition  --------------------------------
# See SHARED_REPEAT_BONUS. Both sides repeating the same token, not merely one.
rep_a <- tok_repeat[ia]; rep_b <- tok_repeat[ib]
shared_repeat <- vapply(seq_along(ia), function(k) {
  length(intersect(rep_a[[k]], rep_b[[k]])) > 0L
}, logical(1))

# ---------------------------------------------------------------------------
# ONE NUMBER DECIDES EVERYTHING
# ---------------------------------------------------------------------------
# There used to be two. `name_weight_gate` -- the plain sum, before the
# collocation correction and the bonus -- decided whether a pair reached the
# review file, while the corrected `name_weight` decided its tier. The argument
# was that gating on the corrected figure removed 19 pairs from the file
# entirely and 4 of them looked genuine; a mis-tiered pair is still reviewable,
# an absent one is invisible.
#
# Unified on 2026-09-23, for two reasons.
#
# It is untenable where this is going. A link is to carry ONE score out of 1.0
# (matching.score_weights), and a single published score cannot be built from a
# number that decided visibility differing from the number that decided
# confidence. "The name score" has to mean one thing.
#
# And the original objection has largely dissolved. Removing the distance
# penalty lifted every weight, so far fewer pairs now sit close enough to the
# floor for the collocation correction to push them under. Re-measured on
# LINELIST_01092026 under the rules above: 12 pairs leave rather than 19, and
# 9 of the 12 have just 1 of 5 corroborating fields agreeing -- precisely the
# pairs that should go.
#
# ONE CASUALTY REMAINS, and it is the same one the old comment named:
# `BARAKA JEAN PIERRE` / `BARAKA JEAN-PERRE`, 4 of 5 fields agreeing, 14.6 ->
# 11.1 against a floor of 12. It drops for a defensible reason -- `jean pierre`
# is a collocated compound, so counting it as two independent pieces of evidence
# was the error, not the correction. Whether 11.1 belongs below the floor is a
# question about MIN_NAME_WEIGHT, not about having two numbers.
#
# The components are still WRITTEN OUT (name_weight_plain, colloc_reduction,
# shared_repeat), because a reviewer looking at that Baraka pair should be able
# to see why it scored 11.1. They decide nothing.
name_weight_plain <- name_weight
name_weight <- name_weight - colloc_reduction +
  SHARED_REPEAT_BONUS * as.numeric(shared_repeat)

dlog("  Pairs reduced by a collocated group: ", n_colloc_pairs,
     " | pairs earning the shared-repetition bonus (+", SHARED_REPEAT_BONUS,
     "): ", sum(shared_repeat))


# -------------------------  Corroborators  ------------------------------------
# Independent of the name score; the corroborator half of the link score.
# Agreement adds confidence;
# disagreement is never treated as proof of two different people.

c_sex  <- as.integer(nzchar(sex1[ia]) & nzchar(sex1[ib]) & sex1[ia] == sex1[ib])
c_zone <- as.integer(nzchar(hzone[ia]) & hzone[ia] == hzone[ib])
c_area <- as.integer(nzchar(harea[ia]) & harea[ia] == harea[ib])
c_age  <- as.integer(!is.na(age_num[ia]) & !is.na(age_num[ib]) &
                       abs(age_num[ia] - age_num[ib]) <= AGE_TOLERANCE_YEARS)
onset_gap <- as.numeric(abs(onset[ia] - onset[ib]))
c_onset <- as.integer(!is.na(onset_gap) & onset_gap <= ONSET_TOLERANCE_DAYS)

n_corr <- c_sex + c_zone + c_area + c_age + c_onset

# ---- How many of the five could be compared AT ALL, per pair -----------------
# A field only counts as available when BOTH rows carry a value: a blank cannot
# agree, so counting it as available silently raises the bar for that pair.
# This is the same failure the fractional thresholds in 3-deduplicate_contacts.R
# and 2-build-person-candidates.R were introduced to fix, one level further
# down -- there it was one denominator per LINK TYPE, here it is one per PAIR.
#
# It is the denominator of the corroborator half of the link score
# (corrob_score() in 0-setup.R), so a blank field is neither credit nor
# penalty.
#
# It matters much more once corroborators are weighted rather than counted,
# which is why the column is being recorded now: the fields are worth 0.76
# (sex) to 3.52 (health area) bits, so WHICH field is missing changes the pair's
# ceiling by up to 4.6x. Measured on LINELIST_01092026, onset is uncomparable on
# 23.4% of pairs and age on 1.6%; sex, zone and area on none.
#
# 2-build-person-candidates.R reads this column and falls back to assuming 5
# when it is absent, warning as it does so. On the 2026-09-05 output that
# fallback was right for the ~2,345 pairs where all five are comparable and
# wrong for the other 754 (734 with four, 20 with three). The log reports the
# split for each run, so the figure stays current.
n_avail <- as.integer(
  (nzchar(sex1[ia]) & nzchar(sex1[ib])) +
  (nzchar(hzone[ia]) & nzchar(hzone[ib])) +
  (nzchar(harea[ia]) & nzchar(harea[ib])) +
  (!is.na(age_num[ia]) & !is.na(age_num[ib])) +
  (!is.na(onset[ia]) & !is.na(onset[ib]))
)

# An agreement on a field counted as unavailable would mean the two disagree
# about what "blank" is. Cheap to assert, and it would catch a future edit that
# changed one side's blank test without the other.
stopifnot(all(n_corr <= n_avail))


# -------------------------  Which pairs are kept  -----------------------------
# THE LINK SCORE DECIDES, and nothing else: a pair is written when its link
# score (name half + corroborator half, each out of 1, weighted by
# pipeline_configs.json score_weights) is at least link_score.min_keep, and
# rejected below it -- those are not weak candidates, they are not candidates.
# 193,514 of 220,606 compared pairs on the run that cutoff was chosen from were
# a single shared surname in two different health zones.
#
# (There used to be name-weight floors and a tier per pair -- A, B, C, S --
# set by name strength and corroborator counts. Retired on 2026-10-03: the
# score already weighs both, and nothing downstream read the tier.)
#
# It costs one thing: the file stops describing what lies below the cutoff, so
# the curve down there cannot be re-fitted from it. Lower the key and re-run
# before fitting anything that depends on that shape.
MIN_LINK_SCORE <- match_param("link_score.min_keep")
.link_score_all <- round(
  round(SCORE_W[["name"]] * dedup_name_score(name_weight), 4) +
    round(SCORE_W[["corroborator"]] * corrob_score(n_corr, n_avail), 4), 4)
sel <- !is.na(.link_score_all) & .link_score_all >= MIN_LINK_SCORE
dlog("  Rejecting link_score < ", MIN_LINK_SCORE, ": ",
     sum(!sel), " of ", length(sel), " compared pair(s) dropped, ",
     sum(sel), " written")

# Reported over the pairs that reach the FILE, not every pair compared, so the
# figure matches what a reader can count in duplicate_candidates.csv.
dlog("\nCOMPARABLE CORROBORATOR FIELDS (per pair, of 5)")
dlog("  all five: ", sum(n_avail[sel] == 5L),
     " | four: ",   sum(n_avail[sel] == 4L),
     " | three or fewer: ", sum(n_avail[sel] <= 3L))
dlog("  Uncomparable because one side is blank -- onset: ",
     sum(sel & !(!is.na(onset[ia]) & !is.na(onset[ib]))),
     " | age: ", sum(sel & !(!is.na(age_num[ia]) & !is.na(age_num[ib]))))

dlog("\nCANDIDATE PAIRS")
if (any(sel)) {
  .q <- stats::quantile(.link_score_all[sel], c(0, .5, 1))
  dlog("  link score: min ", round(.q[1], 3), " | median ", round(.q[2], 3),
       " | max ", round(.q[3], 3))
} else {
  dlog("  No pair cleared the link score — an empty review table will be written.")
}
dlog("  Total candidate pairs: ", sum(sel))
dlog("  Distinct rows involved: ", length(unique(c(ia[sel], ib[sel]))))


# -------------------------  Build the review table  ---------------------------

ai <- ia[sel]; bi <- ib[sel]
# How many candidate partners each row has. A high count is
# a review signal in its own right — either one person recorded many times, or
# a name common enough that the matching cannot separate its bearers.
pair_counts <- table(c(ai, bi))
n_cand_a <- as.integer(pair_counts[as.character(ai)])
n_cand_b <- as.integer(pair_counts[as.character(bi)])

# Length-matched to ai/bi even when the column is absent, so a run with zero
# candidate pairs still produces a valid empty table rather than a
# "differing number of rows" error.
pick <- function(nm) {
  if (!nm %in% names(ll)) {
    return(list(a = rep(NA_character_, length(ai)),
                b = rep(NA_character_, length(bi))))
  }
  list(a = ll[[nm]][ai], b = ll[[nm]][bi])
}
# Only the identifiers are copied into the file. It used to carry both
# records' name, sex, age, location, dates, classification, lab ID and
# telephone, for reviewing pairs in a spreadsheet; that review moved to the
# Person Review tab, and the only reader of this file
# (2-build-person-candidates.R) uses the identifiers and the scores. Dropping
# them halves the file and keeps names and telephone numbers out of it. Any
# record's details are one lookup away: row_a / row_b are row numbers in this
# export's dhis2_processed_linelist_names.csv, and alert_id_a / alert_id_b the
# IDs.
v_alert <- pick("alert_id")

# Rounded once, here, so the three columns and their total are arithmetically
# consistent in the file rather than each independently rounded from a
# different exact value.
.name_score   <- round(dedup_name_score(name_weight[sel]), 4)
.name_points  <- round(SCORE_W[["name"]] * dedup_name_score(name_weight[sel]), 4)
.corrob_score <- round(corrob_score(n_corr[sel], n_avail[sel]), 4)
.corrob_points <- round(SCORE_W[["corroborator"]] *
                          corrob_score(n_corr[sel], n_avail[sel]), 4)

out <- data.frame(
  pair_id            = seq_along(ai),
  n_candidates_for_a = n_cand_a,
  n_candidates_for_b = n_cand_b,

  # --- the two rows
  row_a = ai, row_b = bi,
  alert_id_a = v_alert$a, alert_id_b = v_alert$b,

  # --- the evidence
  # The name evidence, in nats: the name half of the link score.
  name_weight    = round(name_weight[sel], 2),
  # The same evidence on a 0-1 scale, and then as its share of the total. Kept
  # beside the weight rather than replacing it: the weight is what the curve is
  # calibrated against, and a score with no way back to it cannot be checked.
  name_score     = .name_score,
  name_points    = .name_points,
  # The other half. corrob_score() is in 0-setup.R so the three matchers
  # cannot turn the same two counts into different numbers.
  corrob_score   = .corrob_score,
  corrob_points  = .corrob_points,
  # THE NUMBER THAT DECIDES, out of 1: a pair is written only when this is at
  # least link_score.min_keep.
  #
  # The sum of the two ROUNDED halves, not a rounded sum of the two exact
  # ones. Those differ by up to 1e-4, which is invisible until a reviewer adds
  # up the two columns beside it and gets a different answer -- and until a
  # test asserts the total is its parts, which it should.
  link_score     = round(.name_points + .corrob_points, 4),
  # The three columns below are DIAGNOSTIC ONLY -- nothing branches on them.
  # They break `name_weight` into its parts so a reviewer can see how a score
  # was built: the plain de-duplicated sum, what the dependence correction took
  # off, and whether the repetition bonus applied. A pair sitting just under the
  # floor because `jean pierre` collapsed into one unit should be explicable
  # from the file rather than by reading this script.
  name_weight_plain = round(name_weight_plain[sel], 2),
  colloc_reduction  = round(colloc_reduction[sel], 2),
  shared_repeat     = shared_repeat[sel],
  name_dist      = round(g_dist[sel], 4),
  tokens_matched = n_match[sel],
  tokens_unmatched = n_unmatched[sel],
  rarest_matched_token_idf = round(best_idf[sel], 2),
  corroborators  = n_corr[sel],
  # How many of the five could be compared at all on THIS pair. Sits beside
  # `corroborators` so "3" can be read as the fraction it is -- 3 of 5 and 3 of
  # 4 are different statements. 2-build-person-candidates.R reads this column
  # and warns that it is assuming 5 when it is absent.
  n_corroborators_available = n_avail[sel],
  sex_agrees     = as.logical(c_sex[sel]),
  zone_agrees    = as.logical(c_zone[sel]),
  area_agrees    = as.logical(c_area[sel]),
  age_agrees     = as.logical(c_age[sel]),
  onset_agrees   = as.logical(c_onset[sel]),
  onset_gap_days = onset_gap[sel],
  # (No reviewer columns: pairs are not reviewed in this file. Decisions are
  # made on the clusters in the Person Review tab and kept in link_ledger.csv.)

  stringsAsFactors = FALSE
)

# Strongest first.
out <- out[order(-out$link_score, -out$name_weight, -out$corroborators), , drop = FALSE]
row.names(out) <- NULL

out_path <- file.path(epi_dir, "duplicate_candidates.csv")
write_processed_csv(out, out_path)
dlog("\nWrote ", nrow(out), " candidate pairs to ", out_path)

# NEAR MISSES: pairs scoring in [near_miss.min_score, link_score.min_keep).
# Not candidates -- nothing downstream merges, clusters or queues them. They go
# to a separate file so the Algorithm Tuning tab can audit what the cutoff
# throws away (see near_miss in pipeline_configs.json). Identifiers and scores
# only, like the main file.
NEAR_MISS_MIN <- match_param("near_miss.min_score")
.nm <- !is.na(.link_score_all) & .link_score_all >= NEAR_MISS_MIN &
       .link_score_all < MIN_LINK_SCORE
.nm_alert <- if ("alert_id" %in% names(ll)) ll$alert_id else rep(NA_character_, nrow(ll))
nm_out <- data.frame(
  alert_id_a = .nm_alert[ia[.nm]], alert_id_b = .nm_alert[ib[.nm]],
  name_weight = round(name_weight[.nm], 2),
  name_dist   = round(g_dist[.nm], 4),
  link_score  = .link_score_all[.nm],
  corroborators = n_corr[.nm],
  n_corroborators_available = n_avail[.nm],
  stringsAsFactors = FALSE)
write_processed_csv(nm_out, file.path(epi_dir, "near_miss_candidates.csv"))
dlog("Wrote ", nrow(nm_out), " near-miss pairs (link score ", NEAR_MISS_MIN,
     " to under ", MIN_LINK_SCORE, ") to near_miss_candidates.csv")

# WHICH CORPUS PRODUCED THESE SCORES, recorded beside them.
#
# Every weight in duplicate_candidates.csv is a sum of IDFs, and an IDF is
# log(N/n): the same pair of names scores differently under a different N. The
# file used to be self-describing in that respect, because the corpus was this
# export's own rows and could be reconstructed from the export. It is now the
# pooled running table, which is overwritten on every build -- so without this,
# a candidate file and the numbers that justify it drift apart with nothing on
# disk to say they have.
#
# tests/test_name_score.py reads this to tell which corpus a given
# duplicate_candidates.csv was scored under; several of its assertions are
# measurements of the linelist-only corpus and are not claims about this one.
writeLines(
  paste0(
    '{\n',
    '  "rarity_source": "pooled_running_table",\n',
    '  "rarity_stem": ', if (is.null(.nr$stem) || is.na(.nr$stem)) 'null'
                          else paste0('"', .nr$stem, '"'), ',\n',
    '  "rarity_built_at": ', if (is.null(.nr$built_at)) 'null'
                             else paste0('"', .nr$built_at, '"'), ',\n',
    '  "corpus_entries": ', N_named, ',\n',
    '  "idf_unseen": ', round(IDF_UNSEEN, 4), ',\n',
    # How a token was matched and what a match was worth. Both changed on
    # 2026-09-23 and both move every weight in the file, so a candidate file
    # that does not say which rules produced it cannot be compared with another.
    '  "token_match_rule": "edit_budget",\n',
    '  "token_match_metric": "',
        pipeline_configs$matching$token_match$metric, '",\n',
    '  "token_distance_penalty": false,\n',
    # The one rule that decides what is written (no tiers, no name floors
    # since 2026-10-03).
    '  "min_link_score": ', MIN_LINK_SCORE, ',\n',
    '  "n_candidate_pairs": ', nrow(out), '\n',
    '}\n'
  ),
  file.path(epi_dir, "dedup_scoring_meta.json")
)

# Rows with an implausible number of candidate partners usually mean a name too
# common for the matching to separate, not a person recorded that many times.
busy <- max(c(out$n_candidates_for_a, out$n_candidates_for_b), na.rm = TRUE)
if (is.finite(busy) && busy >= 5) {
  dlog("  Busiest row appears in ", busy, " candidate pairs — review rows with ",
       "a high n_candidates_for_* with extra scepticism.")
}

dlog("\nNOTE: nothing has been removed, merged, or resolved. ",
     "The linelist CSVs are unchanged; ", basename(out_path),
     " is a review table only.")

write_log_file(.dedup_log, paste0(basename(epi_dir), "_dedup_log.txt"))

duplicate_candidates <- out
epi_linelist_dedup_complete <- TRUE

}


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, c("duplicate_candidates",
                                       "epi_linelist_dedup_complete"))
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
