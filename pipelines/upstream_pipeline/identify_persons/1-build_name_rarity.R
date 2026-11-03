#!/usr/bin/env Rscript
# 1-build_name_rarity.R
#
# How unusual is a name token, across everyone known to this outbreak?
#
# Pools the epi linelist and the contact tracing export into one corpus and
# writes an inverse-document-frequency (IDF) score per name token. Reads both
# processed exports; changes neither.
#
# Output: data/processed/person_matching/<stem>/name_rarity/
#           name_rarity.csv, name_collocations.csv, name_rarity_meta.json
#         data/processed/person_matching/<stem>/matching_params.json
#         the name_rarity section of data/processed/latest.json, naming <stem>
#           as the current table
#
# ---------------------------------------------------------------------------
# THIS RUNS BEFORE EITHER DEDUPLICATION
# ---------------------------------------------------------------------------
# It used to run after both, as a cross-file step, and the two dedups each
# computed their own token rarity and their own collocations from whatever file
# they had in hand. That gave three incompatible definitions of "rare" for the
# same outbreak: the linelist's over 35,105 entries, CT's over 2,798 (saturating
# at log(2798) = 7.94, so `zilongo` scored 10.47 in one and 7.94 in the other),
# and this table's over the pool.
#
# Token rarity is a property of the outbreak's whole name population. Nothing
# about it depends on which spreadsheet a name arrived in, so it is measured
# once, over everyone, before anything is matched. Both dedups and
# 2-build-person-candidates.R now read the result.
#
# The order is: epi 1-colname-standard -> CT 1-colname-standard ->
# CT 2-assign-contact-ids -> THIS -> epi 2-deduplicate_linelist ->
# CT 3-deduplicate_contacts -> identify_persons 2-build-person-candidates.
#
# ---------------------------------------------------------------------------
# WHY THERE IS ALWAYS A CURRENT TABLE
# ---------------------------------------------------------------------------
# latest.json always names the current snapshot, so a consumer has a table it
# can rely on without knowing which export dates were last paired; under the
# old dated-folder-only layout there was no such pointer, which is the direct
# reason each script grew its own copy of this computation. (A current/ folder
# holding a byte copy of the newest snapshot did this job until latest.json
# took it over.)
#
# It also settles the objection that kept 2-deduplicate_linelist.R
# self-sufficient: 11 of 51 linelist export dates have no CT counterpart, so
# the dedup could not depend on a table that might not exist for its date. A
# current table always exists — on a linelist-only date it is simply the
# last pooled build, which is a better corpus than that date's linelist alone,
# not a worse one.
#
# `<stem>/name_rarity/` is kept per run, because a rarity score is evidence
# behind a review decision and IDF moves as the corpus grows: a reviewer's
# judgement is only auditable against the numbers they actually saw.
#
# ---------------------------------------------------------------------------
# WHY THIS EXISTS AS A SHARED TABLE
# ---------------------------------------------------------------------------
# 2-deduplicate_linelist.R already computes token IDF internally, and it is the
# reason that matcher works: name-token *rarity* carries information a string
# distance cannot express. Two people both called `jean kambale` share nothing
# useful; two people both called `zilongo` almost certainly are the same person.
#
# That measure was private to one script. Cross-file matching needs the same
# notion of "unusual", and if it computed its own the two would silently drift
# apart — the same pair could be near-certain to one and marginal to the other.
#
# ---------------------------------------------------------------------------
# THE THREE RULES, EACH MEASURED RATHER THAN ASSUMED
# ---------------------------------------------------------------------------
# Measured on LINELIST_01092026 x CONTACT-TRACING_01092026.
#
# 1. POOL BOTH FILES. Neither works alone.
#
#    CT alone SATURATES: 2,798 entries means its IDF ceiling is log(2798) =
#    7.94, so a genuinely rare name and a merely-unseen one score identically.
#    `zilongo` scores 10.47 in the linelist and 7.94 in CT purely because CT is
#    small.
#
#    The linelist alone UNDER-COVERS: 881 of 2,552 CT tokens (34%) never appear
#    in it, so they would fall back to IDF_UNSEEN and look maximally rare —
#    inflating confidence on exactly the CT<->linelist links needing most care.
#
# 2. COUNT ONCE PER IDENTIFIER, NEVER ONCE PER ROW. CT is one row per
#    follow-up: median 7, up to 26. Counting rows would inflate a contact's
#    name up to 26-fold and make a rare name look common. That error suppresses
#    true matches, silently, and would be very hard to spot afterwards. So the
#    corpus is collapsed to distinct alert_id / contact_id /
#    source_case_alert_id before anything is counted.
#
# 3. THE LINELIST DEDUP BARELY MOVES; THE CT DEDUP MOVES A LOT. Pooling shifts
#    linelist-internal IDF by a median of 0.077, with 44 of 22,929 tokens
#    moving more than 1.0 — the linelist is 93% of the pooled corpus. So for
#    2-deduplicate_linelist.R, reading this table instead of computing its own
#    is a consistency fix, not a numerical one; it was previously left
#    self-sufficient for the 11 export dates with no CT counterpart, and the
#    always-present current table removes that reason (see the header).
#
#    For 3-deduplicate_contacts.R it IS a correctness fix. Its own corpus is
#    2,798 entries and saturates, so every token seen once in CT scored 7.94
#    whether it was `zilongo` or a typo. On the pooled corpus the same tokens
#    separate properly.
#
# ---------------------------------------------------------------------------
# CONFIDENTIALITY
# ---------------------------------------------------------------------------
# name_rarity.csv CONTAINS PATIENT NAME FRAGMENTS. 69% of tokens occur exactly
# once, so a single-occurrence row is, in effect, one person's name. Handle it
# exactly like dhis2_processed_linelist_names.csv. It is an internal working
# artefact and must not go into any ctsafe or shared export.


# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# -------------------------  Configuration  ------------------------------------

# The tokeniser is name_tokens() in 0-setup.R, defined once for every script.
# These are only for the metadata block; name_tokens() reads the same two
# config keys as its own defaults.
NR_MIN_TOKEN_CHARS <- match_param("tokenisation.score_min_token_chars")
NR_MAX_TOKENS      <- match_param("tokenisation.max_tokens_per_name")

# Where the corpus comes from: column pairs of (identifier, name) per source.
# The identifier is what the corpus is deduplicated on — see rule 2 above.
NR_SOURCES <- list(
  list(source = "linelist",   id = "alert_id",               name = "name"),
  list(source = "ct_contact", id = "contact_id",             name = "contact_name"),
  list(source = "ct_case",    id = "source_case_alert_id",   name = "case_name")
)

# ---------------------------------------------------------------------------
# COLLOCATIONS: WHY THE IDF OF A TOKEN IS NOT THE WHOLE STORY
# ---------------------------------------------------------------------------
# Every matcher here scores a name by SUMMING the IDF of its matched tokens.
# That sum assumes the tokens are independent. They are not. Measured on the
# pooled corpus, `de` and `dieu` appear together 49 times where independence
# predicts 0.6 -- 82 times more often than chance.
#
# The consequence is not academic. Summing the parts of `JEAN DE DIEU` gives
# 15.29 when the honest joint figure is 6.76, because 44 entries carry all
# three tokens. That 8.53 of inflation is nearly three times the whole width of
# the tier band (min 12.12, strong 15.08), so a name shared by 44 identifiers
# scores as if it were unique. `MBATEKO JEAN DE DIEU` reaches 20.50 --
# comfortably "near certain" -- while six identifiers share it exactly.
#
# So this table records which token pairs co-occur far more than chance, and
# the matchers merge such groups into a single unit scored by the frequency of
# the WHOLE group. A merged unit needs no independence assumption: it is
# counted, not inferred.
#
# WHY A RATIO OF 10, AND WHY IT IS NOT A TASTE CALL. The threshold is bounded
# above by the case it exists to fix. A group merges only if EVERY pair inside
# it is collocated (see nr_collocation_cliques below), and the weakest edge of
# {jean, de, dieu} is `dieu`+`jean` at 12.3x. At 13x the triple stops merging
# and the whole inflation returns. 10x is the highest round value that keeps it
# with margin. It yields 29 pairs.
#
# What the threshold buys, measured:
#
#     ratio    pairs   {jean, de, dieu} merges?
#       6x        45   yes
#       8x        34   yes
#      10x        29   yes          <- chosen
#      12x        24   yes
#     12.4x       23   NO
#      19x        20   NO
#
# Above roughly 19x the members are all compound given names or placeholder
# text (`jean baptiste`, `dieu merci`, `la joie`, `ma fille`, `tout passe`).
# Between 8x and 14x they are mostly surname + given-name family pairs
# (`jean claude`, `marie louise`, `jean paul`) -- also genuine
# dependence, and merging them is arithmetically right, but that kind of
# dependence is pervasive, so a low threshold slides the score towards full
# joint rarity. Full joint rarity was measured to be WORSE at telling true
# matches from false ones (AUC 0.573 against 0.659 for the plain sum), because
# a full name's token combination is unique for 84% of entries and so carries
# no ranking information at all. A light, targeted correction is the point.
#
# WHY POOLED, NOT PER FILE. The contact tracing export alone detects ZERO
# collocations: with 2,539 distinct contacts, `dieu merci` appears 7 times and
# everything else once or twice, so nothing clears the occurrence floor. Same
# reason the rarity table is pooled.
#
# The linelist alone finds 28 groups against the pool's 30, agreeing on 27 --
# it is 93% of the corpus -- and both find the same single triple. So
# 2-deduplicate_linelist.R may detect its own and stay self-sufficient for the
# 11 export dates with no CT counterpart. Every disagreement is a threshold
# near-miss rather than a real difference of opinion: `jean`+`luc` co-occurs 8
# times pooled and 7 in the linelist, one short of the floor, so a high ratio
# is no protection against falling out. tests/test_name_score.py pins the four
# and asserts each is a near-miss, which is a weaker claim than "the strong
# core is identical" -- that stronger version is FALSE, and was tried.
NR_COLLOC_MIN_RATIO <- match_param("collocation.min_ratio")
# An occurrence floor as well as a ratio, because a ratio computed on two
# co-occurrences is noise: two tokens each seen once, appearing together once,
# scores an enormous ratio and means nothing.
NR_COLLOC_MIN_TOGETHER <- as.integer(match_param("collocation.min_together"))


# -------------------------  Helper Functions  ---------------------------------

.nr_log <- character(0)
nrlog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .nr_log <<- c(.nr_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

nr_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", qmethod = "double")
}

# ---- Tokenisation -----------------------------------------------------------
# name_translit_ascii() and name_tokens() live in 0-setup.R and are the ONLY
# copy. They were previously duplicated verbatim here and in the two dedups and
# the candidate builder, with tests extracting each copy and asserting they
# matched character for character -- an arrangement that detected drift rather
# than preventing it, and made any change to tokenisation a four-file edit.

nr_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# ---- Collocation detection --------------------------------------------------
# name_pair_counts() and name_collocation_cliques() likewise live in 0-setup.R.
# The clique rule -- every pair inside a group must itself be collocated, not
# merely connected -- and the reason it is not transitive are documented there.

# One entry per distinct identifier. Where an identifier carries more than one
# spelling across rows (data entry variation), the most frequent wins — the
# same rule 4-build-id-lookup.R uses when collapsing follow-ups.
nr_collapse <- function(df, id_col, name_col, source_label) {
  if (is.null(df) || !nrow(df)) return(NULL)
  if (!all(c(id_col, name_col) %in% names(df))) return(NULL)
  ids <- trimws(as.character(df[[id_col]]))
  nms <- trimws(as.character(df[[name_col]]))
  keep <- !nr_blank(ids) & !nr_blank(nms)
  if (!any(keep)) return(NULL)
  ids <- ids[keep]; nms <- nms[keep]
  chosen <- vapply(split(nms, ids), function(v) {
    tb <- table(v)
    names(tb)[which.max(as.numeric(tb))]
  }, character(1))
  data.frame(source = source_label,
             identifier = names(chosen),
             name = unname(chosen),
             stringsAsFactors = FALSE)
}


# -------------------------  Locate the inputs  --------------------------------

epi_processed <- file.path(repo_root, "data", "processed",
                           "dhis2_linelist_processed")
ct_processed  <- file.path(repo_root, "data", "processed",
                           "contact_tracing_processed")

nrlog("=== Building the pooled name rarity table ===")

# The _names.csv is required: the de-identified export has no name columns at
# all, so there is nothing to count.
epi_dir  <- tryCatch(resolve_epi_latest_dir(epi_processed), error = function(e) NA_character_)
epi_csv  <- if (!is.na(epi_dir)) file.path(epi_dir, "dhis2_processed_linelist_names.csv") else NA_character_
# CT input via the shared resolver (combined or latest, per
# contact_tracing.data.matching_input). No CT at all is a normal state in
# "latest" mode and builds a linelist-only table; in "combined" mode a missing
# combined dataset is an error, because quietly dropping CT from the corpus is
# not what anyone who chose "combined" asked for.
ct_in <- tryCatch(resolve_ct_matching_input(ct_processed), error = function(e) e)
if (inherits(ct_in, "error")) {
  if (identical(pipeline_configs$contact_tracing$data$matching_input, "combined"))
    stop(conditionMessage(ct_in), call. = FALSE)
  ct_in <- NULL
}
ct_dir   <- if (!is.null(ct_in)) ct_in$dir else NA_character_
ct_csv   <- if (!is.null(ct_in)) ct_in$names_csv else NA_character_

if (is.na(epi_csv) || !file.exists(epi_csv)) {
  stop("No processed linelist _names.csv found. Run ",
       "epi_linelist_pipeline/1-colname-standard.R first.\n  Looked for: ",
       if (is.na(epi_csv)) "(no latest.json)" else epi_csv, call. = FALSE)
}

epi_stem <- basename(epi_dir)
ct_stem  <- if (!is.na(ct_csv) && file.exists(ct_csv)) ct_in$stem else NA_character_

read_names <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                  colClasses = "character")
}

ll <- read_names(epi_csv)
nrlog("  linelist: ", epi_stem, " (", nrow(ll), " rows)")

ct <- NULL
if (!is.na(ct_stem)) {
  ct <- read_names(ct_csv)
  nrlog("  contact tracing: ", ct_stem, " (", nrow(ct), " rows)")
} else {
  # 11 of 51 linelist export dates have no CT counterpart, so this is a normal
  # state, not an error. The table is still built — it is just linelist-only,
  # and says so, because a consumer must be able to tell.
  nrlog("  NOTE: no processed contact tracing export found. Building a ",
        "LINELIST-ONLY corpus.")
  nrlog("        CT name tokens absent from the linelist (34% of them on the ",
        "last paired run) will score as unseen.")
}


# -------------------------  Build the corpus  ---------------------------------

parts <- list()
for (s in NR_SOURCES) {
  df <- if (s$source == "linelist") ll else ct
  p  <- nr_collapse(df, s$id, s$name, s$source)
  if (!is.null(p)) parts[[length(parts) + 1L]] <- p
}
corpus <- do.call(rbind, parts)
if (is.null(corpus) || !nrow(corpus)) {
  stop("No usable (identifier, name) pairs found in either export.", call. = FALSE)
}

N <- nrow(corpus)
nrlog("\n  Corpus, one entry per identifier:")
by_src <- table(corpus$source)
for (k in names(by_src)) nrlog("    ", k, ": ", as.numeric(by_src[k]))
nrlog("    TOTAL: ", N)


# -------------------------  Count and score  ----------------------------------

tok_lists <- name_tokens(corpus$name)

# unique() per entry: document frequency, so a name repeating a token counts
# once. Matches the dedup, which does the same before building its IDF.
tok_per_entry <- lapply(tok_lists, unique)
n_tok <- lengths(tok_per_entry)

flat_tok <- unlist(tok_per_entry, use.names = FALSE)
flat_src <- rep(corpus$source, n_tok)

if (!length(flat_tok)) stop("No tokens survived tokenisation.", call. = FALSE)

tok_freq <- table(flat_tok)
tokens   <- names(tok_freq)
n_entries <- as.numeric(tok_freq)

# Per-source counts, so a reader can see whether a token is CT-only, which is
# what makes the pooling necessary in the first place.
src_counts <- function(src) {
  t <- table(factor(flat_tok[flat_src == src], levels = tokens))
  as.numeric(t)
}

rarity <- data.frame(
  token          = tokens,
  n_entries      = n_entries,
  idf            = round(log(N / n_entries), 4),
  n_linelist     = src_counts("linelist"),
  n_ct_contact   = src_counts("ct_contact"),
  n_ct_case      = src_counts("ct_case"),
  stringsAsFactors = FALSE
)
rarity <- rarity[order(-rarity$n_entries, rarity$token), , drop = FALSE]

# The score for a token this corpus has never seen. Callers need it explicitly:
# defaulting an unseen token to 0 would treat an unknown name as maximally
# common, which is backwards and the more dangerous direction.
idf_unseen <- round(log(N), 4)

nrlog("\n  Distinct tokens: ", nrow(rarity),
      " | occurring exactly once: ", sum(rarity$n_entries == 1),
      " (", round(100 * sum(rarity$n_entries == 1) / nrow(rarity)), "%)")
nrlog("  IDF range: ", min(rarity$idf), " to ", max(rarity$idf),
      " | unseen token scores ", idf_unseen)

if (!is.na(ct_stem)) {
  ct_only <- sum(rarity$n_linelist == 0 &
                   (rarity$n_ct_contact > 0 | rarity$n_ct_case > 0))
  nrlog("  Tokens appearing ONLY in contact tracing: ", ct_only,
        " — these are the ones a linelist-only table would mis-score.")
}

nrlog("\n  Most common tokens (least informative):")
for (i in seq_len(min(8L, nrow(rarity)))) {
  nrlog("    ", rarity$token[i], "  n=", rarity$n_entries[i],
        "  idf=", rarity$idf[i])
}


# -------------------------  Collocations  -------------------------------------
# See the rationale block by NR_COLLOC_MIN_RATIO. Every group emitted here
# carries its own entry count, so a matcher never has to intersect token sets
# itself -- it looks the group up and uses the stored idf.

idf_of_token <- setNames(rarity$idf, rarity$token)
entries_of   <- setNames(rarity$n_entries, rarity$token)

pairs_all <- name_pair_counts(tok_per_entry)
pairs_all$expected <- entries_of[pairs_all$a] * entries_of[pairs_all$b] / N
pairs_all$ratio    <- pairs_all$together / pairs_all$expected

edges <- pairs_all[pairs_all$together >= NR_COLLOC_MIN_TOGETHER &
                     is.finite(pairs_all$ratio) &
                     pairs_all$ratio >= NR_COLLOC_MIN_RATIO, , drop = FALSE]
edges <- edges[order(-edges$ratio), , drop = FALSE]

nrlog("\n  Collocations (>= ", NR_COLLOC_MIN_RATIO, "x chance, seen together ",
      ">= ", NR_COLLOC_MIN_TOGETHER, " times): ", nrow(edges), " token pairs")

groups <- name_collocation_cliques(edges[, c("a", "b")])

# Entry count for a whole group, by intersecting the entries that contain each
# token. Only ~30 groups, so a plain loop is fine and stays readable.
tok_entry_idx <- split(rep(seq_along(tok_per_entry), lengths(tok_per_entry)),
                       unlist(tok_per_entry, use.names = FALSE))
group_entries <- function(g) {
  ids <- Reduce(intersect, lapply(g, function(t) tok_entry_idx[[t]]))
  max(length(ids), 1L)
}
# The weakest edge in the group. This is the number that decides whether the
# group survives a threshold change, so it is recorded rather than recomputed:
# {de, dieu, jean} rests entirely on `dieu`+`jean` at 12.3x, and if a future
# export pushes that under NR_COLLOC_MIN_RATIO the triple silently stops
# merging and every Jean de Dieu score re-inflates by about 8.5.
edge_ratio <- setNames(edges$ratio, paste0(edges$a, "\r", edges$b))
group_min_ratio <- function(g) {
  cb <- utils::combn(sort(g), 2L)
  min(edge_ratio[paste0(cb[1L, ], "\r", cb[2L, ])])
}

if (length(groups)) {
  ge <- vapply(groups, group_entries, numeric(1))
  colloc <- data.frame(
    tokens        = vapply(groups, paste, character(1), collapse = " "),
    n_tokens      = lengths(groups),
    n_entries     = ge,
    idf           = round(log(N / ge), 4),
    sum_separate  = round(vapply(groups, function(g)
                            sum(idf_of_token[g]), numeric(1)), 4),
    min_pair_ratio = round(vapply(groups, group_min_ratio, numeric(1)), 1),
    stringsAsFactors = FALSE
  )
  colloc$inflation_removed <- round(colloc$sum_separate - colloc$idf, 4)
  colloc <- colloc[order(-colloc$n_tokens, -colloc$inflation_removed), ,
                   drop = FALSE]
} else {
  colloc <- data.frame(tokens = character(0), n_tokens = integer(0),
                       n_entries = numeric(0), idf = numeric(0),
                       sum_separate = numeric(0), min_pair_ratio = numeric(0),
                       inflation_removed = numeric(0),
                       stringsAsFactors = FALSE)
}

nrlog("    groups after requiring every pair to be collocated: ", nrow(colloc),
      " (", sum(colloc$n_tokens == 2L), " pairs, ",
      sum(colloc$n_tokens >= 3L), " larger)")
if (nrow(colloc)) {
  nrlog("    largest inflation removed:")
  for (i in seq_len(min(6L, nrow(colloc)))) {
    nrlog("      ", formatC(colloc$tokens[i], width = -26),
          " ", colloc$n_entries[i], " entries | idf ", colloc$idf[i],
          " vs ", colloc$sum_separate[i], " summed  (-",
          colloc$inflation_removed[i], ")")
  }
}


# -------------------------  Write  --------------------------------------------

# ONE DESTINATION: person_matching/<stem>/name_rarity/, one per (epi run x ct run).
# A rarity score is evidence behind a review decision, and IDF moves as the
# corpus grows, so the numbers a reviewer actually saw have to stay
# recoverable. Which snapshot is current is recorded in the name_rarity
# section of data/processed/latest.json, and that is what every matching
# script reads (load_name_rarity() -> current_name_rarity_dir() in 0-setup.R).
#
# latest.json is updated LAST: if the process dies partway, latest.json still
# names the previous complete snapshot, so the pipeline never reads a
# half-written table.
stem     <- paste0(epi_stem, "__", if (is.na(ct_stem)) "no-ct" else ct_stem)
snap_dir <- name_rarity_dir(stem)   # data/processed/person_matching/<stem>/name_rarity/
dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)

out_path    <- file.path(snap_dir, "name_rarity.csv")
colloc_path <- file.path(snap_dir, "name_collocations.csv")
nr_write_csv(rarity, out_path)
nr_write_csv(colloc, colloc_path)

meta <- paste0(
  '{\n',
  '  "stem": "', stem, '",\n',
  '  "epi_run": "', epi_stem, '",\n',
  '  "ct_run": ', if (is.na(ct_stem)) 'null' else paste0('"', ct_stem, '"'), ',\n',
  '  "corpus_entries": ', N, ',\n',
  '  "distinct_tokens": ', nrow(rarity), ',\n',
  '  "idf_unseen": ', idf_unseen, ',\n',
  '  "min_token_chars": ', NR_MIN_TOKEN_CHARS, ',\n',
  '  "max_tokens_per_name": ', NR_MAX_TOKENS, ',\n',
  '  "colloc_min_ratio": ', NR_COLLOC_MIN_RATIO, ',\n',
  '  "colloc_min_together": ', NR_COLLOC_MIN_TOGETHER, ',\n',
  '  "colloc_groups": ', nrow(colloc), ',\n',
  '  "built_at": "', format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), '"\n',
  '}\n'
)
# load_name_rarity() reads corpus_entries and idf_unseen from here rather than
# recomputing them from the table, so a consumer's log(N) is the same number
# this run printed — not one reconstructed from rounded columns. Kept in the
# snapshot (not only in latest.json) so an old snapshot still reproduces.
writeLines(meta, file.path(snap_dir, "name_rarity_meta.json"))

# The tuning parameters this run used, recorded beside its output.
#
# The snapshot exists because IDF moves as the corpus grows and a score behind a
# reviewed decision has to reproduce. Since those scores also depend on
# pipeline_configs.json's `matching` block, which is editable and unversioned,
# the table alone is not enough to reproduce them: the same corpus under
# different collocation thresholds gives different numbers. Recording the block
# closes that gap.
# One copy per run, in the run folder; 2-build-person-candidates.R writes the
# same file when it builds the run's candidates.
writeLines(match_params_json(),
           file.path(pm_run_dir(stem), "matching_params.json"))

# Last: point latest.json at the now-complete snapshot.
write_latest("name_rarity", jsonlite::fromJSON(meta, simplifyVector = FALSE))

nrlog("\n  Written (read by every matching script via latest.json):")
nrlog("    ", out_path)
nrlog("    ", colloc_path)
nrlog("  NOTE: both files contain patient NAME FRAGMENTS. ",
      sum(rarity$n_entries == 1), " tokens occur exactly once, so those rows ",
      "are in effect individual names. Handle them like ",
      "dhis2_processed_linelist_names.csv; they must not enter any ctsafe or ",
      "shared export.")
write_log_file(.nr_log, "name_rarity_log.txt")

name_rarity <- rarity
name_collocations <- colloc
name_rarity_complete <- TRUE


# ------------------------- Clean up environment -------------------------------
keep_objects <- append(keep_objects, c("name_rarity", "name_collocations",
                                       "name_rarity_complete"))
rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
