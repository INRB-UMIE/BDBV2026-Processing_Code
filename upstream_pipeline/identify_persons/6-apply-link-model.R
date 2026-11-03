#!/usr/bin/env Rscript
# 6-apply-link-model.R
#
# Runs the link classifier (BDBV2026-Link_Classification) on tonight's scoring
# table and writes its decisions for the ledger to apply.
#
#   in:  data/processed/person_matching/training/link_scoring_latest.csv
#        (5-build-link-training.R, earlier in this same run)
#   out: data/processed/person_matching/model_decisions/
#          model_decisions_<YYYY-MM-DD>.csv   one per day (same-day reruns replace)
#          model_decisions_latest.csv          what the ledger reads
#
# The model and its code live in their own repo, installed on the instance at
# <repo root>/link_classification/ (not part of this repo; git-ignored here).
# This step only calls its scripts/predict.R in a separate R process, so the
# pipeline needs to know nothing about the model beyond "run this, read that".
#
# Controlled by pipeline_configs.json, matching.model:
#   enabled  0 = skipped (and the ledger withdraws earlier model decisions)
#   version  which saved model to use, e.g. "model_v1"
#
# After this step the runner re-runs the ledger and registry, so tonight's
# decisions take effect tonight (see run_*_bg() in shiny/global.R).
#
# Fails loudly (stop) on anything unexpected; the runner catches it, logs it,
# and carries on WITHOUT model decisions -- the safe direction.

# -------------------------  Check if Initialised  -----------------------------

if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}

.lm_log <- character(0)
lmlog <- function(...) {
  txt <- paste0(...)
  message(txt)
  .lm_log <<- c(.lm_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
}

link_model_applied <- FALSE
lmlog("Link classifier")

tryCatch({
if (!isTRUE(as.logical(match_param("model.enabled")))) {
  lmlog("  matching.model.enabled is 0 -- skipped")
} else {
  local({
    version <- as.character(pipeline_configs$matching$model$version)
    if (length(version) != 1L || !grepl("^model_v[0-9]+$", version))
      stop("matching.model.version must be a name like \"model_v1\"", call. = FALSE)

    lc_dir  <- file.path(repo_root, "link_classification")
    script  <- file.path(lc_dir, "scripts", "predict.R")
    model   <- file.path(lc_dir, "models", version, "model.rds")
    input   <- file.path(person_matching_dir(), "training", "link_scoring_latest.csv")
    out_dir <- file.path(person_matching_dir(), "model_decisions")
    stem    <- read_latest("person_candidates")$stem

    for (f in c(script, model, input))
      if (!file.exists(f)) stop("not found: ", f, call. = FALSE)
    if (is.null(stem) || !nzchar(stem)) stop("no current candidate run", call. = FALSE)

    # The scoring table must describe THIS run's links, or the decisions
    # would be made on stale evidence (and the ledger would refuse them).
    runs <- unique(utils::read.csv(input, colClasses = "character",
                                   na.strings = "")$features_run)
    runs <- runs[!is.na(runs)]
    if (length(runs) && !all(runs == stem))
      stop("link_scoring_latest.csv is from run ", paste(runs, collapse = ", "),
           ", not ", stem, call. = FALSE)

    day <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
    dated <- file.path(out_dir, paste0("model_decisions_", day, ".csv"))
    lmlog("  model ", version, " on ", input)
    # This R's own Rscript, rather than whatever is first on PATH.
    res <- system2(file.path(R.home("bin"), "Rscript"), c("--no-init-file", shQuote(script),
                                "--model", version, "--input", shQuote(input),
                                "--output", shQuote(dated), "--run", shQuote(stem)),
                   stdout = TRUE, stderr = TRUE)
    for (l in res) lmlog("    ", l)
    status <- attr(res, "status")
    if (!is.null(status) && status != 0L)
      stop("predict.R failed (exit ", status, ")", call. = FALSE)
    if (!file.exists(dated)) stop("predict.R wrote nothing", call. = FALSE)

    latest <- file.path(out_dir, "model_decisions_latest.csv")
    file.copy(dated, paste0(latest, ".tmp"), overwrite = TRUE)
    file.rename(paste0(latest, ".tmp"), latest)
    lmlog("  -> ", latest)
  })
  link_model_applied <- TRUE
}
}, finally = write_log_file(.lm_log, "link_model_log.txt"))   # log kept on failure too
