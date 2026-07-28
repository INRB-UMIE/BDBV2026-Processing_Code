# pipelines/0-setup.R
#
# Adapted from BDBV2026-Contact_Tracing_Analysis's pipelines/0-setup.R for this
# repo's code/data split. This repo (BDBV2026-Processing_Code) holds only the
# scripts for steps 1-7 of the epi linelist pipeline (delay-distribution
# fitting, date imputation, aggregation, dashboard plots, residence mapping,
# residence-vs-alert reporting, nowcasting prep). It has no data/ directory
# of its own — all input/output data lives in the sibling
# BDBV2026-Processed_Sensitive_Data repo (data/ + outputs/), located via
# resolve_data_root() below.
#
# Two roots are resolved and kept in scope for the rest of the pipeline:
#   code_root — this repo's root (where pipelines/ lives)
#   data_root — the BDBV2026-Processed_Sensitive_Data repo's root (where
#               data/ and outputs/ live)
# Anywhere the old single-repo scripts referenced `repo_root`, that has been
# split into whichever of the two is actually meant at that call site.

# -------------------------
# Setup Packages (auto-install if missing)
# -------------------------

req_pkgs <- c(
  "readr", "dplyr", "tidyr", "purrr", "stringr",
  "lubridate", "ggplot2", "sf", "readxl", "pammtools", "HDInterval",
  "jsonlite", "binom", "slider", "fitdistrplus", "mgcv",
  "brms", "bayesplot", "loo", "posterior", "svglite", "future", "furrr",
  "rmarkdown", # 8-summary-report.Rmd (knitr comes along as rmarkdown's own dependency)
  "rsvg",     # rasterizing dashboard SVGs for PDF embedding in 8-summary-report.Rmd
  "tinytex"   # LaTeX engine rmarkdown needs to knit 8-summary-report.Rmd to PDF
)

for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

# epidist (delay-distribution fitting, used by 1-fit_delay_distributions.R)
# isn't on CRAN -- it's published on the epinowcast R-universe. cmdstanr (the
# Stan backend epidist fits with) is on the Stan R-universe. Install these
# from their own repos, falling back to CRAN for their regular dependencies.
special_repo_pkgs <- list(
  epidist = "https://epinowcast.r-universe.dev",
  cmdstanr = "https://stan-dev.r-universe.dev"
)

for (p in names(special_repo_pkgs)) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = c(special_repo_pkgs[[p]], "https://cloud.r-project.org"))
  }
}

# primarycensored is pinned to a specific (CRAN) version, not "whatever's
# latest", because 1.4.0 made a breaking change to the Stan side: it added an
# `L` (lower-truncation) parameter to primarycensored_lpmf/_lcdf/_cdf/_pmf,
# changing their call signature. epidist's current release (0.4.0) hasn't
# been updated to emit the new signature, so anything >=1.4.0 fails to
# compile against it with an "Ill-typed arguments... Expected 9 arguments but
# found 8" error. 1.3.0 predates that change.
# See: https://primarycensored.epinowcast.org/news/index.html
# If a newer epidist release adds support for the new signature, this pin
# should be revisited/removed.
#
# Checking the version alone isn't enough to catch a *corrupted* install:
# R's lazy-load database is read function-by-function on first use, so a
# package can report the right version and load fine while one specific
# function's serialized bytecode is truncated/corrupt on disk. That's exactly
# what happened here -- fitting worked, but loo() (which needs a different
# function out of the same package) errored with "lazy-load database ... is
# corrupt". check_primarycensored_ok() also force-touches every exported name
# to catch that before anything downstream does.
PRIMARYCENSORED_PINNED_VERSION <- "1.3.0"

# A corrupted lazy-load database doesn't always throw a hard error -- R's
# decompression step (R_decompress1) can instead emit a *warning* ("internal
# error N in R_decompress1 with libdeflate", often paired with "restarting
# interrupted promise evaluation") and otherwise continue. An error-only
# handler would silently miss that and report "ok", so any warning raised
# while touching the package's exports is treated the same as an error here.
check_primarycensored_ok <- function() {
  tryCatch({
    if (!requireNamespace("primarycensored", quietly = TRUE)) {
      return(FALSE)
    }
    if (as.character(utils::packageVersion("primarycensored")) != PRIMARYCENSORED_PINNED_VERSION) {
      return(FALSE)
    }
    ns <- asNamespace("primarycensored")
    for (nm in getNamespaceExports("primarycensored")) {
      get(nm, envir = ns)
    }
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

if (!check_primarycensored_ok()) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }

  # Make sure nothing has it loaded before removing/overwriting its files.
  if ("primarycensored" %in% loadedNamespaces()) {
    try(unloadNamespace("primarycensored"), silent = TRUE)
  }
  if ("primarycensored" %in% rownames(installed.packages())) {
    try(remove.packages("primarycensored"), silent = TRUE)
  }
  # Belt-and-braces: remove.packages() can leave files behind on a corrupted
  # install (the exact scenario this is guarding against) -- explicitly
  # delete whatever's left on disk before reinstalling.
  leftover_dir <- tryCatch(find.package("primarycensored", quiet = TRUE), error = function(e) character(0))
  if (length(leftover_dir) && dir.exists(leftover_dir)) {
    unlink(leftover_dir, recursive = TRUE, force = TRUE)
  }

  message(
    "Installing primarycensored ", PRIMARYCENSORED_PINNED_VERSION,
    " (pinned -- see comment above this block for why)..."
  )
  remotes::install_version(
    "primarycensored",
    version = PRIMARYCENSORED_PINNED_VERSION,
    repos = "https://cloud.r-project.org"
  )

  if (!check_primarycensored_ok()) {
    warning(
      "primarycensored ", PRIMARYCENSORED_PINNED_VERSION, " was reinstalled but still fails ",
      "the integrity check -- delay distribution fitting (1-fit_delay_distributions.R) may not ",
      "work. Try installing from source explicitly: ",
      "remotes::install_version(\"primarycensored\", version = \"", PRIMARYCENSORED_PINNED_VERSION,
      "\", repos = \"https://cloud.r-project.org\", type = \"source\")",
      call. = FALSE
    )
  }
}

# CmdStan itself (the actual Stan toolchain cmdstanr calls out to) is a heavy,
# separate install -- can take 10-20 minutes the first time. Installed to a
# fixed, known directory (overridable via CMDSTAN_INSTALL_DIR) rather than
# cmdstanr's package default, specifically so CI can cache that exact path
# (actions/cache) across runs instead of reinstalling on every sync.
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  CMDSTAN_INSTALL_DIR <- Sys.getenv("CMDSTAN_INSTALL_DIR", file.path(Sys.getenv("HOME"), ".cmdstan"))

  cmdstan_found <- !is.null(tryCatch(cmdstanr::cmdstan_version(), error = function(e) NULL))

  # A cache restore (or a previous run) may have left a CmdStan install on
  # disk at CMDSTAN_INSTALL_DIR without cmdstanr's *current* R session knowing
  # about it -- point cmdstanr at the newest cmdstan-* subdirectory there
  # before concluding it's actually missing.
  if (!cmdstan_found && dir.exists(CMDSTAN_INSTALL_DIR)) {
    existing <- list.dirs(CMDSTAN_INSTALL_DIR, recursive = FALSE)
    existing <- existing[grepl("^cmdstan-", basename(existing))]
    if (length(existing)) {
      cmdstanr::set_cmdstan_path(sort(existing)[length(existing)])
      cmdstan_found <- !is.null(tryCatch(cmdstanr::cmdstan_version(), error = function(e) NULL))
    }
  }

  if (!cmdstan_found) {
    message("CmdStan not found -- installing to ", CMDSTAN_INSTALL_DIR, " (this can take 10-20 minutes)...")
    dir.create(CMDSTAN_INSTALL_DIR, recursive = TRUE, showWarnings = FALSE)
    cmdstanr::install_cmdstan(dir = CMDSTAN_INSTALL_DIR)
  }
}

# NOTE: installing the actual TinyTeX/LaTeX distribution (as opposed to just
# the tinytex R package above) is deliberately NOT done here. 0-setup.R is
# sourced by every pipeline step, including steps 1-4 which are the only ones
# run automatically in CI (run-processing-code-pipeline.yml) and never knit
# any PDF -- installing it here meant every one of those 4 Rscript
# invocations retried a several-second network install on every single run,
# only to fail every time in that environment and log a warning, for a
# feature none of them use. Only 8-summary-report.Rmd (manual-only, not
# wired into CI) actually needs the LaTeX distribution, so that on-demand
# install now lives in its own setup chunk instead. See that file's setup
# chunk for the actual install_tinytex() call.

suppressWarnings(suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(sf)
  library(readxl)
  library(pammtools)
  library(jsonlite)
  library(binom)
  library(slider)
  library(HDInterval)
  library(fitdistrplus)
  library(brms)
  library(bayesplot)
  library(loo)
  library(future)
  library(furrr)
  library(epidist)
  library(primarycensored)
  library(cmdstanr)
}))

# -----------------------------
# Available cores
# -----------------------------
# Used to size chains/cores for Stan fits and loo() calls (see
# 1-fit_delay_distributions.R) so they adapt to wherever this actually runs,
# rather than a hardcoded number that oversubscribes small environments.
# Standard GitHub Actions runners report 2 here; a local machine will report
# however many (logical) cores it actually has. Overridable via
# N_CORES_OVERRIDE for testing/tuning without editing this file.
N_CORES_AVAILABLE <- tryCatch({
  override <- Sys.getenv("N_CORES_OVERRIDE", "")
  if (nzchar(override)) {
    max(1L, as.integer(override))
  } else {
    n <- parallel::detectCores(logical = TRUE)
    if (is.na(n) || n < 1) 1L else as.integer(n)
  }
}, error = function(e) 1L)


# -----------------------------
# Helper functions
# -----------------------------
# Deterministic per-fit seed so a fit is bit-identical whether run serially or
# in a parallel worker (cmdstan is deterministic given seed + version + data).
# Base-R only (no digest dependency); polynomial rolling hash in double
# precision, reduced into the positive 31-bit range.
epidist_fit_seed <- function(delay_name, family_name) {
  key <- paste(delay_name, family_name, sep = "|")
  h <- 0
  for (ch in utf8ToInt(key)) h <- (h * 31 + ch) %% 2147483647
  if (h == 0) h <- 1
  as.integer(h)
}

# Compute loo() for an epidist/brms fit by evaluating the pointwise
# log-likelihood in parallel over draws, then running PSIS on the assembled
# matrix. Uses ALL draws, so the result equals brms::loo(fit) up to floating
# point -- the sole purpose is speed (epidist's custom-family log_lik is
# recomputed in R per draw x observation, single-threaded inside brms::loo).
# Forking is safe here: sampling is already done, this is pure R.
parallel_loo <- function(fit, cores = N_CORES_AVAILABLE) {
  cores   <- as.integer(cores)
  if (is.na(cores) || cores < 1L) cores <- 1L
  # parallel::mclapply forks, which is unavailable on Windows -- fall back to
  # serial there so the helper is portable (the pipeline itself runs on Linux).
  if (.Platform$OS.type == "windows") cores <- 1L
  n_draws <- brms::ndraws(fit)
  cores   <- min(cores, n_draws)   # never spawn more workers than there are draws
  n_chains <- brms::nchains(fit)
  if (n_draws %% n_chains != 0) {
    stop("parallel_loo: draws are not evenly split across chains (", n_draws,
         " draws / ", n_chains, " chains)", call. = FALSE)
  }
  draws_per_chain <- n_draws %/% n_chains

  # Contiguous chunks preserve the original (chain-major) draw order so the
  # reassembled matrix lines up with chain_id below.
  chunk_id <- pmin(ceiling(seq_len(n_draws) / (n_draws / cores)), cores)
  chunks <- split(seq_len(n_draws), chunk_id)

  ll_parts <- parallel::mclapply(chunks, function(ids) {
    brms::log_lik(fit, draw_ids = ids)
  }, mc.cores = cores)

  bad <- vapply(ll_parts, function(x) inherits(x, "try-error") || is.null(x), logical(1))
  if (any(bad)) stop("parallel_loo: log_lik failed in ", sum(bad), " chunk(s)")

  ll <- do.call(rbind, ll_parts)               # n_draws x n_obs, original order
  chain_id <- rep(seq_len(n_chains), each = draws_per_chain)
  r_eff <- loo::relative_eff(exp(ll), chain_id = chain_id, cores = cores)
  loo::loo(ll, r_eff = r_eff, cores = cores)
}

get_script_dir <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArg <- grep("^--file=", cmdArgs, value = TRUE)
  if (length(fileArg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", fileArg[1]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  getwd()
}

#' Locate this repo's own root (BDBV2026-Processing_Code) — the directory
#' containing pipelines/. Same script-location trick as the old
#' resolve_repo_root(), but anchored on pipelines/ instead of data/, since
#' this repo has no data/ directory of its own.
resolve_code_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
  from_script <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(from_script, "pipelines"))) {
    return(from_script)
  }
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(cwd, "pipelines"))) {
    return(cwd)
  }
  stop("Could not locate BDBV2026-Processing_Code repo root (expected a pipelines/ directory).", call. = FALSE)
}

#' Locate the sibling BDBV2026-Processed_Sensitive_Data repo, which holds all
#' input/output data (data/, outputs/) for this pipeline. Resolution order:
#'   1. BDBV_DATA_ROOT environment variable, if set — an explicit override.
#'      Use this in CI (where checkouts aren't literal sibling folders) or if
#'      your local folder names differ from the defaults below.
#'   2. A folder next to this repo (i.e. code_root/..) named either
#'      "BDBV2026-Processed_Sensitive_Data" (the usual local folder name) or
#'      "BDBV2026-Contact_Tracing_Analysis" (that repo's GitHub name), that
#'      itself contains a data/ directory.
#' This is the "sister folders on a machine" convention: clone
#' BDBV2026-Processing_Code and BDBV2026-Processed_Sensitive_Data side by side
#' and everything resolves automatically with no configuration.
resolve_data_root <- function(code_root) {
  env_override <- Sys.getenv("BDBV_DATA_ROOT", unset = "")
  if (nzchar(env_override)) {
    if (!dir.exists(file.path(env_override, "data"))) {
      stop("BDBV_DATA_ROOT is set to '", env_override,
           "' but no data/ directory was found there.", call. = FALSE)
    }
    return(normalizePath(env_override, winslash = "/", mustWork = TRUE))
  }

  parent_dir <- normalizePath(file.path(code_root, ".."), winslash = "/", mustWork = FALSE)
  known_names <- c("BDBV2026-Processed_Sensitive_Data", "BDBV2026-Contact_Tracing_Analysis")
  for (nm in known_names) {
    candidate <- file.path(parent_dir, nm)
    if (dir.exists(file.path(candidate, "data"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Could not locate the BDBV2026-Processed_Sensitive_Data repo. Expected a ",
    "folder next to ", code_root, " named one of: ", paste(known_names, collapse = ", "),
    " (containing a data/ directory) — clone the two repos as sibling folders — ",
    "or set the BDBV_DATA_ROOT environment variable to point at it explicitly ",
    "(e.g. in CI, where checkouts live in subdirectories rather than true siblings).",
    call. = FALSE
  )
}

code_root <- resolve_code_root()
data_root <- resolve_data_root(code_root)

#' Parse a `data/YYYY-MM-DD` folder name into a Date. Returns NA for anything
#' that isn't a plain ISO date directory (e.g. the data repo's other top-level dirs).
parse_synced_date_dir <- function(dirname_) {
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", dirname_)) {
    return(as.Date(NA))
  }
  tryCatch(as.Date(dirname_), error = function(e) as.Date(NA))
}

#' Return the full path to the latest synced `data/YYYY-MM-DD` folder (under
#' data_root) that contains a `dhis2_processed_linelist_ctsafe.csv` file. This
#' replaces Secure's `resolve_epi_latest_dir()` (which reads a `latest.json`
#' written by step 1) — this repo has no step 1 and no latest.json, so we
#' derive "latest" directly from the synced date folders each time it's called.
#'
#' `processed_dir` is accepted (and ignored beyond existence) for compatibility
#' with call sites ported from Secure that pass an explicit override.
resolve_epi_latest_dir <- function(processed_dir = file.path(data_root, "data")) {
  if (exists("epi_latest_dir", envir = .GlobalEnv) &&
      nzchar(get("epi_latest_dir", envir = .GlobalEnv))) {
    return(get("epi_latest_dir", envir = .GlobalEnv))
  }
  data_dir <- file.path(data_root, "data")
  date_dirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
  dates <- vapply(date_dirs, parse_synced_date_dir, as.Date(NA))
  has_ctsafe <- vapply(date_dirs, function(d) {
    file.exists(file.path(data_dir, d, "dhis2_processed_linelist_ctsafe.csv"))
  }, logical(1))
  valid <- !is.na(dates) & has_ctsafe
  if (!any(valid)) {
    stop(
      "No synced date folder with dhis2_processed_linelist_ctsafe.csv found under ",
      data_dir,
      ". Check that BDBV2026-Linelist_Processing's ctsafe sync has run.",
      call. = FALSE
    )
  }
  latest <- date_dirs[valid][which.max(dates[valid])]
  file.path(data_dir, latest)
}

# Pins the whole pipeline to a specific synced date folder instead of always
# using the latest one -- e.g. to keep working against yesterday's sync while
# a newer one is still being checked, or to reproduce a past run. Set
# EPI_PIPELINE_DATE (YYYY-MM-DD) before running any step; every step sources
# this file first, so the override applies uniformly across all of them, even
# though each step runs as its own Rscript process. resolve_epi_latest_dir()
# already had an `epi_latest_dir` global-override hook (for interactive use,
# e.g. sourcing steps together in one session) -- this just wires that up to
# an env var so it survives across process boundaries too.
epi_pipeline_date_override <- Sys.getenv("EPI_PIPELINE_DATE", "")
if (nzchar(epi_pipeline_date_override)) {
  epi_latest_dir <- file.path(data_root, "data", epi_pipeline_date_override)
  if (!file.exists(file.path(epi_latest_dir, "dhis2_processed_linelist_ctsafe.csv"))) {
    stop(
      "EPI_PIPELINE_DATE is set to '", epi_pipeline_date_override,
      "' but ", epi_latest_dir, " has no dhis2_processed_linelist_ctsafe.csv.",
      call. = FALSE
    )
  }
  message("EPI_PIPELINE_DATE override active -- using ", epi_latest_dir, " instead of the latest synced folder.")
}

#' Build a linelist_meta-shaped list (date, coded, label, basename) from a
#' resolved `data/YYYY-MM-DD` folder, for scripts (e.g. step 6) that need a
#' human-readable run label but have no raw workbook to parse a date from
#' (Secure's `parse_dhis2_linelist_date()` parsed the raw .xlsx filename;
#' this repo only ever sees the synced ctsafe CSVs).
parse_dhis2_linelist_date <- function(epi_dir_or_path) {
  stem <- basename(epi_dir_or_path)
  d <- parse_synced_date_dir(stem)
  if (is.na(d)) {
    # Fall back to file.mtime if handed something unexpected.
    d <- as.Date(file.mtime(epi_dir_or_path))
  }
  list(
    date = d,
    coded = format(d, "%d%m%Y"),
    label = format(d, "%d %B %Y"),
    basename = stem
  )
}

#' Parse a linelist date/datetime column into Date. Handles the DHIS2 export's
#' mixed formats (ISO, dd/mm/yyyy, mm/dd/yyyy, with or without a time
#' component), and passes through columns already typed as Date/POSIXt.
#' Returns NA for anything unparseable rather than erroring.
parse_linelist_date <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  raw <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(raw))
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") {
      next
    }
    val <- sub("[T ].*$", "", val)
    parsed <- suppressWarnings(tryCatch(
      as.Date(val, tryFormats = c(
        "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%OS"
      )),
      error = function(e) NA
    ))
    if (!is.na(parsed)) {
      out[i] <- parsed
    }
  }
  out
}

#' Look up a posterior_summary() row by a list of candidate regex patterns
#' (tried in order), returning its "Estimate" column. Used for pulling a
#' brms/epidist fit's coefficients by name without hardcoding a single naming
#' convention, since the exact row name for a given parameter can vary by
#' brms/epidist version (e.g. "b_Intercept" vs "b_mu_Intercept"). Returns
#' NA_real_ (rather than erroring) if none of the patterns match, so callers
#' can decide how to handle a missing parameter.
get_param_estimate <- function(posterior_summary_mat, patterns) {
  nm <- rownames(posterior_summary_mat)
  for (p in patterns) {
    hit <- grep(p, nm, value = TRUE)
    if (length(hit)) {
      return(unname(posterior_summary_mat[hit[1], "Estimate"]))
    }
  }
  NA_real_
}

#' dpar name epidist/brms uses for each fitted family's dispersion/shape
#' parameter -- shared by anything that needs to look it up (delay
#' distribution summaries in 1-fit_delay_distributions.R, sampling/quantiles
#' in 2-date_imputation.R).
epidist_family_dpar <- list(lognormal = "sigma", weibull = "shape", gamma = "shape")

#' Extract a fitted epidist/brms delay-distribution model's mu and
#' dispersion/shape parameters on their *natural* (response) scale.
#'
#' epidist gives every family's dispersion/shape dpar its own `~1` regression
#' formula (confirmed in each fit's printed "Formula:"/"Links:" lines -- e.g.
#' "shape ~ 1" under "shape = log", or "sigma ~ 1" under "sigma = log", for
#' all three families, including lognormal). Because it's a full regression
#' rather than a plain flat dpar, brms doesn't create an already-back-
#' transformed convenience column for it -- posterior_summary() only has the
#' raw b_<param>_Intercept value, on the *link* scale. Missing this exp() was
#' the actual bug behind "massive" Weibull SDs early on: a raw shape_Intercept
#' of 0.05 (log scale) was being used directly as shape=0.05 (an almost-
#' degenerate, mode-at-zero Weibull) instead of the true shape=exp(0.05)=1.05
#' (an unremarkable shape near 1).
#'
#' mu_link: lognormal's mu (meanlog) uses an identity link; weibull/gamma's
#' mu (already the response mean, per brms's parameterization) uses a log
#' link, so it needs exponentiating back to the natural scale too.
#'
#' Returns list(mu = <natural scale>, disp = <natural scale: sdlog/shape>),
#' or NULL (with a warning) if the expected posterior variables can't be
#' found -- callers should treat that as "can't use this fit" rather than
#' erroring the whole run.
epidist_extract_family_params <- function(fit, family_name) {
  disp_param <- epidist_family_dpar[[family_name]]
  if (is.null(disp_param)) {
    stop("Unknown family: ", family_name, call. = FALSE)
  }

  ps <- brms::posterior_summary(fit)
  mu_link_scale <- get_param_estimate(ps, c("^b_Intercept$", "^b_mu_Intercept$", "^Intercept$"))
  disp_link_scale <- get_param_estimate(ps, c(paste0("^", disp_param, "$"), paste0("^b_", disp_param, "_Intercept$")))

  if (is.na(mu_link_scale) || is.na(disp_link_scale)) {
    warning(
      "Could not locate expected posterior parameters for family ", family_name,
      call. = FALSE
    )
    return(NULL)
  }

  mu <- if (family_name == "lognormal") mu_link_scale else exp(mu_link_scale)
  disp <- exp(disp_link_scale)
  list(mu = mu, disp = disp)
}

#' Convert a family's natural-scale mu/disp (from epidist_extract_family_params())
#' into the parameter names R's own r<family>()/q<family>() functions expect.
#' Weibull's scale is computed via lgamma() (log-space), not raw gamma(), for
#' the same numerical-stability reason documented in
#' 1-fit_delay_distributions.R: 1+1/shape can be large enough that gamma()
#' itself loses precision or overflows when shape is small.
epidist_family_rq_params <- function(family_name, mu, disp) {
  if (family_name == "lognormal") {
    return(list(meanlog = mu, sdlog = disp))
  }
  if (family_name == "weibull") {
    shape <- disp
    log_scale <- log(mu) - lgamma(1 + 1 / shape)
    return(list(shape = shape, scale = exp(log_scale)))
  }
  if (family_name == "gamma") {
    shape <- disp
    return(list(shape = shape, rate = shape / mu))
  }
  stop("Unknown family: ", family_name, call. = FALSE)
}

#' Load a `key,value` lookup CSV from data_root/assets/maps/<name>.csv into a
#' named character vector (value, named by key) -- e.g.
#' load_map_csv("lab_name_map")[["INRBK"]]. Mirrors BDBV2026-Secure_Raw_Data's
#' own load_map_csv() helper for consistency across the BDBV2026 repos,
#' adapted to this repo's data_root/assets/maps/ location. encoding defaults
#' to UTF-8 (that folder's usual convention); override for outlier files
#' saved in a different encoding (e.g. lab_name_map.csv is Mac OS Roman).
#'
#' Reads raw bytes (rather than handing `fileEncoding` to read.csv directly)
#' and strips a leading UTF-8 BOM (EF BB BF) before decoding, because
#' read.csv's automatic BOM handling only kicks in for UTF-8-family
#' encodings. lab_name_map.csv turned out to have a UTF-8 BOM tacked onto an
#' otherwise Mac-Roman-encoded file (likely picked up from a re-save in some
#' editor) -- left in place, those 3 bytes get decoded as Mac Roman too and
#' fuse onto the front of the "key" header, so `names(df)` never actually
#' contains "key" and the check below fails even though the file looks fine
#' at a glance.
load_map_csv <- function(name, maps_dir = file.path(data_root, "assets", "maps"), encoding = "UTF-8") {
  path <- file.path(maps_dir, paste0(name, ".csv"))
  if (!file.exists(path)) {
    stop("Map CSV not found: ", path, call. = FALSE)
  }
  raw <- readBin(path, what = "raw", n = file.size(path))
  utf8_bom <- as.raw(c(0xef, 0xbb, 0xbf))
  if (length(raw) >= 3 && identical(raw[1:3], utf8_bom)) {
    raw <- raw[-(1:3)]
  }
  text <- iconv(rawToChar(raw), from = encoding, to = "UTF-8")
  df <- read.csv(
    text = text, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = character(0)
  )
  if (!all(c("key", "value") %in% names(df))) {
    stop("Map CSV missing key/value columns: ", path, call. = FALSE)
  }
  stats::setNames(df$value, df$key)
}

#' Read a synced linelist CSV the "proper" way, for use by any pipeline step:
#'   - Normalizes blank/NA tokens ("", "NA", "N/A") consistently as NA across
#'     every column, instead of each script handling blanks its own way
#'   - Converts the configured date columns (default:
#'     pipeline_configs$epi_linelist$qc$date_columns) to actual Date vectors
#'     via parse_linelist_date(), instead of leaving them as strings for each
#'     downstream script to reparse on its own
load_linelist_properly <- function(path, date_columns = NULL) {
  if (!file.exists(path)) {
    stop("Linelist not found: ", path, call. = FALSE)
  }
  if (is.null(date_columns)) {
    date_columns <- pipeline_configs$epi_linelist$qc$date_columns
  }
  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA", "N/A")
  )
  for (col in intersect(date_columns, names(df))) {
    df[[col]] <- parse_linelist_date(df[[col]])
  }
  df
}


# -----------------------------
# Load Config
# -----------------------------
config_path <- file.path(code_root, "pipelines", "pipeline_configs.json")
if (!file.exists(config_path)) {
  stop("Pipeline config not found: ", config_path, call. = FALSE)
}

pipeline_configs <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)

# -----------------------------
# Clean Environment
# -----------------------------
keep_objects <- c("keep_objects", "setup_complete", "get_script_dir",
                  "code_root", "data_root", "pipeline_configs",
                  "resolve_code_root", "resolve_data_root",
                  "resolve_epi_latest_dir", "epi_latest_dir", "parse_dhis2_linelist_date",
                  "parse_synced_date_dir", "dhis2_force_stems",
                  "parse_linelist_date", "load_linelist_properly", "load_map_csv",
                  "N_CORES_AVAILABLE", "get_param_estimate",
                  "epidist_family_dpar", "epidist_extract_family_params",
                  "epidist_family_rq_params",
                  "epidist_fit_seed", "parallel_loo")

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)

# -----------------------------
# Set 'setup' flag
# -----------------------------
setup_complete <- TRUE
