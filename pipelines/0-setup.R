# pipelines/0-setup.R
#
# Adapted from BDBV2026-Contact_Tracing_Analysis's pipelines/0-setup.R for this
# repo's code/data split. This repo (BDBV2026-Processing_Code) holds only the
# scripts for steps 2-6 of the epi linelist pipeline (aggregation, nowcasting,
# dashboard plots, residence mapping, residence-vs-alert reporting). It has no
# data/ directory of its own — all input/output data lives in the sibling
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
  "jsonlite", "binom", "slider", "fitdistrplus", "mgcv"
)

for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

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
}))


# -----------------------------
# Helper functions
# -----------------------------
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
                  "resolve_epi_latest_dir", "parse_dhis2_linelist_date",
                  "parse_synced_date_dir", "dhis2_force_stems",
                  "parse_linelist_date", "load_linelist_properly")

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)

# -----------------------------
# Set 'setup' flag
# -----------------------------
setup_complete <- TRUE
