# scripts/R/0-analysis_setup.R

# -------------------------
# Setup Packages (auto-install if missing)
# -------------------------

req_pkgs <- c(
  "readr", "dplyr", "tidyr", "stringr",
  "lubridate", "ggplot2", "sf", "readxl", "pammtools", "HDInterval",
  "googledrive", "gargle", "jsonlite", "fuzzyjoin", "stringdist", "binom", "slider",
  "fitdistrplus", "kableExtra",
  # rmarkdown pulls in knitr, and is enough to knit the report to HTML or to
  # attempt a PDF. `tinytex` is deliberately NOT here: the PDF is produced in
  # CI inside a pre-baked TinyTeX image, and installing a LaTeX distribution
  # on every analyst's machine bought nothing but a warning on every run.
  "rmarkdown",
  # visNetwork drives the person-cluster review in the Shiny app: node tiles,
  # edge thickness by match tier, click-to-break. Needed by the app only, not
  # by any pipeline script -- but listed here so a fresh clone installs it with
  # everything else rather than failing when someone opens that tab.
  "visNetwork"
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
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(sf)
  library(readxl)
  library(pammtools)
  library(googledrive)
  library(gargle)
  library(jsonlite)
  library(fuzzyjoin)
  library(stringdist)
  library(binom)
  library(slider)
  library(HDInterval)
  library(fitdistrplus)
  library(kableExtra)
}))

# -------------------------
# No LaTeX install here, deliberately
# -------------------------
# This used to call tinytex::install_tinytex() when is_tinytex() was FALSE.
# That check ran on EVERY setup call, so on any machine without TinyTeX it
# attempted an install (and warned) on every single pipeline run -- noise on
# every step of every run, for a PDF that is now produced elsewhere.
#
# The PDF report is knitted in CI, inside the pre-baked R + TinyTeX image built
# by .github/workflows/build-report-image.yml and used by
# sync-ctsafe-to-contact-tracing-analysis.yml. That image is where the LaTeX
# toolchain belongs; a local install is not a prerequisite for anything this
# repo does day to day.
#
# The local report step still exists and still tries to knit, but a failure
# there has never failed a run -- the data is already written by then. Anyone
# who does want local PDFs can run tinytex::install_tinytex() once, or use
# docker/report/.


# -----------------------------
# Helper functions 
# -----------------------------
get_script_dir <- function() { #find the folder this script lives in
  # Works when run via Rscript --file=script.R
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArg <- grep("^--file=", cmdArgs, value = TRUE)
  if (length(fileArg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", fileArg[1]))))
  }
  # Works when sourced from RStudio (sometimes)
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  # Fallback
  getwd()
}

data_path <- function(...) file.path("data", "raw", ...)

resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
  from_script <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(from_script, "data"))) {
    return(from_script)
  }
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(cwd, "data"))) {
    return(cwd)
  }
  stop("Could not locate repo root (expected data/ directory)", call. = FALSE)
}

repo_root <- resolve_repo_root()

# -----------------------------
# latest.json: one file for every step
# -----------------------------
# data/processed/latest.json records which run of each step is current, one
# section per step:
#   epi_linelist       folder, raw_file, linelist_date, processed_at
#   contact_tracing    folder, raw_file, ct_date, processed_at
#   name_rarity        stem, epi_run, ct_run, corpus statistics, built_at
#   person_candidates  stem, epi_run, ct_run, counts, built_at
# It replaced four separate latest.json files, one per output folder. Each
# step rewrites ONLY its own section: write_latest() re-reads the file just
# before writing and writes via a temporary file + rename, so a crash cannot
# leave it half-written. shiny/app.R keeps its own copy of read_latest()
# (app_read_latest()), because the app does not source this file at startup.
latest_json_path <- function() {
  file.path(repo_root, "data", "processed", "latest.json")
}

#' One step's section of latest.json, as a list, or NULL if the file or the
#' section does not exist (or the file cannot be read).
read_latest <- function(section, path = latest_json_path()) {
  if (!file.exists(path)) return(NULL)
  all <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.null(all) || is.null(all[[section]])) return(NULL)
  all[[section]]
}

#' Replace one step's section of latest.json, leaving the others as they are.
write_latest <- function(section, info, path = latest_json_path()) {
  all <- if (file.exists(path)) {
    tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
             error = function(e) {
               stop("latest.json exists but cannot be read (", conditionMessage(e),
                    "); not overwriting it: ", path, call. = FALSE)
             })
  } else list()
  all[[section]] <- info
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(all, tmp, pretty = TRUE, auto_unbox = TRUE, null = "null")
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not update ", path, call. = FALSE)
  }
  invisible(path)
}

# -----------------------------
# logs/: one folder per pipeline run
# -----------------------------
# Every log file the pipeline writes goes to logs/<YYYY-MM-DD_HHMMSS>/ at the
# repo root, rather than beside the outputs. One folder per R session: a full
# pipeline run (one background process in the app) puts all its steps' logs
# in one folder, and running a single step creates a folder of its own. The
# folder is created on first use, so a session that writes no log leaves none.
# In an interactive session that runs several steps, call new_log_run() to
# start a fresh folder.
#
# The name is held in .log_run_dir in the global environment. The leading dot
# keeps it out of ls(), so every script's end-of-script cleanup
# (rm(setdiff(ls(), keep_objects))) leaves it alone.
#
# Logs from before this change were moved to logs/legacy/. find_log() looks
# there last, so the newest log for a given name always wins.
logs_root <- function() file.path(repo_root, "logs")

log_run_dir <- function() {
  d <- if (exists(".log_run_dir", envir = .GlobalEnv))
    get(".log_run_dir", envir = .GlobalEnv) else NULL
  if (is.null(d) || !nzchar(d)) {
    stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    d <- file.path(logs_root(), stamp)
    # Two sessions started in the same second must not share a folder.
    k <- 1L
    while (dir.exists(d)) { k <- k + 1L; d <- file.path(logs_root(), paste0(stamp, "_", k)) }
    assign(".log_run_dir", d, envir = .GlobalEnv)
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

new_log_run <- function() {
  assign(".log_run_dir", NULL, envir = .GlobalEnv)
  invisible(log_run_dir())
}

#' Write a log into this run's logs/ folder. `name` is the file name, e.g.
#' "LINELIST_02102026_processing_log.txt". Returns the path.
write_log_file <- function(lines, name) {
  path <- file.path(log_run_dir(), name)
  writeLines(lines, path)
  invisible(path)
}

#' The newest log called `name` across all runs (timestamped folders, newest
#' first, then logs/legacy/), or NA if there is none.
find_log <- function(name, root = logs_root()) {
  if (!dir.exists(root)) return(NA_character_)
  runs <- sort(list.dirs(root, recursive = FALSE, full.names = FALSE),
               decreasing = TRUE)
  runs <- c(runs[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}", runs)], "legacy")
  hits <- file.path(root, runs, name)
  hits <- hits[file.exists(hits)]
  if (length(hits)) hits[[1]] else NA_character_
}

#' The identity of a link: its two node IDs in a FIXED order, joined by `sep`.
#'
#' The order must not depend on the computer. pmin()/pmax() and `<` compare
#' strings by the running locale's collation, which differs between a Mac and
#' a Linux server: "RDC-AAA-BBB-26+201234-MVE" and "RDC-AAA-BBB-26-2021234-MVE"
#' came out in opposite orders, so the same link would get two different IDs
#' and the ledger would treat one as new and lose its decision. order(method =
#' "radix") always sorts in the C locale -- plain byte order, the same on
#' every machine -- so it is used to rank the strings instead.
node_pair_key <- function(a, b, sep = "|") {
  a <- as.character(a); b <- as.character(b)
  u <- unique(c(a, b))
  rk <- integer(length(u))
  rk[order(u, method = "radix")] <- seq_along(u)
  ra <- rk[match(a, u)]; rb <- rk[match(b, u)]
  first <- ifelse(ra <= rb, a, b)
  second <- ifelse(ra <= rb, b, a)
  paste0(first, sep, second)
}

#' Return the full path to the latest processed epi linelist subfolder.
#' If epi_latest_dir is already set in the global env (i.e. script 1 was just
#' run in this session), that value is used directly. Otherwise the
#' epi_linelist section of latest.json is read.
resolve_epi_latest_dir <- function(
    processed_dir = file.path(repo_root, "data", "processed", "dhis2_linelist_processed")) {
  if (exists("epi_latest_dir", envir = .GlobalEnv) &&
      nzchar(get("epi_latest_dir", envir = .GlobalEnv))) {
    return(get("epi_latest_dir", envir = .GlobalEnv))
  }
  info <- read_latest("epi_linelist")
  if (is.null(info) || is.null(info$folder)) {
    stop("No epi_linelist entry in ", latest_json_path(),
         ".\nRun pipelines/epi_linelist_pipeline/1-colname-standard.R first.",
         call. = FALSE)
  }
  dir <- file.path(processed_dir, info$folder)
  if (!dir.exists(dir)) stop("Folder referenced in latest.json does not exist: ", dir, call. = FALSE)
  dir
}


#' Return the full path to the latest processed contact tracing subfolder.
resolve_ct_latest_dir <- function(
    processed_dir = file.path(repo_root, "data", "processed", "contact_tracing_processed")) {
  if (exists("ct_latest_dir", envir = .GlobalEnv) &&
      nzchar(get("ct_latest_dir", envir = .GlobalEnv))) {
    return(get("ct_latest_dir", envir = .GlobalEnv))
  }
  info <- read_latest("contact_tracing")
  if (is.null(info) || is.null(info$folder)) {
    stop("No contact_tracing entry in ", latest_json_path(),
         ".\nRun pipelines/contact_tracing_pipeline/1-colname-standard.R first.",
         call. = FALSE)
  }
  dir  <- file.path(processed_dir, info$folder)
  if (!dir.exists(dir)) stop("Folder referenced in latest.json does not exist: ", dir, call. = FALSE)
  dir
}


#' The contact tracing data every MATCHING step reads: name rarity, CT dedup,
#' person candidates and the person registry.
#'
#' Chosen by contact_tracing.data.matching_input in pipeline_configs.json:
#'   "combined"  every export, from 2b-combine-exports.R (the all-time data)
#'   "latest"    the latest export only -- since 27 Aug that is ~4 weeks
#'
#' ONE RESOLVER, so the steps cannot disagree. Person candidates reads the CT
#' dedup's output from `dir`; if dedup and candidates resolved their input
#' separately, the within-CT links could describe a different set of contacts
#' from the ones being matched.
#'
#' Returns list(mode, dir, stem, names_csv). `stem` names the run:
#' CT-COMBINED_<date of the latest export> or the export's own folder name.
#'
#' A missing combined dataset is an ERROR, not a fall-back to latest: falling
#' back would quietly shrink matching to four weeks, the very thing the
#' combined dataset exists to prevent.
resolve_ct_matching_input <- function(
    processed_dir = file.path(repo_root, "data", "processed", "contact_tracing_processed")) {
  mode <- tryCatch(pipeline_configs$contact_tracing$data$matching_input,
                   error = function(e) NULL)
  if (is.null(mode)) mode <- "latest"
  mode <- as.character(mode)[1]
  if (!mode %in% c("combined", "latest")) {
    stop("contact_tracing.data.matching_input must be \"combined\" or ",
         "\"latest\"; got \"", mode, "\".", call. = FALSE)
  }

  if (identical(mode, "latest")) {
    dir <- resolve_ct_latest_dir(processed_dir)
    return(list(mode = mode, dir = dir, stem = basename(dir),
                names_csv = file.path(dir, "contact_tracing_processed_names.csv")))
  }

  dir  <- file.path(processed_dir, "combined")
  meta <- file.path(dir, "combined_meta.json")
  names_csv <- file.path(dir, "contact_tracing_combined_names.csv")
  if (!file.exists(meta) || !file.exists(names_csv)) {
    stop("matching_input is \"combined\" but the combined contact tracing ",
         "dataset has not been built (", dir, ").\nRun ",
         "pipelines/contact_tracing_pipeline/2b-combine-exports.R, or the ",
         "Rebuild All-time Dataset button, first.", call. = FALSE)
  }
  latest <- jsonlite::fromJSON(meta)$latest_export
  date   <- regmatches(latest, regexpr("[0-9]{8}$", latest))
  list(mode = mode, dir = dir,
       stem = paste0("CT-COMBINED_", if (length(date)) date else "unknown"),
       names_csv = names_csv)
}


#' Pick the newest DHIS2 line list export named LineList_DD_MM_YYYY.xlsx (or .xls).
resolve_latest_dhis2_linelist <- function(
    raw_dir = file.path(repo_root, "data", "raw", "dhis2_linelist_raw")) {
  if (!dir.exists(raw_dir)) {
    stop("DHIS2 raw directory not found: ", raw_dir, call. = FALSE)
  }

  files <- list.files(raw_dir, full.names = TRUE)
  pattern <- "(?i)^Lin[e]?List[ _]([0-9]{2})_([0-9]{2})_([0-9]{4})\\.(xlsx|xls)$"
  hits <- character()
  dates <- as.Date(character())

  for (path in files) {
    name <- basename(path)
    m <- regexec(pattern, name, perl = TRUE)[[1]]
    if (m[1] == -1) {
      next
    }
    bits <- regmatches(name, list(m))[[1]]
    dd <- bits[2]
    mm <- bits[3]
    yyyy <- bits[4]
    file_date <- as.Date(sprintf("%s-%s-%s", yyyy, mm, dd))
    hits <- c(hits, path)
    dates <- c(dates, file_date)
  }

  if (!length(hits)) {
    stop(
      "No LineList_DD_MM_YYYY workbook found in ",
      raw_dir,
      " (expected names like LineList_25_06_2026.xlsx).",
      call. = FALSE
    )
  }

  hits[which.max(dates)]
}

#' Parse DD_MM_YYYY from a LineList workbook path.
parse_dhis2_linelist_date <- function(path) {
  name <- basename(path)
  pattern <- "(?i)^Lin[e]?List[ _]([0-9]{2})_([0-9]{2})_([0-9]{4})\\.(xlsx|xls)$"
  m <- regexec(pattern, name, perl = TRUE)[[1]]
  if (m[1] == -1) {
    stop("Could not parse line list date from filename: ", name, call. = FALSE)
  }
  bits <- regmatches(name, list(m))[[1]]
  dd <- bits[2]
  mm <- bits[3]
  yyyy <- bits[4]
  file_date <- as.Date(sprintf("%s-%s-%s", yyyy, mm, dd))
  list(
    date = file_date,
    coded = sprintf("%s_%s_%s", dd, mm, yyyy),
    label = format(file_date, "%d %B %Y"),
    basename = name
  )
}

#' Load a translation/rename map from data/maps/<name>.csv (columns: key, value)
#' and return it as a named character vector — a drop-in replacement for the
#' named vectors (e.g. SEX_MAP <- c("F" = "female", ...)) that used to be
#' hardcoded in the colname-standard scripts. Order and duplicate entries are
#' preserved exactly as they appear in the CSV, matching how c(...) behaves.
load_map_csv <- function(name, maps_dir = file.path(repo_root, "data", "maps")) {
  path <- file.path(maps_dir, paste0(name, ".csv"))
  if (!file.exists(path)) {
    stop("Map CSV not found: ", path, call. = FALSE)
  }
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                 fileEncoding = "UTF-8", na.strings = character(0))
  if (!all(c("key", "value") %in% names(df))) {
    stop("Map CSV must have 'key' and 'value' columns: ", path, call. = FALSE)
  }
  stats::setNames(df$value, df$key)
}

#' Rename a processed table's OLD column names to the current ones, from
#' data/maps/processed_column_aliases.csv (dataset, key = old, value = new).
#' For anything that reads processed exports written before a rename (the
#' combined contact tracing table, the metrics history), so a renamed column is
#' one continuous column rather than two half-empty ones. A column already
#' present under its new name is left alone. `dataset` is "epi_linelist" or
#' "contact_tracing".
apply_column_aliases <- function(df, dataset,
                                 path = file.path(repo_root, "data", "maps",
                                                  "processed_column_aliases.csv")) {
  if (is.null(df) || !file.exists(path)) return(df)
  a <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                fileEncoding = "UTF-8", na.strings = character(0))
  a <- a[a$dataset == dataset, , drop = FALSE]
  for (k in seq_len(nrow(a))) {
    old <- a$key[k]; new <- a$value[k]
    if (old %in% names(df) && !new %in% names(df))
      names(df)[names(df) == old] <- new
  }
  df
}

#' Load the health-zone alias table (data/aliases/health_zone_aliases.csv —
#' columns: observed_name, canonical_nom, source_dataset, notes) and return it
#' as a named character vector keyed by normalize_key(observed_name), matching
#' how zone_aliases is consumed in load_health_zone_lookup():
#'   for (alias_key in names(zone_aliases)) lookup[[alias_key]] <- zone_aliases[[alias_key]]
#' normalize_key is passed in rather than assumed global, since it's defined
#' locally in each colname-standard.R script (after this file is sourced).
load_zone_aliases_csv <- function(path, normalize_fn) {
  if (!file.exists(path)) {
    stop("Zone aliases CSV not found: ", path, call. = FALSE)
  }
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                 fileEncoding = "UTF-8", na.strings = character(0))
  required <- c("observed_name", "canonical_nom")
  if (!all(required %in% names(df))) {
    stop("Zone aliases CSV must have 'observed_name' and 'canonical_nom' columns: ", path, call. = FALSE)
  }
  observed  <- trimws(df$observed_name)
  canonical <- trimws(df$canonical_nom)
  keep <- nzchar(observed) & nzchar(canonical)
  stats::setNames(canonical[keep], vapply(observed[keep], normalize_fn, character(1)))
}

#script_dir <- get_script_dir() #deleteflag


# -----------------------------
# Download Data
# Moving to a fully operational pipeline will be accomplished in three phases.
# Phase 1: Raw data getting manually copied into data/raw <-- We are here
# Phase 2: Fetching raw data from INRB/Dav hosted Google drive
# Phase 3: Fetching raw data from secure host that is app database
# -----------------------------
# Read manifest - files to download from the drive
#manifest <- read.csv("drive_manifest.csv", stringsAsFactors = FALSE)

# Store OAuth token cache inside the project (avoids re-auth every time)
#options(gargle_oauth_cache = "data/cache")

# First run will open a browser to authenticate (users must have access to the shared Drive folder)
#drive_auth(cache = "data/cache")
# 
# for (i in seq_len(nrow(manifest))) {
#   out <- manifest$relative_path[i]
#   if (file.exists(out)) next
#   
#   message("Downloading: ", manifest$name[i], " -> ", out)
#   dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
#   
#   drive_download(
#     file = as_id(manifest$file_id[i]),
#     path = out,
#     overwrite = FALSE
#   )
#   
#   if (grepl(".zip", manifest$name[i])) {
#     message("Unzipping compressed file")
#     unzip(out, exdir = stringr::str_remove(out, manifest$name[i]))
#     file.remove(out)
#   }
# }
# 
# message("Done. Data available under data/raw/")



# -----------------------------
# Load Config
# -----------------------------
config_path <- file.path(repo_root, "pipelines", "pipeline_configs.json")
if (!file.exists(config_path)) {
  stop("Pipeline config not found: ", config_path, call. = FALSE)
}

pipeline_configs <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)

# -----------------------------
# Matching parameters
# -----------------------------
# One tuning value from pipeline_configs$matching, by dotted key:
#
#   match_param("collocation.min_ratio")
#
# WHY THIS HARD-ERRORS RATHER THAN DEFAULTING. These values decide which
# candidate pairs a reviewer ever sees. A mistyped key that quietly returned a
# default would change the tiering of an entire run and leave no trace — the
# same failure mode as the absolute IDF thresholds, which silently disabled the
# single-token tier in the CT dedup for as long as nobody measured it.
# load_assigned_id_registry() takes the same line on a stale registry schema.
#
# Every value is numeric, and NULL/NA/length != 1 is treated as absent rather
# than passed on: a threshold of NA compares FALSE against everything, which
# would empty a tier without erroring.
match_param <- function(key) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  node <- pipeline_configs$matching
  if (is.null(node)) {
    stop("pipeline_configs.json has no `matching` section — it holds the ",
         "tuning parameters every matching script reads.", call. = FALSE)
  }
  for (i in seq_along(parts)) {
    if (!is.list(node) || is.null(node[[parts[i]]])) {
      stop("matching parameter not found: `", key, "` (missing at `",
           paste(parts[seq_len(i)], collapse = "."), "`). Available at that ",
           "level: ", paste(names(node), collapse = ", "), call. = FALSE)
    }
    node <- node[[parts[i]]]
  }
  if (length(node) != 1L || !is.numeric(node) || is.na(node)) {
    stop("matching parameter `", key, "` is not a single number: ",
         paste(utils::capture.output(str(node)), collapse = " "), call. = FALSE)
  }
  node
}

# The whole block, for writing into a run's output so a past decision can be
# traced to the parameters that produced it. Returned as JSON text rather than
# a list because that is what every caller writes to disk.
match_params_json <- function() {
  jsonlite::toJSON(pipeline_configs$matching, auto_unbox = TRUE, pretty = TRUE)
}


# -----------------------------
# Health-zone geometry
# -----------------------------
# Moved here from identify_persons/0-build-zone-geo.R on 2026-09-10, and that
# script deleted.
#
# It never belonged in a per-export pipeline. It reads REFERENCE GEOGRAPHY --
# the GRID3 shapefile -- and nothing in any pipeline reads what it writes; the
# only consumer is the Shiny review map. So it was a step that depended on no
# export and fed no export, sitting in the middle of a run that processes one.
#
# The consequence was a gap: it was step 0 of Identify Persons but absent from
# run_full_pipeline_bg(), so on a machine that had never opened that tab the
# Home button produced everything EXCEPT the map data, and the map came up
# empty with no explanation. Living in setup makes that structurally
# impossible -- every runner sources 0-setup.R first, so every path gets it.
#
# Still gated the same way: unless the shapefile is newer than the derived
# files, this is one file.mtime() comparison. GRID3 publish a new version
# roughly never, so on a normal run it costs nothing and nobody has to remember
# a separate ritual.
#
# CONFIDENTIALITY
# ---------------------------------------------------------------------------
# Both outputs are PUBLIC REFERENCE GEOGRAPHY -- GRID3 administrative
# boundaries, no patient data of any kind. Unlike almost everything else the
# pipelines write, these two files are safe to share.
ensure_zone_geo <- function() {
  # -------------------------  Configuration  ------------------------------------

  ZG_SHP <- file.path(repo_root, "data", "geo",
                      "GRID3_COD_health_zones_v8", "GRID3_COD_health_zones_v8_0.shp")
  ZG_OUT <- file.path(repo_root, "data", "geo", "derived")

  # Simplification tolerance for zone outlines, in DEGREES.
  #
  # 0.01 deg is roughly 1.1 km. The map is drawn about 320 px wide and never
  # narrower than a 60 km window, so one pixel is ~190 m at the tightest zoom
  # and much coarser normally: 1.1 km is a few pixels of detail at worst, which
  # is the right side of invisible. Raising it starts to square off zone
  # boundaries visibly; lowering it multiplies the file size for detail no
  # reviewer can see.
  ZG_TOLERANCE <- 0.01


  # -------------------------  Helper Functions  ---------------------------------

  .zg_log <- character(0)
  zglog <- function(...) {
    txt <- paste0(...)
    message(txt)
    .zg_log <<- c(.zg_log, strsplit(txt, "\n", fixed = TRUE)[[1]])
  }

  zg_write_csv <- function(df, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE,
                quote = TRUE, na = "", qmethod = "double")
  }


  # -------------------------  Freshness check  ----------------------------------

  zg_centroids <- file.path(ZG_OUT, "health_zone_centroids.csv")
  zg_outlines  <- file.path(ZG_OUT, "health_zone_outlines.csv")

  zglog("=== Deriving health-zone geometry ===")

  if (!file.exists(ZG_SHP)) {
    zglog("  Shapefile not present: ", ZG_SHP)
    zglog("  Skipping. The review map will say it has no geography.")
    zone_geo_complete <- FALSE
  } else if (file.exists(zg_centroids) && file.exists(zg_outlines) &&
             file.mtime(zg_centroids) > file.mtime(ZG_SHP) &&
             file.mtime(zg_outlines)  > file.mtime(ZG_SHP)) {
    zglog("  Already derived and newer than the shapefile — skipping.")
    zglog("    ", zg_centroids)
    zglog("    ", zg_outlines)
    zone_geo_complete <- TRUE
  } else {

    # -----------------------  Read and reduce  ----------------------------------

    hz <- sf::st_read(ZG_SHP, quiet = TRUE)
    if (!all(c("zonesante", "province") %in% names(hz))) {
      stop("Expected 'zonesante' and 'province' columns in ", ZG_SHP, call. = FALSE)
    }
    zglog("  Read ", nrow(hz), " zones from the shapefile")

    # Geographic coordinates throughout. The map is a plain equirectangular
    # plot, so lon/lat is exactly what it needs; projecting would mean the app
    # having to un-project to place a dot.
    hz <- sf::st_transform(hz, 4326)

    # ---- Zone centroids ----
    # st_point_on_surface, not st_centroid: a centroid can fall OUTSIDE a
    # concave or horseshoe-shaped zone, which would put the dot in a neighbour.
    # point_on_surface is guaranteed to land inside the polygon. The warning
    # about assuming planar coordinates is expected and harmless here -- this is
    # a label position, not a measurement.
    pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(hz)))
    xy  <- sf::st_coordinates(pts)

    cent <- data.frame(
      health_zone = as.character(hz$zonesante),
      province    = as.character(hz$province),
      lon         = round(xy[, 1], 5),
      lat         = round(xy[, 2], 5),
      stringsAsFactors = FALSE
    )
    cent <- cent[!is.na(cent$lon) & !is.na(cent$lat), , drop = FALSE]
    cent <- cent[order(cent$province, cent$health_zone), , drop = FALSE]
    zg_write_csv(cent, zg_centroids)
    zglog("  Wrote ", nrow(cent), " zone centroids")

    # ---- Zone outlines ----
    # Simplified per zone, with preserveTopology so a zone cannot collapse to
    # nothing or self-intersect. No dissolving: each zone keeps its own boundary,
    # which is what the map draws.
    geom <- sf::st_make_valid(sf::st_geometry(hz))
    n_before <- nrow(sf::st_coordinates(geom))
    simp <- sf::st_simplify(geom, dTolerance = ZG_TOLERANCE,
                            preserveTopology = TRUE)

    # st_coordinates on (multi)polygons returns X, Y and grouping columns:
    # L1 = ring within a polygon, L2 = polygon within a multipolygon, L3 = which
    # feature. Which of L2/L3 exists depends on whether the geometry came back as
    # POLYGON or MULTIPOLYGON, so the feature index is taken from the LAST L
    # column rather than by assuming a name.
    co <- sf::st_coordinates(simp)
    lcols <- grep("^L[0-9]+$", colnames(co), value = TRUE)
    if (!length(lcols)) stop("st_coordinates returned no ring grouping columns",
                             call. = FALSE)
    feat <- as.integer(co[, lcols[length(lcols)]])
    # A ring id unique across the whole file, so ring 1 of zone A is never
    # confused with ring 1 of zone B when the app groups by it.
    ring <- as.integer(factor(do.call(paste, c(lapply(lcols, function(k) co[, k]),
                                               list(sep = "-")))))

    outl <- data.frame(
      health_zone = as.character(hz$zonesante)[feat],
      province    = as.character(hz$province)[feat],
      ring        = ring,
      lon         = round(co[, "X"], 4),
      lat         = round(co[, "Y"], 4),
      stringsAsFactors = FALSE
    )
    zg_write_csv(outl, zg_outlines)
    zglog("  Wrote ", nrow(outl), " outline points across ",
          length(unique(outl$health_zone)), " zones and ",
          length(unique(outl$ring)), " rings")
    zglog("    (simplified at ", ZG_TOLERANCE, " deg, down from ",
          n_before, " points -- ",
          round(100 * (1 - nrow(outl) / max(n_before, 1))), "% removed)")

    # SAY SO LOUDLY IF THE SIMPLIFICATION DID NOT SIMPLIFY.
    #
    # Found on 2026-09-10: the shipped health_zone_outlines.csv held 3,845,491
    # points -- 7% fewer than the shapefile's 4,131,532, against a design
    # target of ~27,000 -- and weighed 157 MB, two and a half times the 64 MB
    # shapefile it was meant to shrink, because text coordinates cost more than
    # binary ones. Nothing said anything: the file existed, was newer than the
    # shapefile, and the gate above skipped every subsequent run.
    #
    # The cost lands on the app, which read_csv()s the whole thing the first
    # time any cluster map renders.
    #
    # This does not fix the cause -- most likely dTolerance in degrees against
    # a geographic CRS, or preserveTopology holding vertices in -- it just makes
    # a bad reduction impossible to ship silently again.
    kept <- nrow(outl) / max(n_before, 1)
    if (kept > 0.25) {
      zglog("  WARNING: simplification kept ", round(100 * kept),
            "% of the original points. The target is a few percent; at this ",
            "rate the derived file is larger than the shapefile and the ",
            "review map will be slow to open. Check ZG_TOLERANCE and whether ",
            "st_simplify is behaving on a geographic CRS.")
      warning("zone geometry barely simplified: ", nrow(outl), " of ",
              n_before, " points kept", call. = FALSE)
    }

    zone_geo_complete <- TRUE
  }

  zglog("  NOTE: both outputs are public GRID3 reference geography. No patient ",
        "data. Unlike the rest of this pipeline's outputs, they are shareable.")

  dir.create(ZG_OUT, recursive = TRUE, showWarnings = FALSE)
  write_log_file(.zg_log, "zone_geo_log.txt")
  invisible(zone_geo_complete)
}

# Derived once per session, at setup, for every entry point.
zone_geo_complete <- ensure_zone_geo()


# ---------------------------------------------------------------------------
# NAME SCORING CORE
# ---------------------------------------------------------------------------
# Moved here on 2026-09-23. Previously the tokeniser, the transliteration map
# and the two collocation helpers existed as FOUR verbatim copies — in
# 2-deduplicate_linelist.R, 3-deduplicate_contacts.R, 1-build_name_rarity.R and
# 2-build-person-candidates.R — held in step by tests that extracted each copy
# and asserted it matched the others character for character.
#
# That arrangement detected drift; it did not prevent it, and it made every
# change to tokenisation a four-file edit that the tests would reject unless all
# four were kept in lockstep. The copies existed because each script had to run
# standalone and sourcing another pipeline script runs that pipeline as a side
# effect. 0-setup.R has neither problem: every script already sources it, and it
# does nothing but define things.
#
# The tests that pinned the copies now pin the single definition and the
# absence of any second one (tests/test_name_rarity.py,
# tests/test_name_score.py).

# On the explicit map rather than iconv(to = "ASCII//TRANSLIT"): iconv's
# TRANSLIT behaviour comes from the platform C library and is not portable.
# glibc renders "Gisèle" as "Gisele"; macOS commonly yields "Gis?le", and the
# "?" then acts as a token separator — so one name becomes one token on Linux
# and two on macOS, changing every rarity weight downstream. Identical input
# gave tier counts differing by up to 26% between two analysts' machines.
.NAME_TRANSLIT_FROM <- paste0(
  "àáâãäåèéêë",
  "ìíîïòóôõöù",
  "úûüýÿçñðþ",
  "ÀÁÂÃÄÅÈÉÊË",
  "ÌÍÎÏÒÓÔÕÖÙ",
  "ÚÛÜÝÇÑÐÞ"
)
.NAME_TRANSLIT_TO <- paste0(
  "aaaaaaeeee",
  "iiiiooooou",
  "uuuyycndt",
  "AAAAAAEEEE",
  "IIIIOOOOOU",
  "UUUYCNDT"
)

name_translit_ascii <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Multi-character expansions first; chartr() is strictly one-to-one.
  x <- gsub("ß", "ss", x, fixed = TRUE)
  x <- gsub("æ", "ae", x, fixed = TRUE)
  x <- gsub("Æ", "AE", x, fixed = TRUE)
  x <- gsub("œ", "oe", x, fixed = TRUE)
  x <- gsub("Œ", "OE", x, fixed = TRUE)
  x <- chartr(.NAME_TRANSLIT_FROM, .NAME_TRANSLIT_TO, x)
  gsub("[^ -~]", "", x)
}

# Apostrophes are DELETED, not turned into separators. 640 linelist names
# (1.8%) contain one, and they are Alur and Lendu orthography from Ituri —
# DZ'KANA BOLO, UDJONG'A MBELE, ALING'THA NOZA. The apostrophe is part of the
# name, not a word break. Splitting on it would mint `dz` as a token in 160
# unrelated people, plus `da`, `ang`, `na` and other 2-3 character fragments,
# which would then read as shared name evidence between people who share
# nothing — dragging distinctive names toward the common end. Small in
# aggregate (1.8% of names) but concentrated entirely in one community's names,
# which is the worst place for a systematic bias.
#
# Digits are KEPT ([^a-z0-9 ]).
#
# min_chars and max_tokens default to the config rather than to literals, so a
# caller that does not pass them cannot silently disagree with the corpus the
# rarity table was counted over.
name_tokens <- function(x,
                        min_chars  = match_param("tokenisation.score_min_token_chars"),
                        max_tokens = match_param("tokenisation.max_tokens_per_name")) {
  x <- tolower(trimws(as.character(x)))
  x <- name_translit_ascii(x)
  x <- gsub("['`’]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  lapply(strsplit(trimws(x), "[[:space:]]+"), function(tk) {
    tk <- tk[nzchar(tk) & nchar(tk) >= min_chars]
    utils::head(tk, max_tokens)
  })
}

# ---------------------------------------------------------------------------
# WHEN DO TWO DIFFERENTLY-SPELLED TOKENS COUNT AS THE SAME TOKEN?
# ---------------------------------------------------------------------------
# An EDIT BUDGET that grows with the length of the shorter token, replacing the
# single Jaro-Winkler ceiling of 0.15 that 2-deduplicate_linelist.R used until
# 2026-09-23.
#
# WHY NOT A FIXED JARO-WINKLER CEILING. JW is normalised by string length, so
# one ceiling means "a constant FRACTION of the token may be wrong". A single
# substituted letter scores 0.133 on a 4-letter token, 0.078 on a 6-letter one
# and 0.044 on a 9-letter one, so a long name absorbs a typo three times more
# easily than a short one. That is not a subtle effect: at the old 0.15 line the
# rejected-but-genuine pairs were all long (`voctorine`/`victorine`,
# `tibalengwa`/`tibilengwa`) while the accepted-but-junk ones were all short
# (`de`/`dieu`, `mbalisi`/`mbavu`, `bahati`/`batia`). Tightening the ceiling
# made it worse at both ends at once. What we actually believe is that a small
# NUMBER of characters may be wrong, and that is an edit count.
#
# MEASURED, over the 8,179 token comparisons inside LINELIST_01092026's
# candidate pairs, against corroborating fields (sex / age / zone / area /
# onset) which are independent of spelling:
#
#   rule                          kept   backed by 4-5   by 0-1   ratio
#   JW <= 0.15 (old)              1101             470      153     2.4
#   this edit budget               944             431       88     3.7
#
# WHY JARO-WINKLER IS NOT ALSO APPLIED. Requiring both changes 7 comparisons out
# of 8,179, and one of those 7 is `kalembu`/`kalebmu` -- a real match with all
# five fields agreeing, which JW rejects at 0.152. A second metric that costs a
# true match to remove a rounding error is not worth the second set of
# constants. NOTE this is only about the TOKEN gate: the whole-name JW gate
# (name_weight.global_max_dist, 0.10) is a different test, runs first, and is
# unchanged. It is also what makes this rule safe -- see the note on the
# distance penalty in 2-deduplicate_linelist.R.
#
# WHY A SWAP-AWARE EDIT DISTANCE rather than plain Levenshtein: it counts an
# adjacent character swap as one edit, not two. 16 borderline pairs here contain
# a swap and 11 sit in pairs where 4-5 fields agree (`katsuva`/`kastuva`,
# `ruth`/`ruht`, `ngokpa`/`ngopka`); the recurring ts/st pattern reads as a
# transcription habit, not noise. Plain Levenshtein would charge 2 and a 1-edit
# budget would throw them away.
#
# The metric is "osa", NOT "dl", and that is deliberate -- stringdist offers two
# and both get called Damerau-Levenshtein. See matching.token_match._metric_
# comment: they differ only where a character would be transposed twice, which
# name tokens do not do, but the Python reference implements "osa" and the two
# have to compute the same number.
#
# Lives here because all three matchers use it. CT and the cross-file matcher
# adopted it on 2026-09-23, via name_pair_score() below; until then they matched
# tokens EXACTLY and this was reached only from the linelist dedup.
name_token_budget <- function(n_chars) {
  spec <- pipeline_configs$matching$token_match$budget
  if (is.null(spec)) {
    stop("pipeline_configs.json has no matching.token_match.budget — it ",
         "decides when two spellings count as the same token.", call. = FALSE)
  }
  # data.frame when jsonlite simplifies the array, list of lists otherwise.
  lim <- if (is.data.frame(spec)) spec$max_token_chars
         else vapply(spec, function(r) r$max_token_chars, numeric(1))
  bud <- if (is.data.frame(spec)) spec$max_edits
         else vapply(spec, function(r) r$max_edits, numeric(1))
  ord <- order(lim); lim <- lim[ord]; bud <- bud[ord]
  # findInterval, not a loop: this is called once per token comparison and the
  # comparison count runs to millions on a full export.
  #
  # We want the FIRST tier whose max_token_chars is >= n_chars, i.e. one more
  # than the number of limits strictly below it. findInterval() counts limits
  # <= its argument, and token lengths are whole numbers, so n_chars - 1 turns
  # "<=" into "<". Written this way rather than with left.open = TRUE, which
  # findInterval() only gained in a later R and would tie this file to it.
  bud[pmin(findInterval(as.integer(n_chars) - 1L, lim) + 1L, length(bud))]
}

# TRUE where two tokens are close enough to count as the same token.
# Vectorised over pairs; `a` and `b` are character vectors of equal length.
name_tokens_match <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  # "osa", not "dl", if the key is ever missing -- the Python reference
  # implements optimal string alignment and the two must agree exactly.
  metric <- pipeline_configs$matching$token_match$metric
  if (is.null(metric) || !nzchar(metric)) metric <- "osa"
  edits <- stringdist::stringdist(a, b, method = metric)
  !is.na(edits) & edits <= name_token_budget(pmin(nchar(a), nchar(b)))
}


# THE NAME SCORE FOR ONE PAIR, defined once.
#
# 2-deduplicate_linelist.R built this first and kept it to itself; CT and the
# cross-file matcher meanwhile scored on intersect(), so the same two names were
# worth different amounts depending on which script asked. This is that scorer
# with the vectorisation removed, for callers that work a pair at a time. The
# linelist keeps its own vectorised path because it scores every surviving pair
# at once and a per-pair call would be far slower.
#
# THOSE TWO IMPLEMENTATIONS ARE NOT CHECKED AGAINST EACH OTHER, and that is a
# known gap rather than an oversight. Both are R, and there is no R in the test
# environment -- the suite compares the R to a Python port by regexing constants
# out of the source, which cannot reach a greedy assignment loop. Until the
# linelist is moved onto this function too, or a harness runs both over the same
# pairs, an edit to one and not the other will not be caught by anything. Change
# them together.
#
# Returns a list, not a number, because the parts are what make a score
# explicable to a reviewer: what the plain sum was, what the dependence
# correction removed, whether the repetition bonus fired, and how many tokens
# of the shorter name found no partner at all.
#
# `idf_of` takes a character vector and returns a numeric one, unseen tokens
# included -- pass the one from load_name_rarity() so every caller agrees about
# what a token is worth. `groups`/`group_idf` come from the same place and must
# stay in its order: largest group first, then most inflation removed.
name_pair_score <- function(ta_raw, tb_raw, idf_of,
                            groups = list(), group_idf = numeric(0),
                            repeat_bonus = 0,
                            max_tokens = match_param("tokenisation.max_tokens_per_name")) {
  empty <- list(weight = 0, weight_plain = 0, colloc_reduction = 0,
                shared_repeat = FALSE, n_matched = 0L, n_unmatched = 0L,
                best_idf = 0, n_shorter = 0L)
  ta <- unique(as.character(ta_raw)); tb <- unique(as.character(tb_raw))
  if (!length(ta) || !length(tb)) return(empty)

  # Every token against every token, then the budget gate. Names are at most
  # `max_tokens` long, so this is a handful of comparisons.
  gi <- rep(seq_along(ta), times = length(tb))
  gj <- rep(seq_along(tb), each  = length(ta))
  a <- ta[gi]; b <- tb[gj]
  keep <- name_tokens_match(a, b)
  n_shorter <- min(length(ta), length(tb))
  if (!any(keep)) {
    empty$n_unmatched <- as.integer(n_shorter)
    empty$n_shorter   <- as.integer(n_shorter)
    empty$shared_repeat <- .name_shared_repeat(ta_raw, tb_raw)
    empty$weight <- repeat_bonus * as.numeric(empty$shared_repeat)
    return(empty)
  }
  gi <- gi[keep]; gj <- gj[keep]; a <- a[keep]; b <- b[keep]

  metric <- pipeline_configs$matching$token_match$metric
  if (is.null(metric) || !nzchar(metric)) metric <- "osa"
  dist <- stringdist::stringdist(a, b, method = metric)

  idf_a <- idf_of(a); idf_b <- idf_of(b)
  w     <- pmin(idf_a, idf_b)
  # The MORE COMMON surface form, so the collocation lookup below asks about the
  # same token whose IDF was used. `lucie`/`lycie` scores and matches as `lucie`.
  t_min <- ifelse(idf_a <= idf_b, a, b)

  # Greedy best-first: heaviest match, then the heaviest remaining that reuses
  # neither token. `dist` is a genuine tie-break, not decoration -- with no
  # distance penalty an exact match and a one-edit match to the same token weigh
  # the SAME, so without it a name could match its typo in preference to its
  # exact counterpart. Fewest edits next, then position, for determinism.
  ord <- order(-w, dist, gi, gj)
  used_a <- logical(length(ta)); used_b <- logical(length(tb))
  take <- integer(0)
  for (k in ord) {
    if (used_a[gi[k]] || used_b[gj[k]]) next
    used_a[gi[k]] <- TRUE; used_b[gj[k]] <- TRUE
    take <- c(take, k)
    if (length(take) >= max_tokens) break
  }

  m_w <- w[take]; m_tok <- t_min[take]
  plain <- sum(m_w)

  # Dependence correction: a collocated group replaces its parts with the
  # group's own counted frequency. A token may be claimed by one group only, and
  # a pair holding two tokens of a three-token group does not hold the group.
  claimed <- rep(FALSE, length(take))
  reduction <- 0
  for (g in seq_along(groups)) {
    tk <- groups[[g]]
    sel <- !claimed & m_tok %in% tk
    if (!all(tk %in% m_tok[sel])) next
    reduction <- reduction + (sum(m_w[sel]) - group_idf[g])
    claimed <- claimed | sel
  }

  rep_shared <- .name_shared_repeat(ta_raw, tb_raw)
  list(weight        = plain - reduction + repeat_bonus * as.numeric(rep_shared),
       weight_plain  = plain,
       colloc_reduction = reduction,
       shared_repeat = rep_shared,
       n_matched     = length(take),
       n_unmatched   = as.integer(n_shorter - length(take)),
       best_idf      = if (length(m_w)) max(m_w) else 0,
       n_shorter     = as.integer(n_shorter))
}

# Both sides repeat the same token. One-sided repetition earns nothing: it says
# something about one record's data entry, not about the two agreeing.
.name_shared_repeat <- function(ta_raw, tb_raw) {
  ra <- unique(ta_raw[duplicated(ta_raw)])
  rb <- unique(tb_raw[duplicated(tb_raw)])
  length(intersect(ra, rb)) > 0L
}


# Co-occurrence counts for every token pair appearing together at all.
# `tok_per_entry` must already be de-duplicated per entry.
name_pair_counts <- function(tok_per_entry) {
  keys <- unlist(lapply(tok_per_entry, function(tk) {
    if (length(tk) < 2L) return(character(0))
    tk <- sort(unique(tk))
    cb <- utils::combn(tk, 2L)
    paste0(cb[1L, ], "\r", cb[2L, ])   # \r cannot occur in a token
  }), use.names = FALSE)
  if (!length(keys)) return(data.frame(a = character(0), b = character(0),
                                       together = numeric(0),
                                       stringsAsFactors = FALSE))
  tb <- table(keys)
  parts <- do.call(rbind, strsplit(names(tb), "\r", fixed = TRUE))
  data.frame(a = parts[, 1L], b = parts[, 2L],
             together = as.numeric(tb), stringsAsFactors = FALSE)
}

# Groups in which EVERY pair is collocated, not merely connected.
#
# The distinction is the whole safety of this step. `jean` collocates with 13
# other tokens, so gluing transitively would fuse {jean, pierre, paul} into one
# unit -- asserting that "Jean Pierre Paul" is an attested compound when
# `pierre` and `paul` never once appear together in 37,903 entries. Requiring
# all pairs means a group is only ever claimed when the corpus has actually
# seen that whole combination.
name_collocation_cliques <- function(
    edges,
    max_size = match_param("tokenisation.max_tokens_per_name")) {
  if (!nrow(edges)) return(list())
  adj <- list()
  for (i in seq_len(nrow(edges))) {
    a <- edges$a[i]; b <- edges$b[i]
    adj[[a]] <- c(adj[[a]], b)
    adj[[b]] <- c(adj[[b]], a)
  }
  seen <- new.env(parent = emptyenv())
  out  <- list()
  cur  <- lapply(seq_len(nrow(edges)),
                 function(i) sort(c(edges$a[i], edges$b[i])))
  for (g in cur) assign(paste(g, collapse = "\r"), TRUE, envir = seen)
  out <- c(out, cur)
  while (length(cur) && length(cur[[1L]]) < max_size) {
    nxt <- list()
    for (g in cur) {
      cand <- Reduce(intersect, lapply(g, function(t) adj[[t]]))
      for (n in setdiff(cand, g)) {
        ng <- sort(c(g, n))
        k  <- paste(ng, collapse = "\r")
        if (!exists(k, envir = seen, inherits = FALSE)) {
          assign(k, TRUE, envir = seen)
          nxt[[length(nxt) + 1L]] <- ng
        }
      }
    }
    if (!length(nxt)) break
    out <- c(out, nxt)
    cur <- nxt
  }
  out
}


# ---------------------------------------------------------------------------
# THE RUNNING NAME RARITY TABLE
# ---------------------------------------------------------------------------
# 1-build_name_rarity.R writes token rarity and collocations to one folder per
# run:
#
#   data/processed/person_matching/<stem>/name_rarity/
#
# and records <stem> in the name_rarity section of data/processed/latest.json.
# The current table is whichever snapshot latest.json names (there used to be
# a separate current/ folder holding a byte copy of it; latest.json made that
# redundant). The snapshot is written first and latest.json updated last, so a
# run that dies partway leaves the pipeline on the previous complete table.
#
# WHY ONE SHARED TABLE. Both dedups and the cross-file matcher
# need the same notion of "rare", and they need it BEFORE they run — a token's
# rarity is a property of the outbreak's whole name population, not of whichever
# file happens to be in hand. Under the old dated-folder layout there was no
# path a script could rely on, so each computed its own; that is precisely how
# three mutually incompatible definitions of "rare" came to exist, one of them
# (CT's) saturating at log(2798) and scoring a genuinely rare name the same as a
# merely unseen one.
#
# WHY ONE FOLDER PER RUN. A rarity score is evidence behind a review decision,
# and IDF moves as the corpus grows: a reviewer's judgement is only auditable
# against the numbers they actually saw, so each run's table is kept.
#
# THE PERSON-MATCHING FOLDER. data/processed/person_matching/ holds the running
# records at the top (link_ledger.csv, person_registry.csv,
# person_registry_history.csv) and one folder per run, named after the exports
# it was built from (LINELIST_x__CT-COMBINED_y), holding everything that run
# produced:
#   <run>/name_rarity/          the rarity table (1-build_name_rarity.R)
#   <run>/candidates/           links, clusters, review queues, tile data
#                               (2-build-person-candidates.R)
#   <run>/matching_params.json  the matching settings the run used
# plus three running folders of human work, one file per person, that belong
# to no run (see reviewer_history_dir() below):
#   reviewer_history/<name>.csv    every vote a reviewer has cast
#   harmoniser_history/<name>.csv  every verdict a harmoniser has recorded
#   near_miss_history/<name>.csv   Algorithm Tuning near-miss verdicts (same /
#                                  different / unsure), per link_id. Replaced
#                                  <run>/reviews/algorithm_flags-<name>.csv,
#                                  which nothing read, on 2026-10-18.
# plus archive/ for anything retired. shiny/app.R mirrors these paths
# (pm_run_dir()), because the app does not source this file at startup.
person_matching_dir <- function() {
  file.path(repo_root, "data", "processed", "person_matching")
}

#' One run's folder, or one part of it ("name_rarity", "candidates", "reviews").
pm_run_dir <- function(stem, part = NULL) {
  d <- file.path(person_matching_dir(), stem)
  if (is.null(part)) d else file.path(d, part)
}

name_rarity_dir <- function(stem) pm_run_dir(stem, "name_rarity")

# ---- Reviewer and harmoniser history ----------------------------------------
# Votes and verdicts used to live in each run's reviews/ folder, so a new run
# left them behind: the review tab opened an empty file, the ledger stopped
# counting them, and a harmonisation saved in an older run never reached the
# ledger. They now live in two running folders, ONE FILE PER PERSON, so two
# people working at once never write to the same file (a shared file would
# lose whichever save landed first).
#
# Rows are only ever ADDED. A person's latest row on a link is the one that
# counts, so changing your mind is a new row and the history is kept. Links
# are keyed on link_id (node_pair_key(), byte order), which does not change
# between runs. No names or other personal fields are written here.
#
# reviewer_history/<name>.csv
#   vote             kept / broken; "" = vote withdrawn (cluster un-marked)
#   edge_reason      e.g. auto:exact_id (prefill left in place),
#                    override:auto:exact_id. A broken link may also carry its
#                    own reasons as a "break:" suffix (since 2026-10-18):
#                    break:age_disc;sex_mismatch, or
#                    override:auto:score|break:age_disc
#   cluster_mark     confirmed / unsure
#   cluster_reasons  ;-joined reason codes ticked for the cluster
# harmoniser_history/<name>.csv
#   final_decision   kept / broken
#   final_cluster_verdict  settled / unsure / escalate
#   final_reasons, final_note, harmonised_from (reviewers shown)
# both also carry: link_id, node_a, node_b, recorded_on (YYYY-MM-DD HH:MM:SS),
#   run (the candidate run on screen), cluster_members (;-joined node IDs in
#   the cluster at the time, so a later reader can tell if it has changed).
REVIEWER_HISTORY_COLS <- c("link_id", "node_a", "node_b", "vote",
                           "edge_reason", "cluster_mark", "cluster_reasons",
                           "recorded_on", "run", "cluster_members")
HARMONISER_HISTORY_COLS <- c("link_id", "node_a", "node_b", "final_decision",
                             "final_cluster_verdict", "final_reasons",
                             "final_note", "harmonised_from", "recorded_on",
                             "run", "cluster_members")

reviewer_history_dir   <- function() file.path(person_matching_dir(), "reviewer_history")
harmoniser_history_dir <- function() file.path(person_matching_dir(), "harmoniser_history")

#' Every person's file in a history folder, latest row per person per link.
#'
#' Returns one data frame with a `who` column (the file name), or NULL when
#' the folder is empty. "Latest" is the LAST row in the file: rows are only
#' appended, so file order is time order, and that avoids trusting clocks.
read_history_latest <- function(dir) {
  f <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(f)) return(NULL)
  parts <- lapply(f, function(p) {
    d <- tryCatch(read.csv(p, stringsAsFactors = FALSE, na.strings = "",
                           colClasses = "character"),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d) || !"link_id" %in% names(d)) return(NULL)
    d[is.na(d)] <- ""
    d$who <- sub("\\.csv$", "", basename(p))
    d[!duplicated(d$link_id, fromLast = TRUE), , drop = FALSE]
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(d) {
    for (cl in setdiff(cols, names(d))) d[[cl]] <- ""
    d[, cols, drop = FALSE]
  })
  do.call(rbind, parts)
}

# The snapshot folder of the current table, per latest.json. Errors (rather
# than guessing) if latest.json has no name_rarity entry or the folder is gone.
current_name_rarity_dir <- function() {
  stem <- read_latest("name_rarity")$stem
  if (is.null(stem) || !nzchar(stem)) {
    stop("No name_rarity entry in ", latest_json_path(), "\n  Run ",
         "pipelines/identify_persons/1-build_name_rarity.R first — it builds ",
         "the pooled token rarity and collocations every matching script ",
         "reads.", call. = FALSE)
  }
  d <- name_rarity_dir(stem)
  if (!dir.exists(d))
    stop("latest.json names name rarity run ", stem, " but ", d,
         " does not exist.", call. = FALSE)
  d
}

# Reads the running tables and returns everything a consumer needs to score a
# name, as ONE object, so no caller has to reassemble the pieces or decide what
# an unseen token is worth.
#
# HARD-ERRORS when the table is absent, for the same reason match_param() does:
# a consumer that quietly fell back to computing its own would reintroduce the
# drift this table exists to remove, and would do it silently.
load_name_rarity <- function(dir = current_name_rarity_dir()) {
  rarity_path <- file.path(dir, "name_rarity.csv")
  colloc_path <- file.path(dir, "name_collocations.csv")
  meta_path   <- file.path(dir, "name_rarity_meta.json")

  if (!file.exists(rarity_path)) {
    stop("No name rarity table at ", rarity_path, "\n  Run ",
         "pipelines/identify_persons/1-build_name_rarity.R first — it builds ",
         "the pooled token rarity and collocations every matching script ",
         "reads.", call. = FALSE)
  }

  rarity <- utils::read.csv(rarity_path, stringsAsFactors = FALSE,
                            na.strings = "")
  meta   <- if (file.exists(meta_path)) jsonlite::fromJSON(meta_path) else list()

  colloc <- if (file.exists(colloc_path)) {
    utils::read.csv(colloc_path, stringsAsFactors = FALSE, na.strings = "",
                    colClasses = c(tokens = "character"))
  } else {
    data.frame(tokens = character(0), idf = numeric(0),
               stringsAsFactors = FALSE)
  }

  N <- if (!is.null(meta$corpus_entries)) as.numeric(meta$corpus_entries) else NA_real_
  idf_unseen <- if (!is.null(meta$idf_unseen)) as.numeric(meta$idf_unseen) else log(N)

  # The table is written rounded to 4 dp; recomputing from n_entries would give
  # a slightly different number than the one a reviewer saw. Use what is on
  # disk.
  idf_vec <- stats::setNames(as.numeric(rarity$idf), rarity$token)

  idf_of <- function(tk) {
    v <- idf_vec[tk]
    v[is.na(v)] <- idf_unseen
    unname(as.numeric(v))
  }

  groups <- if (nrow(colloc)) {
    strsplit(trimws(colloc$tokens), "[[:space:]]+")
  } else {
    list()
  }
  group_idf <- if (nrow(colloc)) as.numeric(colloc$idf) else numeric(0)

  # Largest group first, then most inflation removed: a token may be claimed by
  # only one group, and the larger group is the more specific claim. The build
  # script writes the table in this order already; re-sorting here means a
  # hand-edited or re-ordered CSV cannot change what a name scores.
  if (length(groups)) {
    ord <- order(-lengths(groups),
                 -(vapply(groups, function(g) sum(idf_of(g)), numeric(1)) -
                     group_idf))
    groups    <- groups[ord]
    group_idf <- group_idf[ord]
  }

  # The weight of a set of tokens, with collocated groups scored as one unit.
  # Greedy over the ordering above, and a token consumed by a group cannot be
  # claimed by another.
  weight_tokens <- function(tk) {
    tk <- unique(tk)
    if (!length(tk)) return(0)
    total <- 0
    for (gi in seq_along(groups)) {
      g <- groups[[gi]]
      if (all(g %in% tk)) {
        total <- total + group_idf[gi]
        tk <- setdiff(tk, g)
        if (!length(tk)) return(total)
      }
    }
    total + sum(idf_of(tk))
  }

  list(rarity = rarity, colloc = colloc, meta = meta,
       n = N, idf_unseen = idf_unseen, idf_vec = idf_vec, idf_of = idf_of,
       groups = groups, group_idf = group_idf, weight_tokens = weight_tokens,
       built_at = meta$built_at, stem = meta$stem, dir = dir)
}


# ---------------------------------------------------------------------------
# THE 0-1 LINK SCORE
# ---------------------------------------------------------------------------
# Every link gets one score in [0, 1], a weighted sum of a name component and a
# corroborator component. The weights live in pipeline_configs.json under
# matching.score_weights so they can be tuned in one place; they are asserted
# to sum to 1 here rather than normalised, because a pair that silently does
# not sum to 1 makes every recorded score incomparable with every other run's.
# The corroborator half of a link score, in [0, 1]. Defined once, because all
# three matchers write the same two columns and should turn them into the same
# number.
#
# Takes the COUNTS rather than the fields themselves, which is what lets one
# function serve five comparisons with different field sets and different
# denominators -- the linelist's five, the CT contact pass's five, the two
# one-field source-case comparisons.
#
# net = agreements - disagreements. A field neither side can be compared on is
# absent from n_avail, so it contributes nothing: no credit, no penalty. That
# is the difference between "we looked and they differ", which counts against
# the pair, and "one side is blank", which is not evidence of anything.
#
# Centred on zero, so a pair with nothing comparable scores 0.5 and lands in
# the middle of its half rather than being penalised for missing data.
corrob_score <- function(n_corr, n_avail) {
  a <- suppressWarnings(as.numeric(n_corr))
  v <- suppressWarnings(as.numeric(n_avail))
  s <- match_param("corroborators.score_steepness")
  net <- 2 * a - v
  ifelse(is.na(net), NA_real_, 1 / (1 + exp(-net / s)))
}


score_weights <- function() {
  w <- c(name = match_param("score_weights.name"),
         corroborator = match_param("score_weights.corroborator"))
  if (abs(sum(w) - 1) > 1e-9) {
    stop("matching.score_weights must sum to 1; got name = ", w[["name"]],
         " + corroborator = ", w[["corroborator"]], " = ", sum(w),
         call. = FALSE)
  }
  w
}


# -----------------------------
# Clean Environment
# -----------------------------
keep_objects <- c("keep_objects", "manifest", "setup_complete", "get_script_dir",
                  "repo_root", "pipeline_configs", "resolve_repo_root",
                  "resolve_epi_latest_dir", "resolve_ct_latest_dir",
                  "latest_json_path", "read_latest", "write_latest",
                  "logs_root", "log_run_dir", "new_log_run",
                  "current_name_rarity_dir", "person_matching_dir", "pm_run_dir",
                  "node_pair_key",
                  "REVIEWER_HISTORY_COLS", "HARMONISER_HISTORY_COLS",
                  "reviewer_history_dir", "harmoniser_history_dir",
                  "read_history_latest",
                  "write_log_file", "find_log",
                  "resolve_ct_matching_input",
                  "resolve_latest_dhis2_linelist", "parse_dhis2_linelist_date",
                  "load_map_csv", "load_zone_aliases_csv", "apply_column_aliases",
                  "match_param", "match_params_json",
                  "ensure_zone_geo", "zone_geo_complete",
                  "dhis2_force_stems", "ct_force_stems",
                  ".NAME_TRANSLIT_FROM", ".NAME_TRANSLIT_TO",
                  "name_translit_ascii", "name_tokens",
                  "name_token_budget", "name_tokens_match",
                  "name_pair_score", ".name_shared_repeat",
                  "name_pair_counts", "name_collocation_cliques",
                  "name_rarity_dir", "load_name_rarity",
                  "corrob_score", "score_weights")

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)

# -----------------------------
# Set 'setup' flag
# -----------------------------
setup_complete <- TRUE

