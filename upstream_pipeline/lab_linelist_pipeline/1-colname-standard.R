#!/usr/bin/env Rscript
# Clean and standardise Ituri and Nord-Kivu INRB lab line lists.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  getwd()
}
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)

setup_path <- file.path(repo_root, "src", "0-setup.R")
if (!file.exists(setup_path)) {
  stop("Setup script not found: ", setup_path, call. = FALSE)
}

# Package setup aligned with src/0-setup.R (not sourced: that script clears the env).
req_pkgs <- c(
  "readr", "dplyr", "tidyr", "stringr",
  "lubridate", "ggplot2", "sf", "readxl",
  "googledrive", "gargle"
)
for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(sf)
  library(readxl)
  library(googledrive)
  library(gargle)
})
data_repo <- normalizePath(file.path(repo_root, "..", "BDBV2026-Data"), mustWork = FALSE)
shapefile_path <- file.path(data_repo, "data", "shapefiles", "DRC_Health_zones.shp")
aliases_path <- file.path(data_repo, "data", "aliases.csv")
raw_dir <- file.path(repo_root, "data", "raw", "lab_linelist_raw")
processed_dir <- file.path(repo_root, "data", "processed", "lab_linelist_processed")
HEALTH_AREA_NAMES_PATH <- file.path(
  repo_root, "data", "geo", "rdc_aires-de-sante", "health_area_names.csv"
)

ITURI_XLSX <- file.path(raw_dir, "BD-LABORATOIRE 05 juin 2026.xlsx")
ITURI_SHEET <- "Base Nettoyer"
ITURI_OUTPUT <- file.path(processed_dir, "ituri_processed_linelist_latest.csv")

NK_XLSX <- list.files(
  raw_dir,
  pattern = "^Beni-Butembo_MVE17_Partage_.*\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)[1]
NK_SHEET <- "FVH_NK_INRB_BENI_BUTEMBO"
NK_HEADER_ROW <- 7L
NK_OUTPUT <- file.path(processed_dir, "nord_kivu_processed_linelist_latest.csv")

DROP_COLUMNS <- c(
  "N\u00ba", "N\u00b0", "No", "N\u00ba ",
  "Tranche d\u00e2ge", "Tranche d'âge"
)

ITURI_EXPLICIT_RENAMES <- c(
  "No" = "record_number",
  "ID LABO KIN" = "kinshasa_lab_id",
  "ID LABO BUNIA" = "bunia_lab_id",
  "Nom du Labo" = "lab_name",
  "Statut du pr\u00e9l\u00e8vement" = "sample_collection_status",
  "Nombre d'echantillon" = "sample_count",
  "Statut Final" = "final_status",
  "Date de d\u00e9c\u00e8s" = "date_of_death",
  "Age" = "age",
  "Sexe" = "sex",
  "Province" = "province",
  "Zone de sant\u00e9" = "health_zone",
  "Air de sant\u00e9" = "health_area",
  "Date de d\u00e9but sympt\u00f4mes" = "date_of_symptom_onset",
  "Date pr\u00e9l\u00e8vement" = "date_of_sample_collection",
  "Type d\u2019\u00e9chantillon" = "sample_type",
  "Type d'\u00e9chantillon" = "sample_type",
  "DATE RECEPETION" = "date_of_reception",
  "DELAIS D'ACH" = "turnaround_days",
  "DELAIS RENDU DE RESULTAT" = "result_turnaround_days",
  "DATE D'ANALYSE 1 BUNIA" = "bunia_analysis_date",
  "R\u00e9sultat" = "test_result",
  "RadiOne (Ct GP)" = "radione_ct_gp",
  "Contr\u00f4le Interne" = "internal_control",
  "DATE RESULTAT ALTON" = "altona_result_date",
  "RESULTAT ALTONA BUNIA" = "altona_bunia_result",
  "Positif correction" = "positive_correction",
  "N\u00e9gatif correction" = "negative_correction",
  "GeneXpert Ct Np" = "genexpert_ct_np",
  "GeneXpert Resultat" = "genexpert_result",
  "DATE D'ANALYSE 2 KINSHASA" = "kinshasa_analysis_date",
  "Resultat ALTONA" = "altona_final_result",
  "Commentaire" = "comment",
  "Remarques" = "remarks"
)

NK_EXPLICIT_RENAMES <- c(
  "N\u00b0" = "record_number",
  "ID labo \r\n(NUMERO DE L' ECHANTILLON)" = "lab_sample_id",
  "\r\n(NUMERO EPIDEMIOLOGIQUE)" = "epidemiological_number",
  "Nouveau prelevement ou reprelevement?" = "collection_event_type",
  "Date de Notification \r\n(DD/MM/YYY)" = "date_of_notification",
  "Statut du patient au moment collecte \u00e9chantillon \r\n(DCD, VIVANT)" = "sample_collection_status",
  "Age \r\n(nombre)" = "age",
  "Type d'age \r\n(ANS, MOIS, JOURS)" = "age_unit",
  "Sexe \r\n(Homme,Femme)" = "sex",
  "Pays d'origine" = "country_of_origin",
  "Province" = "province",
  "Zone de Sant\u00e9 Debut maladie" = "health_zone",
  "Aire de Sant\u00e9" = "health_area",
  "Type d'Echantillon" = "sample_type",
  "Date Debut de Symptomes (DD/MM/YYY)" = "date_of_symptom_onset",
  "Date de Prelevement (DD/MM/YYY)" = "date_of_sample_collection",
  "Date d'Analyse (DD/MM/YYY)" = "analysis_date",
  "RESULTAT FINAL" = "final_result",
  "OBSERVATION" = "remarks"
)

NK_UNCLEAR_COLUMNS <- c(
  "epidemiological_number" = "No direct Ituri equivalent (Ituri uses kinshasa_lab_id / bunia_lab_id).",
  "age_unit" = "Ituri encodes age units inside the single Age column (e.g. '3 mois').",
  "country_of_origin" = "Not present in the Ituri lab export.",
  "collection_event_type" = "Closest Ituri field is sample_collection_status (alive/deceased), not new vs re-collection.",
  "analysis_date" = "Ituri splits analysis dates into bunia_analysis_date and kinshasa_analysis_date.",
  "date_of_notification" = "Ituri primarily uses date_of_reception rather than notification date.",
  "health_zone" = "Mapped from 'Zone de Sant\u00e9 Debut maladie' (wording differs from Ituri 'Zone de sant\u00e9')."
)

COLLECTION_STATUS_MAP <- c(
  "VIVANT" = "Alive",
  "VIVANTE" = "Alive",
  "DECEDE" = "Deceased",
  "DCD" = "Deceased",
  "Vivant" = "Alive",
  "D\u00e9c\u00e9d\u00e9" = "Deceased"
)

FINAL_STATUS_MAP <- c(
  "DECEDE" = "Deceased"
)

SAMPLE_TYPE_MAP <- c(
  "SANG TOTAL" = "Whole blood",
  "SWAB" = "Swab",
  "Sang" = "Whole blood",
  "Cordon ombilical(Sang,Swab)" = "Umbilical cord (blood, swab)"
)

COLLECTION_EVENT_MAP <- c(
  "Nouveau Pr\u00e9l\u00e8vement" = "New collection",
  "Nouveau Prelevement" = "New collection",
  "R\u00e9-Pr\u00e9levement" = "Re-collection",
  "Re-Prelevement" = "Re-collection"
)

SEX_MAP <- c(
  "F" = "Female",
  "M" = "Male",
  "Femme" = "Female",
  "Homme" = "Male"
)

RESULT_VALUE_MAP <- c(
  "Positif" = "Positive",
  "Positif " = "Positive",
  "Negatif" = "Negative",
  "Négatif" = "Negative",
  "Invalide" = "Invalid",
  "Invalide " = "Invalid",
  "invalide" = "Invalid",
  "Indéterminé" = "Undetermined",
  "Indetermine" = "Undetermined"
)

zone_aliases <- c(
  mongbwalu = "Mongbalu",
  nyankunde = "Nyakunde",
  gety = "Gethy",
  makisokisangani = "Makiso-Kisangani",
  rwmapara = "Rwampara",
  mangurunjipa = "Manguredjipa",
  manguripa = "Manguredjipa"
)

normalize_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(trimws(iconv(x, to = "ASCII//TRANSLIT"))))
}

trim_names <- function(nms) {
  trimws(gsub("[\r\n]+", " ", iconv(nms, to = "UTF-8", sub = "byte")))
}

standardize_other_name <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("'", "", x, fixed = TRUE)
  x <- gsub("`", "", x, fixed = TRUE)
  x <- gsub("[^[:alnum: ]]", "", x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

find_explicit_rename <- function(old, mapping) {
  idx <- which(
    vapply(names(mapping), function(k) {
      normalize_key(k) == normalize_key(old)
    }, logical(1))
  )
  if (length(idx) == 0) {
    return(NA_character_)
  }
  unname(mapping[idx[1]])
}

translate_mapped_values <- function(x, mapping) {
  raw <- trimws(as.character(x))
  out <- raw
  out[raw == ""] <- NA_character_
  hits <- raw %in% names(mapping)
  out[hits] <- unname(mapping[raw[hits]])
  out[is.na(out)] <- ""
  out
}

format_iso_date <- function(x) {
  vals <- x
  out <- rep(NA_character_, length(vals))

  if (inherits(vals, "POSIXt")) {
    return(format(as.Date(as.POSIXct(vals)), "%Y-%m-%d"))
  }
  if (inherits(vals, "Date")) {
    return(format(vals, "%Y-%m-%d"))
  }

  raw <- trimws(as.character(vals))
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next

    parsed <- tryCatch(
      suppressWarnings(as.Date(val, tryFormats = c(
        "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d %H:%M:%S", "%Y%m%d"
      ))),
      error = function(e) NA
    )
    if (!is.na(parsed)) {
      out[i] <- format(parsed, "%Y-%m-%d")
      next
    }

    num <- suppressWarnings(as.numeric(val))
    if (!is.na(num) && num > 20000 && num < 120000) {
      dt <- tryCatch(as.Date(num, origin = "1899-12-30"), error = function(e) NA)
      if (!is.na(dt)) {
        out[i] <- format(dt, "%Y-%m-%d")
      }
    }
  }

  out
}

weeks_to_months <- function(weeks) {
  num <- suppressWarnings(as.numeric(weeks))
  if (is.na(num) || num <= 0) return(NA_integer_)
  as.integer(max(1, round(num * 12 / 52)))
}

days_to_months <- function(days) {
  num <- suppressWarnings(as.numeric(days))
  if (is.na(num) || num <= 0) return(0L)
  as.integer(min(11, round(num / 30)))
}

parse_ituri_age <- function(x) {
  raw <- trimws(as.character(x))
  n <- length(raw)
  age <- rep("", n)
  age_months <- rep("", n)
  under_one_year <- rep("", n)

  for (i in seq_len(n)) {
    val <- raw[i]
    if (is.na(val) || val == "") next

    if (grepl("mois", val, ignore.case = TRUE)) {
      num <- suppressWarnings(as.numeric(gsub("[^0-9.,]", "", gsub(",", ".", val))))
      age[i] <- "0"
      if (!is.na(num)) age_months[i] <- as.character(as.integer(num))
      under_one_year[i] <- "1"
      next
    }

    if (grepl("semaine", val, ignore.case = TRUE)) {
      num <- suppressWarnings(as.numeric(gsub("[^0-9.,]", "", gsub(",", ".", val))))
      age[i] <- "0"
      m <- weeks_to_months(num)
      if (!is.na(m)) age_months[i] <- as.character(m)
      under_one_year[i] <- "1"
      next
    }

    if (grepl("ans", val, ignore.case = TRUE)) {
      num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", val)))
      if (!is.na(num)) age[i] <- as.character(as.integer(num))
      next
    }

    num <- suppressWarnings(as.numeric(val))
    if (!is.na(num)) age[i] <- as.character(as.integer(num))
  }

  list(age = age, age_months = age_months, under_one_year = under_one_year)
}

parse_nk_age <- function(age_values, age_unit_values) {
  age_raw <- trimws(as.character(age_values))
  unit_label <- trimws(as.character(age_unit_values))
  unit_key <- toupper(unit_label)
  n <- length(age_raw)
  age <- ifelse(nzchar(age_raw), age_raw, "")
  age_months <- rep("", n)
  under_one_year <- rep("", n)

  for (i in seq_len(n)) {
    if (is.na(unit_key[i]) || unit_key[i] == "" || unit_key[i] == "ANS") {
      next
    }

    num <- suppressWarnings(as.numeric(age_raw[i]))
    age[i] <- "0"
    under_one_year[i] <- "1"

    if (unit_key[i] == "MOIS") {
      if (!is.na(num)) age_months[i] <- as.character(as.integer(num))
    } else if (unit_key[i] %in% c("JOURS", "JOUR")) {
      m <- days_to_months(num)
      age_months[i] <- as.character(m)
    }
  }

  list(age = age, age_months = age_months, under_one_year = under_one_year)
}

clean_special_characters <- function(x) {
  raw <- trimws(as.character(x))
  out <- rep("", length(raw))

  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next
    val <- iconv(val, from = "UTF-8", to = "ASCII//TRANSLIT")
    val <- gsub("'", "", val, fixed = TRUE)
    val <- gsub("`", "", val, fixed = TRUE)
    val <- gsub("[^[:alnum:][:space:]/\\-]", "", val)
    val <- gsub("[[:space:]]+", " ", val)
    out[i] <- trimws(val)
  }

  out
}

load_health_area_lookup <- function(csv_path) {
  if (!file.exists(csv_path)) {
    stop("Health area names file not found: ", csv_path, call. = FALSE)
  }

  ref <- read.csv(csv_path, stringsAsFactors = FALSE)
  if (!all(c("health_area", "health_area_cleaned") %in% names(ref))) {
    stop("health_area_names.csv must contain health_area and health_area_cleaned columns.", call. = FALSE)
  }

  raw <- trimws(ref$health_area)
  clean <- trimws(ref$health_area_cleaned)

  list(
    raw = raw,
    clean = clean,
    raw_lower = tolower(raw),
    clean_lower = tolower(clean)
  )
}

resolve_health_area_single <- function(x, lookup) {
  val <- trimws(as.character(x))
  if (is.na(val) || val == "") {
    return(list(value = "", matched = FALSE))
  }

  idx <- match(val, lookup$raw)
  if (!is.na(idx)) {
    return(list(value = lookup$raw[idx], matched = TRUE))
  }

  idx <- match(val, lookup$clean)
  if (!is.na(idx)) {
    return(list(value = lookup$raw[idx], matched = TRUE))
  }

  idx <- match(tolower(val), lookup$raw_lower)
  if (!is.na(idx)) {
    return(list(value = lookup$raw[idx], matched = TRUE))
  }

  idx <- match(tolower(val), lookup$clean_lower)
  if (!is.na(idx)) {
    return(list(value = lookup$raw[idx], matched = TRUE))
  }

  list(value = val, matched = FALSE)
}

resolve_health_area_value <- function(x, lookup) {
  val <- trimws(as.character(x))
  if (is.na(val) || val == "") {
    return(list(value = "", matched = FALSE))
  }

  if (!grepl(" / ", val, fixed = TRUE)) {
    return(resolve_health_area_single(val, lookup))
  }

  parts <- trimws(strsplit(val, " / ", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(list(value = "", matched = FALSE))
  }

  resolved <- lapply(parts, resolve_health_area_single, lookup = lookup)
  list(
    value = paste(vapply(resolved, function(r) r$value, character(1)), collapse = " / "),
    matched = all(vapply(resolved, function(r) r$matched, logical(1)))
  )
}

standardize_health_area_column <- function(x, lookup) {
  resolved <- lapply(x, resolve_health_area_value, lookup = lookup)
  list(
    value = vapply(resolved, function(r) r$value, character(1)),
    unmatched = !vapply(resolved, function(r) r$matched, logical(1))
  )
}

normalize_age_fields <- function(age, age_months) {
  age_num <- suppressWarnings(as.numeric(age))
  months_num <- suppressWarnings(as.numeric(age_months))
  n <- length(age_num)
  if (length(months_num) != n) {
    months_num <- rep(NA_real_, n)
  }

  for (i in seq_len(n)) {
    if (!is.na(months_num[i]) && months_num[i] > 12) {
      extra_years <- floor(months_num[i] / 12)
      remainder <- months_num[i] %% 12
      base_age <- if (is.na(age_num[i])) 0 else age_num[i]
      age_num[i] <- base_age + extra_years
      months_num[i] <- remainder
    }
  }

  age_num[!is.na(age_num) & age_num > 120] <- NA_real_

  age_out <- vapply(age_num, function(v) {
    if (is.na(v)) "" else as.character(as.integer(v))
  }, character(1))
  months_out <- vapply(months_num, function(v) {
    if (is.na(v)) "" else as.character(as.integer(v))
  }, character(1))

  list(age = age_out, age_months = months_out)
}

finalize_infant_age_fields <- function(age, age_months, under_one_year) {
  age_num <- suppressWarnings(as.numeric(age))
  months_out <- age_months
  uoy_out <- under_one_year
  for (i in seq_along(age_num)) {
    if (!is.na(age_num[i]) && age_num[i] >= 1) {
      months_out[i] <- ""
      uoy_out[i] <- ""
    }
  }
  list(age_months = months_out, under_one_year = uoy_out)
}

load_health_zone_lookup <- function(shp_path, alias_csv) {
  if (!file.exists(shp_path)) {
    stop("Shapefile not found: ", shp_path, call. = FALSE)
  }

  hz <- st_read(shp_path, quiet = TRUE)
  lookup <- stats::setNames(hz$Nom, vapply(hz$Nom, normalize_key, character(1)))

  if (file.exists(alias_csv)) {
    alias_df <- read.csv(alias_csv, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(alias_df))) {
      observed <- trimws(alias_df$observed_name[i])
      target <- trimws(alias_df$canonical_nom[i])
      if (nzchar(observed) && nzchar(target)) {
        lookup[[normalize_key(observed)]] <- target
      }
    }
  }

  for (alias_key in names(zone_aliases)) {
    lookup[[alias_key]] <- zone_aliases[[alias_key]]
  }

  lookup
}

match_health_zone <- function(x, lookup) {
  raw <- trimws(as.character(x))
  out <- rep(NA_character_, length(raw))
  pick <- function(key) {
    if (!nzchar(key) || !key %in% names(lookup)) {
      return(NA_character_)
    }
    unname(lookup[[key]])
  }
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next
    hit <- pick(normalize_key(val))
    if (!is.na(hit)) {
      out[i] <- hit
      next
    }
    title <- paste0(
      toupper(substring(val, 1, 1)),
      tolower(substring(val, 2))
    )
    hit <- pick(normalize_key(title))
    if (!is.na(hit)) out[i] <- hit
  }
  out
}

blank_or_na <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

normalize_result_token <- function(x) {
  tok <- tolower(trimws(as.character(x)))
  gsub("[^a-z]", "", iconv(tok, to = "ASCII//TRANSLIT"))
}

is_positive_token <- function(x) {
  !blank_or_na(x) & normalize_result_token(x) == "positif"
}

is_negative_token <- function(x) {
  !blank_or_na(x) & normalize_result_token(x) == "negatif"
}

is_invalid_token <- function(x) {
  !blank_or_na(x) & normalize_result_token(x) == "invalide"
}

classify_result_category <- function(x) {
  if (blank_or_na(x)) {
    return(NA_character_)
  }
  tok <- normalize_result_token(x)
  if (tok == "positif") {
    return("Positive")
  }
  if (tok == "negatif") {
    return("Negative")
  }
  if (tok == "invalide") {
    return("Invalid")
  }
  if (tok %in% c("indetermine", "indetermine")) {
    return("Undetermined")
  }
  "Unclassified"
}

results_agree <- function(a, b) {
  ca <- classify_result_category(a)
  cb <- classify_result_category(b)
  !is.na(ca) && !is.na(cb) && ca == cb
}

# Returns a data.frame with one row per input:
#   category : the consensus result category (as before)
#   source   : which assay drove the call, used to pick the corresponding Ct.
#              One of "kinshasa_altona", "bench_radione", "bunia_altona", or NA
#              (NA when no single assay determined the result: Undetermined or
#              Not yet run). The source is presence-based: it reflects which
#              assay was consulted, independent of whether that assay's value
#              classified cleanly.
compute_ituri_consensus <- function(resultat, resultat_altona, resultat_altona_bunia) {
  r <- trimws(as.character(resultat))
  a <- trimws(as.character(resultat_altona))
  b <- trimws(as.character(resultat_altona_bunia))
  n <- length(r)
  category <- rep(NA_character_, n)
  source   <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    if (!blank_or_na(a[i])) {
      source[i]   <- "kinshasa_altona"
      category[i] <- classify_result_category(a[i])
      if (is.na(category[i])) {
        category[i] <- "Unclassified"
      }
      next
    }

    has_r <- !blank_or_na(r[i])
    has_b <- !blank_or_na(b[i])

    if (has_r && !has_b) {
      source[i]   <- "bench_radione"
      category[i] <- classify_result_category(r[i])
    } else if (!has_r && has_b) {
      source[i]   <- "bunia_altona"
      category[i] <- classify_result_category(b[i])
    } else if (has_r && has_b) {
      if (results_agree(r[i], b[i])) {
        source[i]   <- "bench_radione"
        category[i] <- classify_result_category(r[i])
      } else {
        category[i] <- "Undetermined"   # disagreement: no single assay, source NA
      }
    } else {
      category[i] <- "Not yet run"       # neither present: source NA
    }

    if (is.na(category[i])) {
      category[i] <- "Unclassified"
    }
  }

  data.frame(category = category, source = source, stringsAsFactors = FALSE)
}

# Pick the consensus Ct from whichever assay drove the consensus result.
#   source             -> Ct column
#   "kinshasa_altona"  -> altona_ct_kinshasa
#   "bench_radione"    -> radione_ct
#   "bunia_altona"     -> altona_ct_bunia
#   NA                 -> NA
# Reported only for a Positive consensus (`category`): a Ct can be recorded even
# when the call is Negative/Undetermined, but consensus_ct is meant as the Ct of a
# confirmed positive, so non-positive rows are NA. Strict: the Ct comes only from
# the driving assay (no fallback); values coerced to numeric as-is (blank/
# non-numeric -> NA), no plausibility bounds. consensus_ct_source is the provenance
# of the reported Ct, so it is NA wherever consensus_ct is NA.
build_consensus_ct <- function(source, category, radione_ct,
                               altona_ct_bunia, altona_ct_kinshasa) {
  to_num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
  n   <- length(source)
  rad <- rep(to_num(radione_ct),         length.out = n)
  ab  <- rep(to_num(altona_ct_bunia),    length.out = n)
  ak  <- rep(to_num(altona_ct_kinshasa), length.out = n)

  ct <- rep(NA_real_, n)
  # `%in%` treats NA source as no-match, so NA source -> NA Ct.
  ct[source %in% "kinshasa_altona"] <- ak[source %in% "kinshasa_altona"]
  ct[source %in% "bench_radione"]   <- rad[source %in% "bench_radione"]
  ct[source %in% "bunia_altona"]    <- ab[source %in% "bunia_altona"]

  # Positivity gate: keep a Ct only for a Positive consensus.
  ct[!(category %in% "Positive")] <- NA_real_

  # Source describes where the reported Ct came from -> NA when there is no Ct.
  src_out <- source
  src_out[is.na(ct)] <- NA_character_

  data.frame(consensus_ct = ct, consensus_ct_source = src_out,
             stringsAsFactors = FALSE)
}

resolve_nk_symptom_onset_date <- function(onset_values, notification_values) {
  onset_raw <- trimws(as.character(onset_values))
  notif_raw <- trimws(as.character(notification_values))
  out <- rep(NA_character_, length(onset_raw))

  for (i in seq_along(onset_raw)) {
    val <- onset_raw[i]
    if (is.na(val) || val == "") {
      next
    }

    if (grepl("semaine", val, ignore.case = TRUE)) {
      weeks <- suppressWarnings(as.numeric(gsub("[^0-9]", "", val)))
      if (is.na(weeks)) {
        weeks <- 3
      }
      notif_iso <- format_iso_date(notif_raw[i])
      if (!is.na(notif_iso) && nzchar(notif_iso)) {
        out[i] <- format(as.Date(notif_iso) - (weeks * 7L), "%Y-%m-%d")
      }
      next
    }

    out[i] <- format_iso_date(val)
  }

  out
}

rename_columns <- function(df, explicit_mapping) {
  new_names <- names(df)
  for (i in seq_along(new_names)) {
    old <- new_names[i]
    if (old %in% c("age_months", "under_one_year")) {
      next
    }
    explicit <- find_explicit_rename(old, explicit_mapping)
    if (!is.na(explicit)) {
      new_names[i] <- explicit
      next
    }
    new_names[i] <- standardize_other_name(old)
  }
  make.unique(new_names, sep = "_")
}

apply_value_translations <- function(df) {
  for (col in intersect(c("sample_collection_status"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], COLLECTION_STATUS_MAP)
  }
  for (col in intersect(c("final_status"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], FINAL_STATUS_MAP)
  }
  for (col in intersect(c("sample_type"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], SAMPLE_TYPE_MAP)
  }
  for (col in intersect(c("collection_event_type"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], COLLECTION_EVENT_MAP)
  }
  for (col in intersect(c("sex"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], SEX_MAP)
  }
  for (col in intersect(
    c(
      "test_result", "altona_final_result", "altona_bunia_result",
      "genexpert_result", "positive_correction", "negative_correction",
      "final_result"
    ),
    names(df)
  )) {
    df[[col]] <- translate_mapped_values(df[[col]], RESULT_VALUE_MAP)
  }

  date_cols <- intersect(
    c(
      "date_of_notification", "date_of_death", "date_of_hospitalisation",
      "date_of_discharge", "date_of_sample_collection", "date_of_symptom_onset",
      "date_of_reception", "bunia_analysis_date", "altona_result_date",
      "kinshasa_analysis_date", "analysis_date"
    ),
    names(df)
  )
  for (col in date_cols) {
    df[[col]] <- format_iso_date(df[[col]])
  }

  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- ""
    }
  }

  df
}

write_processed_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(
    df,
    path,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "",
    qmethod = "escape"
  )
}

process_ituri_linelist <- function() {
  if (!file.exists(ITURI_XLSX)) {
    stop("Ituri workbook not found: ", ITURI_XLSX, call. = FALSE)
  }

  df <- read_excel(ITURI_XLSX, sheet = ITURI_SHEET)
  names(df) <- trim_names(names(df))

  drop_hits <- vapply(names(df), function(nm) {
    any(vapply(DROP_COLUMNS, function(d) normalize_key(nm) == normalize_key(d), logical(1)))
  }, logical(1))
  df <- df[, !drop_hits, drop = FALSE]

  raw_test_result <- if ("R\u00e9sultat" %in% names(df)) {
    trimws(as.character(df[["R\u00e9sultat"]]))
  } else {
    rep(NA_character_, nrow(df))
  }
  raw_altona_final <- if ("Resultat ALTONA" %in% names(df)) {
    trimws(as.character(df[["Resultat ALTONA"]]))
  } else {
    rep(NA_character_, nrow(df))
  }
  raw_altona_bunia <- if ("RESULTAT ALTONA BUNIA" %in% names(df)) {
    trimws(as.character(df[["RESULTAT ALTONA BUNIA"]]))
  } else {
    rep(NA_character_, nrow(df))
  }

  # Raw Ct columns, captured before rename (for consensus_ct).
  raw_radione_ct <- if ("RadiOne (Ct GP)" %in% names(df)) {
    df[["RadiOne (Ct GP)"]]
  } else {
    rep(NA, nrow(df))
  }
  # The sheet has two identical "Altona (FAM-EBOLA)" headers (Bunia block then
  # Kinshasa block); readxl disambiguates them positionally ("...26"/"...33").
  # Identify them by duplicate ORDER (robust to column add/remove), not suffix.
  .altona_base <- sub("\\.\\.\\.[0-9]+$", "", names(df))
  .altona_idx  <- which(.altona_base == "Altona (FAM-EBOLA)")
  if (length(.altona_idx) != 2L) {
    stop(sprintf(
      "Expected exactly 2 'Altona (FAM-EBOLA)' columns (Bunia, then Kinshasa); found %d.",
      length(.altona_idx)), call. = FALSE)
  }
  raw_altona_ct_bunia    <- df[[.altona_idx[1]]]
  raw_altona_ct_kinshasa <- df[[.altona_idx[2]]]

  age_col <- grep("^age$", names(df), ignore.case = TRUE, value = TRUE)[1]
  parsed_age <- parse_ituri_age(if (!is.na(age_col)) df[[age_col]] else rep(NA, nrow(df)))
  if (!is.na(age_col)) {
    df[[age_col]] <- parsed_age$age
  }
  df$age_months <- parsed_age$age_months
  df$under_one_year <- parsed_age$under_one_year

  names(df) <- rename_columns(df, ITURI_EXPLICIT_RENAMES)

  hz_lookup <- load_health_zone_lookup(shapefile_path, aliases_path)
  if ("health_zone" %in% names(df)) {
    df$health_zone <- match_health_zone(df$health_zone, hz_lookup)
  }

  if ("health_area" %in% names(df)) {
    df$health_area <- clean_special_characters(df$health_area)
    ha_lookup <- load_health_area_lookup(HEALTH_AREA_NAMES_PATH)
    ha_std <- standardize_health_area_column(df$health_area, ha_lookup)
    df$health_area <- ha_std$value
    df$health_area_unmatched <- ha_std$unmatched
  }

  if ("age" %in% names(df)) {
    age_months_col <- if ("age_months" %in% names(df)) df$age_months else rep("", nrow(df))
    uoy_col <- if ("under_one_year" %in% names(df)) df$under_one_year else rep("", nrow(df))
    normalized_age <- normalize_age_fields(df$age, age_months_col)
    df$age <- normalized_age$age
    finalized_age <- finalize_infant_age_fields(
      df$age, normalized_age$age_months, uoy_col
    )
    df$age_months <- finalized_age$age_months
    df$under_one_year <- finalized_age$under_one_year
  }

  cons <- compute_ituri_consensus(
    raw_test_result,
    raw_altona_final,
    raw_altona_bunia
  )
  df$consensus_test_result <- cons$category

  # Ct from whichever assay drove the consensus result.
  cct <- build_consensus_ct(
    cons$source,
    cons$category,
    raw_radione_ct,
    raw_altona_ct_bunia,
    raw_altona_ct_kinshasa
  )
  df$consensus_ct <- cct$consensus_ct
  df$consensus_ct_source <- cct$consensus_ct_source

  df <- apply_value_translations(df)

  age_cols <- intersect(c("age_months", "under_one_year"), names(df))
  if (length(age_cols) > 0) {
    col_order <- setdiff(names(df), age_cols)
    age_idx <- match("age", col_order)
    if (!is.na(age_idx)) {
      col_order <- append(col_order, age_cols, after = age_idx)
    } else {
      col_order <- c(col_order, age_cols)
    }
    df <- df[, col_order, drop = FALSE]
  }

  write_processed_csv(df, ITURI_OUTPUT)

  message("Ituri read: ", ITURI_XLSX, " (sheet: ", ITURI_SHEET, ")")
  message("Ituri wrote: ", ITURI_OUTPUT)
  message("Ituri rows: ", nrow(df), " | columns: ", ncol(df))
  if ("consensus_test_result" %in% names(df)) {
    message("Ituri consensus_test_result:")
    print(table(df$consensus_test_result, useNA = "ifany"))
  }
  if ("health_area_unmatched" %in% names(df)) {
    message("Ituri health_area_unmatched:")
    print(table(df$health_area_unmatched, useNA = "ifany"))
  }

  invisible(df)
}

process_nord_kivu_linelist <- function() {
  if (is.na(NK_XLSX) || !file.exists(NK_XLSX)) {
    stop("Nord-Kivu workbook not found in data/raw.", call. = FALSE)
  }

  header_raw <- read_excel(NK_XLSX, sheet = NK_SHEET, col_names = FALSE, n_max = NK_HEADER_ROW)
  header_names <- trim_names(as.character(unlist(header_raw[NK_HEADER_ROW, ])))
  header_names[is.na(header_names) | header_names == ""] <- paste0(
    "unnamed_", seq_along(header_names)
  )[is.na(header_names) | header_names == ""]

  df <- read_excel(
    NK_XLSX,
    sheet = NK_SHEET,
    skip = NK_HEADER_ROW,
    col_names = header_names
  )
  names(df) <- trim_names(names(df))

  id_col <- grep("ID labo", names(df), ignore.case = TRUE, value = TRUE)[1]
  result_col <- grep("^RESULTAT FINAL$", names(df), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(result_col)) {
    stop("RESULTAT FINAL column not found in Nord-Kivu export.", call. = FALSE)
  }

  keep <- rep(TRUE, nrow(df))
  if (!is.na(id_col)) {
    keep <- keep & !blank_or_na(df[[id_col]])
    keep <- keep & !grepl("^ID labo", df[[id_col]], ignore.case = TRUE)
  }
  keep <- keep & !grepl("^RESULTAT FINAL$", trimws(as.character(df[[result_col]])), ignore.case = TRUE)
  df <- df[keep, , drop = FALSE]

  raw_final_result <- trimws(as.character(df[[result_col]]))
  onset_col <- grep("Symptom", names(df), ignore.case = TRUE, value = TRUE)[1]
  notif_col <- grep("Notification", names(df), ignore.case = TRUE, value = TRUE)[1]
  raw_onset <- if (!is.na(onset_col)) trimws(as.character(df[[onset_col]])) else rep("", nrow(df))
  raw_notification <- if (!is.na(notif_col)) trimws(as.character(df[[notif_col]])) else rep("", nrow(df))

  names(df) <- rename_columns(df, NK_EXPLICIT_RENAMES)

  if ("age" %in% names(df) && "age_unit" %in% names(df)) {
    parsed_age <- parse_nk_age(df$age, df$age_unit)
    df$age <- parsed_age$age
    df$age_months <- parsed_age$age_months
    df$under_one_year <- parsed_age$under_one_year
  } else {
    df$age_months <- ""
    df$under_one_year <- ""
  }

  if ("age" %in% names(df)) {
    age_months_col <- if ("age_months" %in% names(df)) df$age_months else rep("", nrow(df))
    uoy_col <- if ("under_one_year" %in% names(df)) df$under_one_year else rep("", nrow(df))
    normalized_age <- normalize_age_fields(df$age, age_months_col)
    df$age <- normalized_age$age
    finalized_age <- finalize_infant_age_fields(
      df$age, normalized_age$age_months, uoy_col
    )
    df$age_months <- finalized_age$age_months
    df$under_one_year <- finalized_age$under_one_year
  }

  hz_lookup <- load_health_zone_lookup(shapefile_path, aliases_path)
  if ("health_zone" %in% names(df)) {
    df$health_zone <- match_health_zone(df$health_zone, hz_lookup)
  }

  if ("health_area" %in% names(df)) {
    df$health_area <- clean_special_characters(df$health_area)
    ha_lookup <- load_health_area_lookup(HEALTH_AREA_NAMES_PATH)
    ha_std <- standardize_health_area_column(df$health_area, ha_lookup)
    df$health_area <- ha_std$value
    df$health_area_unmatched <- ha_std$unmatched
  }

  if ("date_of_symptom_onset" %in% names(df)) {
    df$date_of_symptom_onset <- resolve_nk_symptom_onset_date(
      raw_onset,
      raw_notification
    )
  }

  df <- apply_value_translations(df)
  df$consensus_test_result <- translate_mapped_values(raw_final_result, RESULT_VALUE_MAP)

  age_cols <- intersect(c("age_months", "under_one_year"), names(df))
  if (length(age_cols) > 0) {
    col_order <- setdiff(names(df), age_cols)
    age_idx <- match("age", col_order)
    if (!is.na(age_idx)) {
      col_order <- append(col_order, age_cols, after = age_idx)
    } else {
      col_order <- c(col_order, age_cols)
    }
    df <- df[, col_order, drop = FALSE]
  }

  write_processed_csv(df, NK_OUTPUT)

  message("Nord-Kivu read: ", NK_XLSX, " (sheet: ", NK_SHEET, ")")
  message("Nord-Kivu wrote: ", NK_OUTPUT)
  message("Nord-Kivu rows: ", nrow(df), " | columns: ", ncol(df))
  message("Nord-Kivu columns with unclear Ituri matching:")
  for (nm in names(NK_UNCLEAR_COLUMNS)) {
    if (nm %in% names(df)) {
      message("  - ", nm, ": ", NK_UNCLEAR_COLUMNS[[nm]])
    }
  }
  if ("consensus_test_result" %in% names(df)) {
    message("Nord-Kivu consensus_test_result:")
    print(table(df$consensus_test_result, useNA = "ifany"))
  }
  if ("health_area_unmatched" %in% names(df)) {
    message("Nord-Kivu health_area_unmatched:")
    print(table(df$health_area_unmatched, useNA = "ifany"))
  }

  invisible(df)
}

# Run the pipeline only when executed as a script. Sourcing with
# options(linelist.skip_main = TRUE) loads the function definitions for testing
# without triggering a full (data-dependent) pipeline run.
if (!isTRUE(getOption("linelist.skip_main", FALSE))) {
  process_ituri_linelist()
  process_nord_kivu_linelist()
}
