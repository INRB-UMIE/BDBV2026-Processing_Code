#!/usr/bin/env Rscript
# Clean and standardise the DHIS2 MVE epidemic line list export.

# -------------------------  Check if Initialised  -----------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R", local = FALSE)
} else if (!setup_complete) {
  source("pipelines/0-setup.R", local = FALSE)
}


# ------------------------------  Functions  -----------------------------------

normalize_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(trimws(iconv(x, to = "ASCII//TRANSLIT"))))
}

trim_names <- function(nms) {
  trimws(gsub("[\r\n]+", " ", iconv(nms, to = "UTF-8", sub = "byte")))
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

strip_column_prefixes <- function(x) {
  x <- trimws(x)
  x <- gsub("^[0-9]+(\\.\\d+)?\\s*-\\s*", "", x)
  x <- gsub("^S[0-9]+\\s*-\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^MVE\\s*-\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^MVE-N-\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^MPOX-N-\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^LAB\\s*-\\s*", "", x, ignore.case = TRUE)
  x
}

standardize_column_name <- function(x) {
  x <- strip_column_prefixes(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  replacements <- c(
    "Date de rapportage" = "reporting_date",
    "Numero d'identification de l'alerte" = "alert_id",
    "Sexe" = "sex",
    "Age(ans)" = "age_years",
    "Age (Mois)" = "age_months",
    "Age < 1 an ?" = "under_one_year",
    "Nature de l'Alerte" = "type_of_alert",
    "Etat de sante actuel" = "current_health_status",
    "Symptomes" = "symptoms",
    "Personnel sante - position" = "health_worker_position",
    "Date de debut des symptomes" = "date_of_symptom_onset",
    "Date & Heure d'investigation" = "investigation_datetime",
    "Date d'hospitalisation/Date de consultation - Debut" = "date_of_hospitalisation_start",
    "Date d'hospitalisation/Date de consultation - Fin" = "date_of_hospitalisation_end",
    "Date et heure de notification de l'alerte" = "date_of_notification",
    "Cas suspects preleves" = "suspected_cases_collected",
    "Est-ce qu'un prelevement a deja ete soumis pour ce malade?" = "sample_already_submitted",
    "No Prelevement" = "sample_number",
    "Statut du patient lors du prelevement" = "sample_collection_status",
    "Date du prelevement" = "date_of_sample_collection",
    "Type de prelevement" = "sample_type",
    "Echantillons analyses" = "samples_analyzed",
    "Echantillons recus" = "samples_received",
    "Date d'analyse" = "lab_analysis_date",
    "Nom Laboratoire" = "lab_name",
    "Classification finale du cas" = "final_mve_case_classification",
    "GeneXpert Ebola Assay (resultat)" = "genexpert_result",
    "Date(s) du contact - Fin" = "contact_end_date",
    "Cas confirmes" = "confirmed_cases",
    "Lieu de Residence Permanente : Aire de Sante" = "permanent_residence_health_area",
    "Lieu de Residence Permanente : Village/Quartier" = "permanent_residence_village",
    "Organisation unit name hierarchy" = "org_unit_hierarchy"
  )
  for (pat in names(replacements)) {
    if (normalize_key(x) == normalize_key(pat)) {
      return(replacements[[pat]])
    }
  }
  x <- gsub("'", "", x, fixed = TRUE)
  x <- gsub("[^[:alnum: ]]", " ", x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

rename_columns <- function(df, explicit_mapping) {
  log_msg("RENAMING DHIS2 LINELIST COLUMNS")
  new_names <- names(df)
  renamed <- 0
  not_renamed <- c()
  for (i in seq_along(new_names)) {
    old <- new_names[i]
    explicit <- find_explicit_rename(old, explicit_mapping)
    if (!is.na(explicit)) {
      new_names[i] <- explicit
      renamed <- renamed + 1
      next
    }
    not_renamed <- append(not_renamed, old)
    new_names[i] <- standardize_column_name(old)
  }
  log_msg(paste0(renamed, " columns successfully renamed using explicit labels"))
  if (length(not_renamed > 0)) {
    log_msg(paste0("Some column names not found in label list: ", paste0(not_renamed, collapse = ", ")))
  }
  make.unique(new_names, sep = "_")
}

translate_sample_type <- function(x, label = NULL) {
  raw <- trimws(as.character(x))
  out <- rep("", length(raw))
  token_map <- c("S" = "blood", "EB" = "oral swab")
  unmatched_tokens <- character(0)

  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next
    parts <- strsplit(val, ",", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    translated <- vapply(parts, function(p) {
      if (p %in% names(token_map)) {
        unname(token_map[[p]])
      } else {
        unmatched_tokens <<- c(unmatched_tokens, p)
        tolower(p)
      }
    }, character(1))
    out[i] <- paste(translated, collapse = ", ")
  }

  if (!is.null(label)) {
    unmatched_tokens <- sort(unique(unmatched_tokens[nzchar(unmatched_tokens)]))
    if (length(unmatched_tokens) > 0) {
      # Quote each value (doubling any literal quote characters) so a value
      # that itself contains a comma can be told apart from the ", "
      # separator between *different* unmatched values when this line is
      # parsed back out in pipeline_run_report.Rmd.
      quoted <- paste0("'", gsub("'", "''", unmatched_tokens, fixed = TRUE), "'")
      log_msg(
        "Unmatched ", label, " tokens (", length(unmatched_tokens), " distinct): ",
        paste(quoted, collapse = ", ")
      )
    }
  }

  out
}

# `label` (optional, e.g. "sex (SEX_MAP)") logs the distinct raw values that
# had no entry in `mapping` — i.e. passed through untranslated — so gaps in
# data/maps/*.csv show up in the per-file log the same way unmatched
# health_zone/health_area values do.
translate_mapped_values <- function(x, mapping, label = NULL) {
  raw <- trimws(as.character(x))
  out <- raw
  out[raw == ""] <- NA_character_
  hits <- raw %in% names(mapping)
  out[hits] <- unname(mapping[raw[hits]])
  out[is.na(out)] <- ""

  if (!is.null(label)) {
    unmatched_vals <- sort(unique(raw[!hits & nzchar(raw)]))
    if (length(unmatched_vals) > 0) {
      # Quote each value (doubling any literal quote characters) so a value
      # that itself contains a comma (e.g. a DHIS2 multi-select field stored
      # as one raw string, like "INC, CON, MAR") can be told apart from the
      # ", " separator between *different* unmatched values when this line
      # is parsed back out in pipeline_run_report.Rmd.
      quoted <- paste0("'", gsub("'", "''", unmatched_vals, fixed = TRUE), "'")
      log_msg(
        "Unmatched ", label, " values (", length(unmatched_vals), " distinct): ",
        paste(quoted, collapse = ", ")
      )
    }
  }

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
  raw <- sub("\\.0$", "", raw)
  raw <- sub("[T ].*$", "", raw)
  
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next
    
    parsed <- tryCatch(
      suppressWarnings(as.Date(val, tryFormats = c(
        "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%OS"
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

#' Build a health-area name lookup from the GRID3 health areas reference
#' (columns: airesante = canonical name, asnom_alt = alternate name, plus
#' zonesante/province/etc — see data/geo/GRID3_COD_health_areas_v8/). Mirrors
#' load_health_zone_lookup()'s pattern: keyed by normalize_key() so matching is
#' robust to case/accent/punctuation differences, with asnom_alt layered on
#' top as an alias (same idea as zone_aliases for health zones). Replaces the
#' old health_area_names.csv (raw/cleaned spelling-correction table) — see
#' README for the handful of real-data names GRID3 doesn't cover.
#'
#' `aliases` (optional) is a named character vector keyed by
#' normalize_key(observed_name), same shape as zone_aliases — e.g. from
#' load_zone_aliases_csv(health_area_aliases_path, normalize_key). Lets a
#' reviewer manually map a health_area spelling GRID3 doesn't recognise
#' (e.g. "Ngadi") onto a canonical GRID3 name, without editing this function.
load_health_area_lookup <- function(csv_path, aliases = NULL) {
  if (!file.exists(csv_path)) {
    stop("Health area names file not found: ", csv_path, call. = FALSE)
  }

  ref <- read.csv(csv_path, stringsAsFactors = FALSE, colClasses = "character",
                  fileEncoding = "UTF-8")
  if (!all(c("airesante", "asnom_alt") %in% names(ref))) {
    stop("GRID3 health areas CSV must contain airesante and asnom_alt columns.", call. = FALSE)
  }

  primary <- trimws(ref$airesante)
  primary <- primary[nzchar(primary)]
  lookup <- stats::setNames(primary, vapply(primary, normalize_key, character(1)))

  alt <- trimws(ref$asnom_alt)
  has_alt <- nzchar(alt) & nzchar(trimws(ref$airesante))
  for (i in which(has_alt)) {
    lookup[[normalize_key(alt[i])]] <- trimws(ref$airesante[i])
  }

  for (alias_key in names(aliases)) {
    lookup[[alias_key]] <- aliases[[alias_key]]
  }

  lookup
}

resolve_health_area_single <- function(x, lookup) {
  val <- trimws(as.character(x))
  if (is.na(val) || val == "") {
    return(list(value = "", matched = FALSE))
  }

  key <- normalize_key(val)
  if (nzchar(key) && key %in% names(lookup)) {
    return(list(value = unname(lookup[[key]]), matched = TRUE))
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

clean_geo_prefixes <- function(x, level = c("province", "health_zone", "health_area")) {
  level <- match.arg(level)
  out <- trimws(as.character(x))
  out[is.na(out)] <- ""
  out <- gsub("^[a-z]{2}[-[:space:]]+", "", out, ignore.case = TRUE)
  if (level == "province") {
    out <- gsub("[[:space:]]*Province[[:space:]]*$", "", out, ignore.case = TRUE)
  } else if (level == "health_zone") {
    out <- gsub("[[:space:]]*Zone de Sant[eé][[:space:]]*$", "", out, ignore.case = TRUE)
  } else {
    out <- gsub("[[:space:]]*Aire de Sant[eé][[:space:]]*$", "", out, ignore.case = TRUE)
  }
  trimws(out)
}

format_province_label <- function(x) {
  out <- rep("", length(x))
  for (i in seq_along(x)) {
    val <- trimws(x[i])
    if (!nzchar(val)) next
    val_key <- tolower(iconv(val, to = "ASCII//TRANSLIT"))
    if (grepl("nord[[:space:]]*kivu", val_key)) {
      out[i] <- "Nord-Kivu"
      next
    }
    if (grepl("sud[[:space:]]*kivu", val_key)) {
      out[i] <- "Sud-Kivu"
      next
    }
    if (grepl("kinshasa", val_key)) {
      out[i] <- "Kinshasa"
      next
    }
    if (grepl("^ituri$", val_key)) {
      out[i] <- "Ituri"
      next
    }
    parts <- strsplit(val_key, "[[:space:]-]+")[[1]]
    parts <- parts[nzchar(parts)]
    out[i] <- paste(
      vapply(parts, function(p) paste0(toupper(substring(p, 1, 1)), substring(p, 2)), character(1)),
      collapse = "-"
    )
  }
  out
}

split_org_hierarchy <- function(x) {
  raw <- trimws(as.character(x))
  country <- province <- health_zone <- health_area <- rep("", length(raw))
  
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next
    parts <- strsplit(val, " / ", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    if (length(parts) >= 1) {
      country[i] <- if (grepl("congo", parts[1], ignore.case = TRUE)) {
        "Democratic Republic of the Congo"
      } else {
        parts[1]
      }
    }
    if (length(parts) >= 2) {
      province[i] <- clean_geo_prefixes(parts[2], "province")
    }
    if (length(parts) >= 3) {
      health_zone[i] <- clean_geo_prefixes(parts[3], "health_zone")
    }
    if (length(parts) >= 4) {
      cleaned_areas <- vapply(parts[4:length(parts)], clean_geo_prefixes, character(1), level = "health_area")
      health_area[i] <- paste(cleaned_areas[nzchar(cleaned_areas)], collapse = " / ")
    }
  }
  
  data.frame(
    country = country,
    province = format_province_label(province),
    health_zone = health_zone,
    health_area = clean_special_characters(health_area),
    stringsAsFactors = FALSE
  )
}

load_health_zone_lookup <- function(shp_path, alias_csv) {
  if (!file.exists(shp_path)) {
    stop("Shapefile not found: ", shp_path, call. = FALSE)
  }
  
  hz <- st_read(shp_path, quiet = TRUE)
  # GRID3_COD_health_zones_v8 stores the zone's own name in `zonesante`
  # (matching the `zonesante` column in the GRID3 health-areas CSV), not
  # `Nom` — the field name used by the older DRC_Health_zones.shp schema
  # (still used by the CT and lab pipelines' own copy of this function).
  if (!"zonesante" %in% names(hz)) {
    stop("Expected 'zonesante' column not found in shapefile: ", shp_path, call. = FALSE)
  }
  lookup <- stats::setNames(hz$zonesante, vapply(hz$zonesante, normalize_key, character(1)))

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

# DHIS2 org-unit names carry a 2-letter province code prefix (e.g. "kn Gombe",
# "it Bunia", "ks Luebo"). Strip a leading 2-letter lowercase code so the bare
# zone name can be matched against the reference / retained as a fallback. Names
# that are already clean (Title-case, e.g. "Bunia", "Kasa Vubu") are untouched.
strip_province_prefix <- function(x) {
  sub("^[a-z]{2}[[:space:]]+", "", trimws(as.character(x)))
}

# Resolve health-zone names against the reference lookup. Mirrors the
# health_area handling (standardize_health_area_column): a matched zone is
# replaced with its canonical name; an UNMATCHED zone keeps its (prefix-stripped)
# raw value and is flagged, rather than being silently blanked. Returns a list of
# parallel vectors: value + unmatched.
match_health_zone <- function(x, lookup) {
  raw <- strip_province_prefix(x)
  value <- rep("", length(raw))
  unmatched <- rep(FALSE, length(raw))
  pick <- function(key) {
    if (!nzchar(key) || !key %in% names(lookup)) {
      return(NA_character_)
    }
    unname(lookup[[key]])
  }
  for (i in seq_along(raw)) {
    val <- raw[i]
    if (is.na(val) || val == "") next  # genuinely-absent zone: "" and not flagged
    hit <- pick(normalize_key(val))
    if (is.na(hit)) {
      title <- paste0(
        toupper(substring(val, 1, 1)),
        tolower(substring(val, 2))
      )
      hit <- pick(normalize_key(title))
    }
    if (!is.na(hit)) {
      value[i] <- hit
    } else {
      value[i] <- val        # symmetric fallback: keep the cleaned raw value
      unmatched[i] <- TRUE
    }
  }
  list(value = value, unmatched = unmatched)
}

apply_value_translations <- function(df) {
  for (col in intersect(c("sex"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], SEX_MAP, label = paste0(col, " (SEX_MAP)"))
  }
  for (col in intersect(c("sample_collection_status"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], COLLECTION_STATUS_MAP, label = paste0(col, " (COLLECTION_STATUS_MAP)"))
  }
  for (col in intersect(c("genexpert_result"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], RESULT_VALUE_MAP, label = paste0(col, " (RESULT_VALUE_MAP)"))
  }
  for (col in intersect(c("final_mve_case_classification"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], CASE_CLASSIFICATION_MAP, label = paste0(col, " (CASE_CLASSIFICATION_MAP)"))
  }
  for (col in intersect(c("type_of_alert"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], TYPE_OF_ALERT_MAP, label = paste0(col, " (TYPE_OF_ALERT_MAP)"))
  }
  for (col in intersect(c("current_health_status"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], HEALTH_STATUS_MAP, label = paste0(col, " (HEALTH_STATUS_MAP)"))
  }
  for (col in intersect(c("health_worker_position"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], HEALTH_WORKER_POSITION_MAP, label = paste0(col, " (HEALTH_WORKER_POSITION_MAP)"))
  }
  for (col in intersect(c("under_one_year", "sample_already_submitted"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], YES_NO_MAP, label = paste0(col, " (YES_NO_MAP)"))
  }
  for (col in intersect(c("sample_type"), names(df))) {
    df[[col]] <- translate_sample_type(df[[col]], label = col)
  }
  
  date_cols <- intersect(
    c(
      "reporting_date", "date_of_symptom_onset", "investigation_datetime",
      "date_of_hospitalisation_start", "date_of_hospitalisation_end",
      "date_of_hospitalisation_end_2", "date_of_notification",
      "date_of_sample_collection", "lab_analysis_date", "contact_end_date"
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

blank_or_na <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
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

# --------------------------  alert_id validation  ------------------------------
# Expected structure:  RDC-<PPP>-<HHH>-26-<NNNNNN|NNNNNNN>-MVE
#   PPP  three-letter province code   (e.g. ITU, NKV, HUE)
#   HHH  three-letter health-zone code (e.g. BUN, MON, RWA)
#   26   outbreak year
#   N..  a six OR seven digit serial
#   MVE  fixed suffix
#
# BOTH serial widths are legitimate: DHIS2 emitted six digits early in the
# outbreak and seven from July 2026 onward (in LINELIST_01092026, 7,885 rows use
# six and 26,765 use seven). The two are NOT the same numbers zero-padded
# differently — 17 serials occur in both widths and every one is a different
# person (RDC-AAA-BBB-26-012345-MVE and RDC-AAA-BBB-26-0012345-MVE are two
# unrelated Bunia cases). The serial is therefore treated as an OPAQUE STRING
# and leading zeros are never stripped; doing so would merge unrelated cases.
ALERT_ID_RE <- "^RDC-[A-Z]{3}-[A-Z]{3}-26-[0-9]{6,7}-MVE$"

# Load the province/health_zone -> alert-code reference table. Columns:
# province, province_code, health_zone, zone_code, source_dataset, notes.
# Derived from the modal code observed per zone pooled across every processed
# run (129 zones; only Tshopo/Kabondo was at all ambiguous, KAB 27 of 30).
# Maintained by hand thereafter — add a row when a new zone appears.
load_alert_code_lookup <- function(csv_path) {
  if (!file.exists(csv_path)) {
    log_msg("  NOTE: alert-code reference not found (", csv_path,
            ") — zone-code consistency check skipped.")
    return(NULL)
  }
  ref <- utils::read.csv(csv_path, stringsAsFactors = FALSE, na.strings = "")
  needed <- c("province", "province_code", "health_zone", "zone_code")
  missing <- setdiff(needed, names(ref))
  if (length(missing) > 0) {
    stop("alert-code reference is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  ref <- ref[!blank_or_na(ref$health_zone) & !blank_or_na(ref$zone_code), , drop = FALSE]
  list(
    zone_code = stats::setNames(toupper(trimws(ref$zone_code)),
                                normalize_key(ref$health_zone)),
    prov_code = stats::setNames(toupper(trimws(ref$province_code)),
                                normalize_key(ref$province))
  )
}

# Cosmetic-only normalisation: strip whitespace, uppercase, collapse repeated
# and non-ASCII dashes. Never touches the serial's digits.
tidy_alert_id <- function(x) {
  out <- toupper(trimws(as.character(x)))
  # Excel and Word habitually substitute typographic dashes for the hyphen.
  # Listed explicitly by code point rather than as a character-class range, so
  # this does not depend on the collation order of the running locale.
  # "+" and "=" share a key with "-", so they are a slip for one, as in the
  # CT pipeline's ct_tidy_alert_id(). "," and ";" are not: they more likely
  # separate two IDs.
  for (dash in c("‐", "‑", "‒", "–", "—",
                 "―", "−", "－", "+", "=")) {
    out <- gsub(dash, "-", out, fixed = TRUE)
  }
  out <- gsub("[[:space:]]+", "", out)
  out <- gsub("-{2,}", "-", out)
  out
}

# Validate (and where unambiguously safe, repair) the alert_id column.
# Returns a list of: value (possibly repaired), original, valid, issue.
#
# Repairs applied — both deterministic, neither invents a serial:
#   1. cosmetic tidying (case, stray whitespace, unicode dashes)
#   2. reconstructing an "RDC-PPP-HHH" prefix that DHIS2 replaced with numeric
#      org-unit codes (e.g. "12345678-12-26-0001234-MVE", 5 rows in
#      LINELIST_01092026, all Ituri/Tchomia points of entry). Only applied when
#      the row's own province and health_zone both resolve in the reference AND
#      the rebuilt ID does not collide with an existing one.
#
# Everything else is flagged and left exactly as found: blank IDs, unrecognised
# shapes, and rows whose embedded zone code disagrees with their health_zone.
validate_alert_ids <- function(alert_id, province, health_zone, lookup) {
  n <- length(alert_id)
  original <- as.character(alert_id)
  value <- tidy_alert_id(original)
  issue <- rep(NA_character_, n)

  prov_key <- normalize_key(province)
  zone_key <- normalize_key(health_zone)
  exp_prov <- if (!is.null(lookup)) unname(lookup$prov_code[prov_key]) else rep(NA_character_, n)
  exp_zone <- if (!is.null(lookup)) unname(lookup$zone_code[zone_key]) else rep(NA_character_, n)

  is_blank <- blank_or_na(value)
  issue[is_blank] <- "missing"

  # --- repair 2: rebuild a mangled prefix, keeping the "-26-<serial>-MVE" tail
  tail_re <- "-26-([0-9]{6,7})-MVE$"
  broken <- !is_blank & !grepl(ALERT_ID_RE, value) & grepl(tail_re, value)
  if (any(broken)) {
    serial <- sub(paste0(".*", tail_re), "\\1", value)
    rebuilt <- paste0("RDC-", exp_prov, "-", exp_zone, "-26-", serial, "-MVE")
    fixable <- broken & !is.na(exp_prov) & !is.na(exp_zone) &
      grepl(ALERT_ID_RE, rebuilt) & !(rebuilt %in% value[!is_blank])
    if (any(fixable)) {
      for (i in which(fixable)) {
        log_msg("  REPAIRED alert_id prefix: ", value[i], " -> ", rebuilt[i],
                "  (province=", province[i], ", health_zone=", health_zone[i], ")")
      }
      issue[fixable] <- "prefix_rebuilt_from_geo"
      value[fixable] <- rebuilt[fixable]
    }
    unfixable <- broken & !fixable
    issue[unfixable] <- "malformed_prefix_not_repairable"
  }

  bad_shape <- !is_blank & is.na(issue) & !grepl(ALERT_ID_RE, value)
  issue[bad_shape] <- "unrecognised_structure"

  ok <- grepl(ALERT_ID_RE, value)

  # --- zone code embedded in the ID vs the row's own health_zone
  got_zone <- ifelse(ok, substr(value, 9, 11), NA_character_)
  zone_bad <- ok & !is.na(exp_zone) & got_zone != exp_zone
  issue[zone_bad & is.na(issue)] <- "zone_code_mismatch"

  got_prov <- ifelse(ok, substr(value, 5, 7), NA_character_)
  prov_bad <- ok & !is.na(exp_prov) & got_prov != exp_prov
  issue[prov_bad & is.na(issue)] <- "province_code_mismatch"

  unknown_zone <- ok & is.na(exp_zone) & !blank_or_na(health_zone)
  issue[unknown_zone & is.na(issue)] <- "zone_not_in_code_reference"

  # --- duplicated IDs (two rows claiming the same individual)
  dup <- ok & value %in% value[ok][duplicated(value[ok])]
  issue[dup & is.na(issue)] <- "duplicate_alert_id"

  list(
    value    = value,
    original = ifelse(value != original & !is.na(original), original, NA_character_),
    valid    = ok & (is.na(issue) | issue == "prefix_rebuilt_from_geo"),
    issue    = issue,
    serial_width = ifelse(ok, nchar(sub(paste0(".*", tail_re), "\\1", value)), NA_integer_)
  )
}

# ==================  Assigned IDs for rows with no alert_id  ==================
#
# 558 rows in LINELIST_01092026 arrive with no alert_id at all (90% Nord-Kivu /
# Beni). They are real people — all 558 have a name and a reporting_date, 21
# carry a case classification and 4 are confirmed cases — but every ID-keyed
# mechanism skips them, `first_seen_date` most visibly.
#
# So they are given an ID of our own. The hard part is that a fresh export has
# no memory of what we assigned last time, so the same person must land on the
# same ID every run without us storing anything in the export itself.
#
# HOW A ROW IS RECOGNISED. By a fingerprint over five fields, chosen because
# they were measured never to change. Tracking every blank-ID row across all 51
# processed exports gave 18,184 comparisons of the same row in consecutive
# files, and these five differed zero times:
#
#     name, sex, age, reporting_date, date_of_notification
#
# Fields deliberately excluded because they DO move: type_of_alert (466
# changes), current_health_status (52), health_area (4), classification (3),
# health_zone (2). Including any of them would rename people between exports.
#
# The same five fields also separate people cleanly: on LINELIST_01092026 the
# 558 blank rows produced 558 distinct fingerprints, none of which collided
# with a row that already had a real alert_id.
#
# WHY THE FINGERPRINT IS NOT ITSELF THE ID. Three reasons:
#   1. Nine rows have started blank and later been given a real DHIS2 ID. A
#      stored registry can record that; a value recomputed from the row cannot
#      remember anything.
#   2. Those five fields have been stable so far. If a correction ever lands —
#      a misspelled name fixed — a derived ID would silently rename the person,
#      making a new case appear and an old one vanish with no error anywhere.
#      With a registry a changed field means the lookup fails, which is visible.
#   3. Privacy. The name is in the fingerprint, so a hash of it, travelling
#      into the de-identified export the way alert_id does, could be tested
#      against a list of candidate names to re-identify people. A sequential
#      number leaks nothing.
#
# NOTE: the registry file contains patient names and must be handled exactly
# like dhis2_processed_linelist_names.csv.
ASSIGNED_ID_PREFIX <- "LL-"
ASSIGNED_ID_RE     <- "^LL-[0-9]{6}$"

assigned_id_registry_path <- function(processed_dir) {
  file.path(processed_dir, "assigned_alert_ids.csv")
}

# Deterministic accent folding. NOT iconv(to = "ASCII//TRANSLIT"), which is
# supplied by the platform C library and is not portable: glibc renders
# "Gisèle" as "Gisele" while macOS commonly yields "Gis?le". A fingerprint is
# the wrong place to tolerate that — the same person would fingerprint
# differently on two analysts' machines and be issued two different assigned
# IDs, which is precisely the failure this whole mechanism exists to prevent.
# (Same map as translit_ascii() in 2-deduplicate_linelist.R.)
.ASSIGN_TRANSLIT_FROM <- paste0(
  "àáâãäåèéêë", "ìíîïòóôõöù", "úûüýÿçñðþ",
  "ÀÁÂÃÄÅÈÉÊË", "ÌÍÎÏÒÓÔÕÖÙ", "ÚÛÜÝÇÑÐÞ"
)
.ASSIGN_TRANSLIT_TO <- paste0(
  "aaaaaaeeee", "iiiiooooou", "uuuyycndt",
  "AAAAAAEEEE", "IIIIOOOOOU", "UUUYCNDT"
)

assign_translit_ascii <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("ß", "ss", x, fixed = TRUE)
  x <- gsub("æ", "ae", x, fixed = TRUE)
  x <- gsub("Æ", "AE", x, fixed = TRUE)
  x <- gsub("œ", "oe", x, fixed = TRUE)
  x <- gsub("Œ", "OE", x, fixed = TRUE)
  x <- chartr(.ASSIGN_TRANSLIT_FROM, .ASSIGN_TRANSLIT_TO, x)
  gsub("[^ -~]", "", x)
}

# Fingerprint of one row. Order-insensitive on the name, since name order
# varies between exports.
assigned_fingerprint <- function(name, sex, age, reporting_date, notification_date) {
  nm <- assign_translit_ascii(tolower(trimws(as.character(name))))
  nm <- gsub("[^a-z0-9 ]", " ", nm)
  nm <- vapply(strsplit(trimws(nm), "[[:space:]]+"), function(tk) {
    tk <- tk[nzchar(tk)]
    if (!length(tk)) return("")
    paste0(sort(tk), collapse = " ")
  }, character(1))

  tidy <- function(x) {
    out <- tolower(trimws(as.character(x)))
    out[is.na(out)] <- ""
    out
  }
  paste(nm, tidy(sex), tidy(age), tidy(reporting_date), tidy(notification_date),
        sep = "|")
}

# Registry schema.
#
# The five fingerprint source fields (name, sex, age, reporting_date,
# date_of_notification) are deliberately NOT stored as separate columns — the
# fingerprint is the key and the individual values add nothing the linelist
# does not already hold.
#
# NOTE, though: `fingerprint` still begins with the patient's name in plain
# text, so this file remains PII and must be handled like
# dhis2_processed_linelist_names.csv. Dropping the separate columns removes
# redundancy, not identifiability. See ASSIGNED_ID_HASH_FINGERPRINT below if
# that needs to change.
ASSIGNED_ID_REGISTRY_COLUMNS <- c(
  "assigned_id",             # LL-nnnnnn, sequential, never reissued
  "fingerprint",             # the matching key (contains the name — see above)
  "first_assigned_run",      # export in which this ID was first handed out
  "num_linelist_rows",       # rows carrying this assigned ID in counts_from_run
  "counts_from_run",         # which export num_linelist_rows describes
  "superseded_by_dhis2_id",  # real ID, once DHIS2 supplies one
  "superseded_in_run"
)

load_assigned_id_registry <- function(processed_dir) {
  path <- assigned_id_registry_path(processed_dir)
  cols <- ASSIGNED_ID_REGISTRY_COLUMNS
  if (!file.exists(path)) {
    empty <- as.data.frame(
      setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    )
    return(empty)
  }
  reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character")

  # Refuse a registry written under the earlier schema.
  #
  # That version stored the five fingerprint fields as columns AND built the
  # fingerprint from the RAW export values ("Masculin", "2026-05-04
  # 00:00:00.0") because assignment ran before the value maps. The fingerprint
  # is now built from cleaned values, so none of the old keys would match:
  # loading such a file would quietly issue every person a second ID and
  # double the registry. Better to stop and be told.
  #
  # Safe as a one-off because no assigned ID has been used downstream yet.
  # Once they are in circulation, changing the fingerprint definition
  # invalidates the registry — so treat the field list as frozen from here on.
  stale <- intersect(c("name", "sex", "age", "reporting_date",
                       "date_of_notification", "linelist_alert_ids"),
                     names(reg))
  wrong_prefix <- if ("assigned_id" %in% names(reg) && nrow(reg) > 0) {
    !all(grepl(ASSIGNED_ID_RE, reg$assigned_id))
  } else {
    FALSE
  }
  if (length(stale) > 0 || wrong_prefix) {
    stop("Assigned-ID registry uses a superseded schema.\n",
         if (length(stale) > 0)
           paste0("  Found retired column(s): ", paste(stale, collapse = ", "),
                  ".\n") else "",
         if (wrong_prefix)
           paste0("  Its IDs do not match the current pattern (",
                  ASSIGNED_ID_RE, ") — e.g. ", reg$assigned_id[1], ".\n") else "",
         "  Stored fingerprints or IDs cannot be matched against the current ",
         "ones, so loading it would issue every person a second ID.\n",
         "  Delete it and let this run rebuild it:\n    ", path,
         "\n  (Safe while no assigned ID has been used downstream. Once they ",
         "have been, neither the fingerprint fields nor the ID prefix may change.)",
         call. = FALSE)
  }

  for (cl in setdiff(cols, names(reg))) reg[[cl]] <- NA_character_
  reg[, cols, drop = FALSE]
}

save_assigned_id_registry <- function(reg, processed_dir) {
  ord <- order(suppressWarnings(as.numeric(sub(ASSIGNED_ID_PREFIX, "", reg$assigned_id))))
  write_processed_csv(reg[ord, , drop = FALSE], assigned_id_registry_path(processed_dir))
}

# Assign IDs to rows whose alert_id is blank.
#
# Returns the updated alert_id vector, the updated registry, and per-row issue
# labels. Never touches a row that already has an alert_id.
assign_missing_alert_ids <- function(alert_id, name, sex, age, reporting_date,
                                     notification_date, reg, run_stem) {
  n <- length(alert_id)
  out <- as.character(alert_id)
  assigned <- rep(NA_character_, n)

  blank <- blank_or_na(out)
  if (!any(blank)) return(list(value = out, registry = reg, assigned = assigned,
                               n_new = 0L, n_reused = 0L))

  # A row cannot be given an ID without a name — there would be nothing to
  # recognise it by next time.
  nameless <- blank & blank_or_na(name)
  usable   <- blank & !nameless

  fp <- assigned_fingerprint(name, sex, age, reporting_date, notification_date)

  known <- stats::setNames(reg$assigned_id, reg$fingerprint)
  next_num <- if (nrow(reg)) {
    max(suppressWarnings(as.numeric(sub(ASSIGNED_ID_PREFIX, "", reg$assigned_id))),
        na.rm = TRUE) + 1L
  } else {
    1L
  }
  if (!is.finite(next_num)) next_num <- 1L

  n_new <- 0L; n_reused <- 0L
  new_rows <- list()

  for (i in which(usable)) {
    # Single bracket, deliberately: `known` is an atomic named vector, and
    # known[["a name that isn't there"]] raises "subscript out of bounds"
    # rather than returning NA. Single-bracket indexing yields NA, which is
    # what the miss case needs — and a miss is the common case on a first run.
    hit <- unname(known[fp[i]])
    if (!is.na(hit)) {
      out[i] <- hit
      assigned[i] <- "assigned_id_reused"
      n_reused <- n_reused + 1L
      next
    }
    new_id <- sprintf("%s%06d", ASSIGNED_ID_PREFIX, next_num)
    next_num <- next_num + 1L
    known[fp[i]] <- new_id   # `[<-` by name extends the vector; see note above
    out[i] <- new_id
    assigned[i] <- "assigned_id_new"
    n_new <- n_new + 1L
    new_rows[[length(new_rows) + 1L]] <- data.frame(
      assigned_id = new_id, fingerprint = fp[i],
      first_assigned_run = run_stem,
      num_linelist_rows = NA_character_, counts_from_run = NA_character_,
      superseded_by_dhis2_id = NA_character_, superseded_in_run = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  assigned[nameless] <- "no_alert_id_and_no_name"

  if (length(new_rows)) reg <- rbind(reg, do.call(rbind, new_rows))
  list(value = out, registry = reg, assigned = assigned,
       n_new = n_new, n_reused = n_reused)
}

# Rewrite the _new/_reused half of a row label to mean "this export introduced
# the ID", not "this pass of the script minted it".
#
# Those are not the same thing, and the difference showed up as a bug. On
# LINELIST_01092026 all 558 assigned rows read `assigned_id_reused` while
# assigned_alert_ids.csv recorded all 558 as first issued in that very export.
# The cause is that assignment is idempotent: reprocess an export whose IDs are
# already registered and every fingerprint is found, so every row is labelled
# "reused" however new the ID actually is. The column therefore described the
# last pass rather than the data.
#
# `first_assigned_run` is the durable answer, so the label is derived from it.
# An ID missing from the registry keeps whatever label it arrived with, rather
# than silently reading as reused — that should not happen, and quietly
# relabelling it would hide the fact that it did.
#
# The CT pipeline has the same helper as ct_relabel_by_first_run(); they are
# kept separate because each script must run on its own.
relabel_assigned_by_first_run <- function(label, ids, reg, run_stem) {
  if (!length(label)) return(label)
  touched <- !is.na(label) & grepl("_(new|reused)$", label)
  if (!any(touched)) return(label)

  # Single bracket: `first` is an atomic named vector, and a miss must give NA
  # rather than the "subscript out of bounds" that [[ ]] raises.
  first <- stats::setNames(reg$first_assigned_run, reg$assigned_id)
  fr    <- unname(first[ids[touched]])

  relabelled <- paste0(sub("_(new|reused)$", "", label[touched]),
                       ifelse(!is.na(fr) & fr == run_stem, "_new", "_reused"))
  relabelled[is.na(fr)] <- label[touched][is.na(fr)]
  label[touched] <- relabelled
  label
}

# Sort key for a LINELIST_DDMMYYYY stem: "YYYYMMDD", so stems compare
# chronologically as plain strings. raw_files comes from list.files(), which is
# alphabetical — LINELIST_01092026 sorts before LINELIST_02072026 — so "the
# last file processed" is NOT "the most recent export". Anything recording
# per-export state has to compare dates rather than rely on loop order.
assigned_run_sortkey <- function(stem) {
  d <- regmatches(stem, regexpr("[0-9]{8}$", stem))
  if (!length(d) || !nzchar(d)) return("")
  paste0(substr(d, 5, 8), substr(d, 3, 4), substr(d, 1, 2))
}

# Record, per registry entry, how many rows of this export carry that assigned
# ID.
#
# Counted on the ASSIGNED ID, not on the fingerprint, because that is the
# question the column answers: how many linelist rows were given this ID. For
# someone DHIS2 has since supplied a real ID for, the answer is 0 — no row
# carries the assigned ID any more — which pairs with superseded_by_dhis2_id
# being populated.
#
# Only overwrites when this export is at least as recent as whatever the
# entry's count already describes, so reprocessing an old file cannot roll the
# figures backwards. That matters because the file loop is alphabetical rather
# than chronological.
update_assigned_id_row_counts <- function(reg, final_alert_ids, run_stem) {
  if (!nrow(reg)) return(reg)

  # Named integer vector, indexed with a SINGLE bracket below: a table is an
  # atomic array, so counts[["an id that isn't there"]] raises "subscript out
  # of bounds" rather than returning NA — and "not in this export" is a normal
  # case here.
  cnt_tab <- table(final_alert_ids[grepl(ASSIGNED_ID_RE, final_alert_ids)])
  counts <- stats::setNames(as.integer(cnt_tab), names(cnt_tab))

  this_key <- assigned_run_sortkey(run_stem)
  stored_key <- vapply(reg$counts_from_run, function(s) {
    if (is.na(s) || !nzchar(s)) "" else assigned_run_sortkey(s)
  }, character(1), USE.NAMES = FALSE)

  for (k in seq_len(nrow(reg))) {
    if (nzchar(stored_key[k]) && this_key < stored_key[k]) next
    n <- unname(counts[reg$assigned_id[k]])
    # Absent from this export: record 0 rather than leave a stale count, so
    # the file always describes one identifiable export.
    reg$num_linelist_rows[k] <- as.character(if (is.na(n)) 0L else n)
    reg$counts_from_run[k] <- run_stem
  }
  reg
}

# A row that once had an assigned ID and now carries a real DHIS2 one: the real
# ID wins in the data (nothing downstream links the two). The supersession is
# recorded in the registry only, so the same fingerprint is never issued a
# second assigned ID if it goes blank again.
record_superseded_assigned_ids <- function(reg, fingerprints, alert_ids, run_stem) {
  if (!nrow(reg)) return(reg)
  idx <- match(reg$fingerprint, fingerprints)
  hit <- !is.na(idx) & is.na(reg$superseded_by_dhis2_id)
  if (!any(hit)) return(reg)
  real <- alert_ids[idx[hit]]
  ok <- !blank_or_na(real) & !grepl(ASSIGNED_ID_RE, real)
  if (!any(ok)) return(reg)
  target <- which(hit)[ok]
  reg$superseded_by_dhis2_id[target] <- real[ok]
  reg$superseded_in_run[target] <- run_stem
  for (k in target) {
    log_msg("  assigned ID superseded: ", reg$assigned_id[k], " -> ",
            reg$superseded_by_dhis2_id[k])
  }
  reg
}

write_processed_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(
    df,
    path,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = "",
    qmethod = "double"
  )
}

# ------------------------------  Logging  --------------------------------------
# log_msg()/log_print() are drop-in replacements for message()/print(): they
# still print (so the Shiny app's live log window is unaffected — it reads
# both stdout and stderr), but also record the same text so it can be saved to
# disk. Two logs are kept: a per-file one, reset at the start of each raw file
# and written as <stem>_processing_log.txt
# (covers renaming, health-zone/health-area/map matching incl. unmatched
# values, the cleaning report, etc — everything about that one file), and a
# whole-run one, accumulated across every file plus the pipeline-level steps
# that happen before/after the per-file loop (latest.json, first_seen_date),
# written once at the end. Both go to this run's logs/ folder.
.file_log_lines <- character(0)
.run_log_lines  <- character(0)

log_msg <- function(...) {
  txt <- paste0(...)
  message(txt)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) lines <- ""
  .file_log_lines <<- c(.file_log_lines, lines)
  .run_log_lines  <<- c(.run_log_lines, lines)
}

log_print <- function(x) {
  out <- utils::capture.output(print(x))
  message(paste(out, collapse = "\n"))
  .file_log_lines <<- c(.file_log_lines, out)
  .run_log_lines  <<- c(.run_log_lines, out)
}

# Both go to this run's logs/<YYYY-MM-DD_HHMMSS>/ folder (log_run_dir() in
# 0-setup.R): the per-file one as <stem>_processing_log.txt, the whole-run one
# as epi_linelist_run_log.txt.
write_file_log <- function(stem) {
  write_log_file(.file_log_lines, paste0(stem, "_processing_log.txt"))
}

write_run_log <- function() {
  if (!length(.run_log_lines)) return(invisible(NULL))
  write_log_file(.run_log_lines, "epi_linelist_run_log.txt")
}


# --------------------------- Column Names and Maps ----------------------------
DHIS2_EXPLICIT_RENAMES <- load_map_csv("dhis2_epi_column_renames")

# Columns containing personally identifiable information. The list lives in
# pipeline_configs.json (pii_columns.epi_linelist) so every script and report
# shares one copy. dhis2_processed_linelist_names.csv keeps them -- it is the
# full table, and the only per-export copy apart from the ctsafe allowlist.
# Anything that must not see PII reads _names.csv and drops these on load.
NAME_COLUMNS <- unlist(pipeline_configs$pii_columns$epi_linelist)
if (!length(NAME_COLUMNS))
  stop("pipeline_configs.json has no pii_columns.epi_linelist list", call. = FALSE)

# Columns specifically kept in the contact tracing data.
# dhis2_processed_linelist_ctsafe.csv has these and removes all others (safe for git).
# permanent_residence_health_area/village + their _unmatched flag are included
# deliberately: they are not in NAME_COLUMNS (not treated as PII by this
# script) and are needed downstream by the epi_linelist_pipeline steps 5-6
# (residence mapping / residence-vs-alert report) that now run in
# BDBV2026-Contact_Tracing_Analysis against this ctsafe export.
CT_COLUMNS <- c('country', 'province', 'health_zone', 'health_zone_unmatched',
                'health_area', 'health_area_unmatched',
                'permanent_residence_health_area', 'permanent_residence_health_area_unmatched',
                'permanent_residence_village',
                'reporting_date', 'alert_id',
                # dhis2_alert_id and alert_id_assigned travel downstream on
                # purpose: alert_id may now hold an ID we assigned ourselves
                # (LL-nnnnnn), so anything that parses alert_id for a
                # province or zone code needs to be able to tell the two apart.
                # Additive columns, so existing consumers are unaffected.
                'dhis2_alert_id', 'alert_id_assigned',
                'sex', 'age', 'current_health_status',
                'date_of_sample_collection', 'first_seen_date',
                'type_of_alert',
                'symptoms', 'health_worker_position', 'date_of_symptom_onset',
                'investigation_datetime', 'date_of_hospitalisation_start',
                'date_of_hospitalisation_end', 'date_of_hospitalisation_end_2',
                'date_of_notification',
                # Renamed 2026-10-30 (data/maps/dhis2_epi_column_renames.csv);
                # source_of_alert added then. The Altona CY5 column was
                # "CT C45" until 5 Sep 2026 and was dropped from this export
                # from then until this change; both now map to one name.
                'source_of_alert',
                'vaccine_status',
                'sample_number', 'sample_collection_status',
                'sample_type',
                'lab_analysis_date',
                'lab_name', 'testing_machine_used', 'laboratory_identifier',
                # PCR cycle-threshold (Ct) values: pcr_ct_<kit>_<channel>[_<target>]
                'pcr_ct_radione_fam_ebov', 'pcr_ct_radione_hex_ic',
                'pcr_ct_altona_fam', 'pcr_ct_altona_hex', 'pcr_ct_altona_cy5',
                # Added 2026-10-30. positive / negative / invalid
                # (RESULT_VALUE_MAP); filled on <1% of rows.
                'genexpert_result',
                'final_mve_case_classification',
                'contact_end_date')


SEX_MAP <- load_map_csv("sex_map")

COLLECTION_STATUS_MAP <- load_map_csv("collection_status_map")

RESULT_VALUE_MAP <- load_map_csv("result_value_map")

CASE_CLASSIFICATION_MAP <- load_map_csv("case_classification_map")

TYPE_OF_ALERT_MAP <- load_map_csv("type_of_alert_map")

HEALTH_STATUS_MAP <- load_map_csv("health_status_map")

HEALTH_WORKER_POSITION_MAP <- load_map_csv("health_worker_position_map")

YES_NO_MAP <- load_map_csv("yes_no_map")


# ------------------- Get specific config and set other paths ------------------
epi_cfg <- pipeline_configs$epi_linelist
raw_dir <- file.path(repo_root, epi_cfg$data$raw_subdir)
processed_dir <- file.path(repo_root, epi_cfg$data$processed_subdir)
shapefile_path <- file.path(repo_root, "data", "geo", "GRID3_COD_health_zones_v8", "GRID3_COD_health_zones_v8_0.shp")
aliases_path <- file.path(repo_root, "data", "aliases", "health_zone_aliases.csv")
health_area_names_path <- file.path(repo_root, "data", "geo", "GRID3_COD_health_areas_v8", "GRID3_COD_health_areas_v8_0.csv")
health_area_aliases_path <- file.path(repo_root, "data", "aliases", "health_area_aliases.csv")
alert_code_ref_path <- file.path(repo_root, "data", "aliases", "health_zone_alert_codes.csv")

# zone_aliases used to be a hardcoded named vector; now loaded from the same
# observed_name/canonical_nom alias table used as load_health_zone_lookup()'s
# alias_csv argument (data/aliases/health_zone_aliases.csv), keyed by
# normalize_key(observed_name) to match how it's consumed there.
zone_aliases <- load_zone_aliases_csv(aliases_path, normalize_key)
log_msg("Loaded ", length(zone_aliases), " zone_aliases entries from ", aliases_path)

# Same idea as zone_aliases, but for health_area names GRID3 doesn't
# recognise. Starts empty (header row only) — add rows to
# data/aliases/health_area_aliases.csv as unmatched names are reviewed.
health_area_aliases <- load_zone_aliases_csv(health_area_aliases_path, normalize_key)
log_msg("Loaded ", length(health_area_aliases), " health_area_aliases entries from ", health_area_aliases_path)

# province/health_zone -> expected alert_id code, for the alert_id structure check
alert_code_lookup <- load_alert_code_lookup(alert_code_ref_path)
if (!is.null(alert_code_lookup)) {
  log_msg("Loaded ", length(alert_code_lookup$zone_code),
          " health-zone alert-code entries from ", alert_code_ref_path)
}

# Registry of IDs we assigned ourselves to rows the export gave no alert_id.
# Loaded once here, updated inside the per-file loop, written once at the end —
# so a run processing several exports keeps one continuous numbering.
assigned_id_registry <- load_assigned_id_registry(processed_dir)
log_msg("Loaded ", nrow(assigned_id_registry), " assigned alert_id entries from ",
        assigned_id_registry_path(processed_dir))

# ---- Discover all raw linelist files (convention: LINELIST_DDMMYYYY.*) ----
raw_files <- list.files(raw_dir,
  pattern     = "\\.(xlsx|xls|csv)$",
  full.names  = TRUE,
  ignore.case = TRUE
)
raw_files <- raw_files[grepl("^LINELIST_\\d{8}\\.", basename(raw_files), ignore.case = TRUE)]
if (length(raw_files) == 0) stop("No raw linelist files found in: ", raw_dir, call. = FALSE)

# If the Shiny app set dhis2_force_stems, restrict processing to only those files.
# (Unselected files are left untouched, whether processed or not.)
if (exists("dhis2_force_stems") && length(dhis2_force_stems) > 0) {
  all_stems <- tools::file_path_sans_ext(basename(raw_files))
  keep      <- all_stems %in% dhis2_force_stems
  if (!any(keep)) stop("None of the selected stems found in raw dir: ",
                       paste(dhis2_force_stems, collapse = ", "), call. = FALSE)
  raw_files <- raw_files[keep]
  log_msg("Processing ", sum(keep), " selected file(s): ",
          paste(tools::file_path_sans_ext(basename(raw_files)), collapse = ", "))
}

processed_dhis2_linelist <- NULL   # will hold the latest processed df

# Stems whose processed CSVs this run (re)wrote. The first_seen_date pass at
# the end only rescans from the earliest of these onwards, so a folder that was
# deleted and reprocessed -- e.g. to pick up an alert_id repair -- must be
# listed here even though its stem is already in the first-seen cache.
first_seen_touched <- character(0)

for (raw_file in raw_files) {
  current_stem       <- tools::file_path_sans_ext(basename(raw_file))
  current_output_dir <- file.path(processed_dir, current_stem)
  DHIS2_XLSX         <- raw_file
  # The full table, PII included. Its existence is also what marks an export
  # as processed (here, in the Shiny app's file lists, and in first_seen below).
  DHIS2_OUTPUT       <- file.path(current_output_dir, "dhis2_processed_linelist_names.csv")
  .file_log_lines    <- character(0)  # reset per-file log for this raw file

  if (file.exists(DHIS2_OUTPUT)) {
    log_msg("Skipping ", current_stem, " — already processed.")
    next
  }
  dir.create(current_output_dir, recursive = TRUE, showWarnings = FALSE)
  log_msg("\n=== Processing: ", current_stem, " ===")
  tryCatch({

# ------------------------- Process DHIS2 Linelist -----------------------------
ext <- tolower(tools::file_ext(raw_file))
df <- if (ext == "csv") {
  read.csv(DHIS2_XLSX, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
} else {
  read_excel(DHIS2_XLSX, sheet = 1)
}

# Tidy names using trim_names()
names(df) <- trim_names(names(df))

# Find the hierarchy col which has the location info
hierarchy_col <- grep("^Organisation unit name hierarchy$", names(df), value = TRUE)[1]
if (is.na(hierarchy_col)) {
  stop("Organisation unit name hierarchy column not found.", call. = FALSE)
}

# Make geo df with location split into components
log_msg("SPLITTING LOCATION INTO COMPONENTS")
geo <- split_org_hierarchy(df[[hierarchy_col]])
df <- df[, setdiff(names(df), hierarchy_col), drop = FALSE]
df <- cbind(geo, df)

# Rename columns to the encoded renames
names(df) <- rename_columns(df, DHIS2_EXPLICIT_RENAMES)

# Check if there are any cols to drop
drop_cols <- vapply(names(df), function(nm) {
  grepl("^\\.{3}|^unnamed_|^[0-9]+$", nm) || all(blank_or_na(df[[nm]]))
}, logical(1))
df <- df[, !drop_cols, drop = FALSE]

# Match health zones to shapefile
log_msg("MATCHING HEALTHZONES TO SHAPEFILE")
hz_lookup <- load_health_zone_lookup(shapefile_path, aliases_path)
if ("health_zone" %in% names(df)) {
  hz_std <- match_health_zone(df$health_zone, hz_lookup)
  df$health_zone <- hz_std$value
  df$health_zone_unmatched <- hz_std$unmatched
}
log_msg("MATCHING HEALTH AREAS TO SHAPEFILE")
if ("health_area" %in% names(df) || "permanent_residence_health_area" %in% names(df)) {
  ha_lookup <- load_health_area_lookup(health_area_names_path, health_area_aliases)
  if ("health_area" %in% names(df)) {
    ha_std <- standardize_health_area_column(df$health_area, ha_lookup)
    df$health_area <- ha_std$value
    df$health_area_unmatched <- ha_std$unmatched
  }
  if ("permanent_residence_health_area" %in% names(df)) {
    df$permanent_residence_health_area <- clean_special_characters(
      clean_geo_prefixes(df$permanent_residence_health_area, "health_area")
    )
    pr_std <- standardize_health_area_column(df$permanent_residence_health_area, ha_lookup)
    df$permanent_residence_health_area <- pr_std$value
    df$permanent_residence_health_area_unmatched <- pr_std$unmatched
  }
}

# Validate alert_id structure (after geo cleaning, so province/health_zone are
# already standardised and can be used both to repair prefixes and to check the
# codes embedded in the ID).
log_msg("VALIDATING ALERT_ID STRUCTURE")
if ("alert_id" %in% names(df)) {
  prov_vec <- if ("province" %in% names(df)) df$province else rep(NA_character_, nrow(df))
  hz_vec   <- if ("health_zone" %in% names(df)) df$health_zone else rep(NA_character_, nrow(df))

  # dhis2_alert_id preserves the value exactly as the export supplied it,
  # including blanks, for every row. alert_id then becomes OUR canonical value:
  # repaired where it was damaged, and assigned where the export had none. The
  # pair is what makes both questions answerable — "what did DHIS2 send us" and
  # "which individual is this row".
  df$dhis2_alert_id <- as.character(df$alert_id)

  aid <- validate_alert_ids(df$alert_id, prov_vec, hz_vec, alert_code_lookup)

  df$alert_id       <- aid$value
  df$alert_id_valid <- aid$valid
  df$alert_id_issue <- aid$issue

  log_msg("  Expected structure: RDC-<PPP>-<HHH>-26-<6 or 7 digits>-MVE")
  log_msg("  Conforming: ", sum(aid$valid), " of ", nrow(df),
          " (", sprintf("%.1f%%", 100 * sum(aid$valid) / nrow(df)), ")")
  log_msg("  Serial width (both widths are valid — see ALERT_ID_RE comment):")
  log_print(table(aid$serial_width, useNA = "ifany"))

  if (any(!is.na(aid$issue))) {
    log_msg("  alert_id issues:")
    log_print(table(aid$issue, useNA = "no"))
    # Name the actual offending values, not just counts — same convention as
    # the health-zone/health-area unmatched logging below.
    for (iss in sort(unique(stats::na.omit(aid$issue)))) {
      if (iss == "missing") {
        log_msg("    missing: ", sum(aid$issue == "missing", na.rm = TRUE),
                " row(s) with no alert_id. By health_zone:")
        log_print(table(hz_vec[which(aid$issue == "missing")]))
        next
      }
      if (iss == "prefix_rebuilt_from_geo") next  # already logged per row above
      rows_i <- which(aid$issue == iss)
      log_msg("    ", iss, " (", length(rows_i), " row(s)): ",
              paste(utils::head(unique(df$alert_id[rows_i]), 25), collapse = ", "),
              if (length(unique(df$alert_id[rows_i])) > 25) ", ..." else "")
      if (iss %in% c("zone_code_mismatch", "province_code_mismatch")) {
        log_print(unique(data.frame(
          alert_id    = df$alert_id[rows_i],
          province    = prov_vec[rows_i],
          health_zone = hz_vec[rows_i],
          stringsAsFactors = FALSE
        )))
      }
    }
  } else {
    log_msg("  No alert_id issues found.")
  }

  # A laboratory_identifier should be unique per sample; a shared one is a
  # separate data-quality problem from the alert_id itself, so it is reported
  # here rather than folded into alert_id_issue.
  if ("laboratory_identifier" %in% names(df)) {
    lab <- df$laboratory_identifier
    lab_present <- !blank_or_na(lab)
    lab_dup <- lab_present & lab %in% lab[lab_present][duplicated(lab[lab_present])]
    if (any(lab_dup)) {
      log_msg("  WARNING: ", length(unique(lab[lab_dup])),
              " laboratory_identifier value(s) shared by more than one row (",
              sum(lab_dup), " rows affected): ",
              paste(utils::head(sort(unique(lab[lab_dup])), 15), collapse = ", "),
              if (length(unique(lab[lab_dup])) > 15) ", ..." else "")
    }
  }

} else {
  log_msg("  alert_id column not present — skipping.")
}


# Tidy ages
log_msg("TIDYING AGE FIELD")
if ("age" %in% names(df)) {
  age_months_col <- if ("age_months" %in% names(df)) df$age_months else rep(NA, nrow(df))
  parsed_age <- normalize_age_fields(df$age, age_months_col)
  df$age <- parsed_age$age
  if ("age_months" %in% names(df)) {
    df$age_months <- parsed_age$age_months
  }
}

# Tidy ages
log_msg("TIDYING OTHER COLUMNS USING PROVIDED MAPS")
df <- apply_value_translations(df)

# The assignment step below deliberately sits AFTER apply_value_translations()
# and the age tidying, not with the validation above. The fingerprint must be
# built from CLEANED values -- "male" not "Masculin", "2026-05-04" not
# "2026-05-04 00:00:00.0" -- because those are the values whose stability was
# measured (zero changes across 18,184 comparisons). Fingerprinting the raw
# export values instead would make every ID hostage to the export's date
# format and to the value maps in data/maps/.
if ("alert_id" %in% names(df)) {
  # ---- Assign our own ID to rows the export gave none -----------------------
  # Runs after validation (so genuinely repairable IDs have already been fixed
  # and are not mistaken for missing ones) and before the first_seen_date pass
  # at the end of this script, which is keyed on alert_id and therefore only
  # covers these rows once they have one.
  log_msg("ASSIGNING IDs TO ROWS WITH NO ALERT_ID")
  name_vec <- if ("name" %in% names(df)) df$name else rep(NA_character_, nrow(df))
  rep_vec  <- if ("reporting_date" %in% names(df)) df$reporting_date else rep(NA_character_, nrow(df))
  not_vec  <- if ("date_of_notification" %in% names(df)) df$date_of_notification else rep(NA_character_, nrow(df))
  sex_vec  <- if ("sex" %in% names(df)) df$sex else rep(NA_character_, nrow(df))
  age_vec  <- if ("age" %in% names(df)) df$age else rep(NA_character_, nrow(df))

  if (all(blank_or_na(name_vec))) {
    log_msg("  No patient names in this export — cannot recognise rows across ",
            "exports, so no IDs assigned. ", sum(blank_or_na(df$alert_id)),
            " row(s) left without one.")
    df$alert_id_assigned <- NA_character_
  } else {
    # Before assigning, note any fingerprint we have seen before that now
    # carries a real DHIS2 ID. The real ID wins in the data; the registry keeps
    # the supersession so this fingerprint is never issued a second ID.
    fp_all <- assigned_fingerprint(name_vec, sex_vec, age_vec, rep_vec, not_vec)
    assigned_id_registry <- record_superseded_assigned_ids(
      assigned_id_registry, fp_all, df$alert_id, current_stem
    )

    asn <- assign_missing_alert_ids(df$alert_id, name_vec, sex_vec, age_vec,
                                    rep_vec, not_vec, assigned_id_registry,
                                    current_stem)
    df$alert_id <- asn$value
    assigned_id_registry <- asn$registry
    df$alert_id_assigned <- relabel_assigned_by_first_run(
      asn$assigned, df$alert_id, assigned_id_registry, current_stem
    )

    still_blank <- sum(blank_or_na(df$alert_id))
    log_msg("  Newly assigned: ", asn$n_new,
            " | reused from the registry: ", asn$n_reused,
            " | still without an ID: ", still_blank)
    if (still_blank > 0) {
      log_msg("  (Rows with neither an alert_id nor a name cannot be given one — ",
              "there would be nothing to recognise them by next export.)")
    }
    # How many rows of this export carry each assigned ID.
    assigned_id_registry <- update_assigned_id_row_counts(
      assigned_id_registry, df$alert_id, current_stem
    )

    multi <- suppressWarnings(as.numeric(assigned_id_registry$num_linelist_rows)) > 1
    multi[is.na(multi)] <- FALSE
    if (any(multi)) {
      log_msg("  ", sum(multi), " assigned ID(s) cover more than one linelist row ",
              "(same person recorded twice — see alert_id_duplicated): ",
              paste(utils::head(assigned_id_registry$assigned_id[multi], 10),
                    collapse = ", "))
    }

    log_msg("  Registry now holds ", nrow(assigned_id_registry), " assigned ID(s); ",
            sum(!blank_or_na(assigned_id_registry$superseded_by_dhis2_id)),
            " since superseded by a real DHIS2 ID.")
  }

  # ---- Flag any alert_id now shared by more than one row --------------------
  # The fingerprint is a PERSON key, not a row key, so two rows the export left
  # blank that agree on all five stable fields are the same person and get the
  # same assigned ID. That is the truthful answer, and it means alert_id is not
  # unique for those rows.
  #
  # This is not a new situation: alert_id was already non-unique —
  # RDC-AAA-BBB-26-0001234-MVE arrives on two rows of LINELIST_01092026. So
  # duplicated assigned IDs are treated exactly like duplicated DHIS2 ones,
  # via one flag computed on the FINAL alert_id after assignment.
  #
  # Distinct from alert_id_issue's "duplicate_alert_id", which describes what
  # DHIS2 sent. alert_id_duplicated describes our canonical value, so it also
  # catches a collision created by a repair or an assignment.
  final_ids <- df$alert_id
  present   <- !blank_or_na(final_ids)
  df$alert_id_duplicated <- present &
    final_ids %in% final_ids[present][duplicated(final_ids[present])]

  if (any(df$alert_id_duplicated)) {
    dup_ids <- sort(unique(final_ids[df$alert_id_duplicated]))
    n_assigned_dup <- sum(df$alert_id_duplicated & grepl(ASSIGNED_ID_RE, final_ids))
    log_msg("  WARNING: ", length(dup_ids), " alert_id value(s) appear on more ",
            "than one row (", sum(df$alert_id_duplicated), " rows affected",
            if (n_assigned_dup > 0) paste0(", ", n_assigned_dup,
                                           " of them assigned IDs") else "",
            "): ", paste(utils::head(dup_ids, 15), collapse = ", "),
            if (length(dup_ids) > 15) ", ..." else "")
    log_msg("    Two rows sharing an assigned ID agree on name, sex, age, ",
            "reporting date and notification date, so they are the same ",
            "person recorded twice — a duplicate to review, not an ID fault.")
  }
}


# Reorder columns
log_msg("REORDERING COLUMNS")
geo_cols <- c("country", "province", "health_zone", "health_zone_unmatched", "health_area", "health_area_unmatched")
pr_cols <- c("permanent_residence_health_area", "permanent_residence_health_area_unmatched")
remaining <- setdiff(names(df), c(geo_cols, pr_cols))
vill_idx <- match("permanent_residence_village", remaining)
col_order <- if (!is.na(vill_idx)) {
  c(
    geo_cols,
    remaining[seq_len(vill_idx - 1)],
    pr_cols,
    remaining[vill_idx:length(remaining)]
  )
} else {
  c(geo_cols, remaining, pr_cols)
}
df <- df[, col_order, drop = FALSE]

# Keep the alert_id companions beside alert_id rather than stranded at the end,
# since they are read together. (first_seen_date positions itself the same way,
# in the pass at the end of this script.)
aid_companions <- c("dhis2_alert_id", "alert_id_valid", "alert_id_issue",
                    "alert_id_assigned", "alert_id_duplicated")
aid_companions <- aid_companions[aid_companions %in% names(df)]
if ("alert_id" %in% names(df) && length(aid_companions)) {
  rest    <- setdiff(names(df), aid_companions)
  aid_at  <- match("alert_id", rest)
  df <- df[, c(rest[seq_len(aid_at)], aid_companions,
               rest[setdiff(seq_along(rest), seq_len(aid_at))]), drop = FALSE]
}


# Write output: two files per export.
#   dhis2_processed_linelist_names.csv   the full table, PII included (always
#                                        written, even if this export has no
#                                        names in it, so every export has one)
#   dhis2_processed_linelist_ctsafe.csv  the CT_COLUMNS allowlist, safe to share
# There used to be a third, "no names" copy (dhis2_processed_linelist.csv).
# It was dropped: it was the full table minus NAME_COLUMNS, so anything that
# needs it can drop those columns from _names.csv on load.
log_msg("WRITING PROCESSED CSV")
pii_present <- NAME_COLUMNS[NAME_COLUMNS %in% names(df)]
cols_to_keep_ct <- CT_COLUMNS[CT_COLUMNS %in% names(df)]

write_processed_csv(df, DHIS2_OUTPUT)
log_msg("  Full table written to ", basename(DHIS2_OUTPUT), " (PII columns present: ",
        if (length(pii_present)) paste(pii_present, collapse = ", ") else "none", ")")

# A no-names copy left over from before the change above would otherwise sit
# beside the new output looking current. Remove it.
legacy_plain <- file.path(current_output_dir, "dhis2_processed_linelist.csv")
if (file.exists(legacy_plain)) {
  unlink(legacy_plain)
  log_msg("  Removed old no-names copy: ", basename(legacy_plain))
}
# first_seen_date is NOT in df yet -- it needs every export, so it is filled in
# by the pass at the end of this script, which rewrites the ctsafe file too.
df_ct_safe <- df[, intersect(cols_to_keep_ct, names(df)), drop = FALSE]
write_processed_csv(df_ct_safe, file.path(current_output_dir, "dhis2_processed_linelist_ctsafe.csv"))
first_seen_touched <- c(first_seen_touched, current_stem)

# Write output
log_msg("\n-------CLEANING REPORT------")
log_msg("DHIS2 read: ", DHIS2_XLSX)
log_msg("DHIS2 wrote: ", DHIS2_OUTPUT)
log_msg("Rows: ", nrow(df), " | columns: ", ncol(df))
if ("province" %in% names(df)) {
  log_msg("Provinces: ", paste(sort(unique(df$province[df$province != ""])), collapse = ", "))
}
if ("type_of_alert" %in% names(df)) {
  log_msg("type_of_alert:")
  log_print(table(df$type_of_alert, useNA = "ifany"))
}
if ("health_zone_unmatched" %in% names(df)) {
  log_msg("health_zone_unmatched:")
  log_print(table(df$health_zone_unmatched, useNA = "ifany"))
  unmatched_hz <- sort(unique(df$health_zone[df$health_zone_unmatched & nzchar(df$health_zone)]))
  log_msg(
    "Unmatched health_zone values (", length(unmatched_hz), " distinct): ",
    paste(unmatched_hz, collapse = ", ")
  )
}
if ("health_area_unmatched" %in% names(df)) {
  log_msg("health_area_unmatched:")
  log_print(table(df$health_area_unmatched, useNA = "ifany"))
  unmatched_ha <- sort(unique(df$health_area[df$health_area_unmatched & nzchar(df$health_area)]))
  log_msg(
    "Unmatched health_area values (", length(unmatched_ha), " distinct): ",
    paste(unmatched_ha, collapse = ", ")
  )
}
if ("permanent_residence_health_area_unmatched" %in% names(df)) {
  log_msg("permanent_residence_health_area_unmatched:")
  log_print(table(df$permanent_residence_health_area_unmatched, useNA = "ifany"))
  unmatched_prha <- sort(unique(df$permanent_residence_health_area[
    df$permanent_residence_health_area_unmatched &
      nzchar(df$permanent_residence_health_area)
  ]))
  log_msg(
    "Unmatched permanent_residence_health_area values (", length(unmatched_prha), " distinct): ",
    paste(unmatched_prha, collapse = ", ")
  )
}

# Store the processed df in a variable that will remain in the env; it is
# reloaded below if this isn't the latest file.
processed_dhis2_linelist <- df

  write_file_log(current_stem)

  }, error = function(e) {
    log_msg("\n!!! ERROR processing ", current_stem, ": ", conditionMessage(e),
            "\n    Skipping this file and continuing.")
    write_file_log(current_stem)
    # Remove the incomplete output so it won't be treated as processed on the
    # next run. The log is kept: it is in logs/, not in this folder.
    if (dir.exists(current_output_dir) && !file.exists(DHIS2_OUTPUT)) {
      unlink(current_output_dir, recursive = TRUE)
    }
  }) # end tryCatch

} # ---- end for (raw_file in raw_files) ----


# ---- Persist the assigned-ID registry --------------------------------------
# Written once per run rather than per file, so processing several exports in
# one go produces one continuous numbering. Saved even if some file errored,
# because IDs already handed out must never be reissued to someone else.
if (exists("assigned_id_registry") && nrow(assigned_id_registry) > 0) {
  save_assigned_id_registry(assigned_id_registry, processed_dir)
  log_msg("\nAssigned-ID registry saved: ", nrow(assigned_id_registry),
          " entries -> ", assigned_id_registry_path(processed_dir))
  log_msg("  NOTE: this file contains patient names. Handle it exactly like ",
          "dhis2_processed_linelist_names.csv.")
}


# ---- Determine latest processed folder and record it in latest.json ----
parse_linelist_date_from_stem <- function(stem) {
  # Match trailing 8-digit run as DDMMYYYY (e.g. LINELIST_29062026 -> 2026-06-29)
  m <- regmatches(stem, regexpr("\\d{8}$", stem))
  if (length(m) == 1 && nzchar(m)) {
    d <- tryCatch(as.Date(m, format = "%d%m%Y"), error = function(e) as.Date(NA))
    if (!is.na(d)) return(d)
  }
  as.Date(NA)
}

# When only a subset of files was processed, determine "latest" from ALL raw files
# so latest.json always points to the genuinely newest one.
all_raw_files <- list.files(raw_dir,
  pattern = "\\.(xlsx|xls|csv)$", full.names = TRUE, ignore.case = TRUE)
all_raw_files <- all_raw_files[grepl("^LINELIST_\\d{8}\\.", basename(all_raw_files), ignore.case = TRUE)]

stems      <- tools::file_path_sans_ext(basename(all_raw_files))
# vapply() drops the Date class (FUN.VALUE as.Date(NA) still returns a bare
# number), which is how latest.json once recorded linelist_date as "20728".
stem_dates <- as.Date(vapply(stems, function(s) as.numeric(parse_linelist_date_from_stem(s)),
                             numeric(1)), origin = "1970-01-01")
# Only exports that have actually been processed can be "latest". A newer raw
# file that this run did not process -- a run restricted to selected dates,
# e.g. "Run Full Pipeline (Selected Date)" on 26 Oct with 27 Oct's export
# already in the raw folder -- used to become latest.json's pointer and then
# stop the run below, because its folder has no _names.csv yet (2026-10-27).
processed  <- file.exists(file.path(processed_dir, stems, "dhis2_processed_linelist_names.csv"))
valid      <- !is.na(stem_dates) & processed
newer_unprocessed <- stems[!is.na(stem_dates) & !processed &
                           (!any(valid) | stem_dates > suppressWarnings(max(stem_dates[valid])))]
if (length(newer_unprocessed))
  log_msg("NOTE: newer raw export(s) not processed yet, so not 'latest': ",
          paste(newer_unprocessed, collapse = ", "), " -- run Epi Linelist cleaning for them.")

latest_idx <- if (any(valid)) {
  which(stem_dates == max(stem_dates[valid]))[1L]
} else if (any(processed)) {
  which(processed)[which.max(file.mtime(all_raw_files[processed]))]
} else {
  which.max(file.mtime(all_raw_files))
}

latest_stem    <- stems[latest_idx]
latest_raw     <- all_raw_files[latest_idx]
latest_date    <- if (!is.na(stem_dates[latest_idx])) as.character(stem_dates[latest_idx]) else NA_character_
epi_latest_dir <- file.path(processed_dir, latest_stem)

latest_info <- list(
  folder        = latest_stem,
  raw_file      = basename(latest_raw),
  linelist_date = latest_date,
  processed_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
)
write_latest("epi_linelist", latest_info)
log_msg("\nLatest linelist: ", latest_stem,
        if (!is.na(latest_date)) paste0(" (", latest_date, ")") else "")
log_msg("latest.json (epi_linelist) updated: ", latest_json_path())


# ---- Build / refresh first_seen_date across all processed linelists ----------
# first_seen_date = the date of the EARLIEST export an alert_id appears in.
#
# INCREMENTAL. This used to read every processed export in full, then read and
# rewrite all three CSVs in every folder, on every run. Two facts make most of
# that unnecessary:
#
#   1. A folder's first_seen_date values depend only on exports dated ON OR
#      BEFORE it. Adding a newer export cannot change any older folder, so only
#      folders from the earliest CHANGED export onwards need rewriting -- on a
#      normal daily run, just the new one.
#   2. The alert_id -> first_seen_date map is kept on disk
#      (first_seen_lookup.csv, plus first_seen_stems.csv listing the exports it
#      was built from), so earlier exports are not re-read to rebuild it.
#
# "Changed" = a stem not in the cache (new or backfilled), a stem this run
# reprocessed, or a stem in the cache whose folder has gone. The cutoff is the
# earliest of their dates: cache entries dated before it are still valid
# (their exports did not change), everything from it onwards is rescanned.
#
# FULL REBUILD when the cache is missing or unreadable, or when the caller sets
# first_seen_full_rebuild <- TRUE. Delete first_seen_lookup.csv to force one.
#
# SELF-HEALING: any CSV whose header lacks first_seen_date is rewritten even if
# it is before the cutoff. That is how ctsafe files written before this pass
# covered them pick the column up.
log_msg("\n=== Updating first_seen_date ===")

fs_lookup_p <- file.path(processed_dir, "first_seen_lookup.csv")
fs_stems_p  <- file.path(processed_dir, "first_seen_stems.csv")
FS_MAIN     <- "dhis2_processed_linelist_names.csv"
FS_FILES    <- c(FS_MAIN,
                 "dhis2_processed_linelist_ctsafe.csv")

all_proc_stems <- list.dirs(processed_dir, full.names = FALSE, recursive = FALSE)
all_proc_stems <- all_proc_stems[grepl("^LINELIST_\\d{8}$", all_proc_stems)]
# A folder with no main CSV is a failed run (only its log is kept): not an export.
# A folder processed before the no-names copy was dropped, and not yet
# reprocessed, may have only dhis2_processed_linelist.csv: it is skipped here
# too, and shows as unprocessed in the app until it is reprocessed.
all_proc_stems <- all_proc_stems[file.exists(
  file.path(processed_dir, all_proc_stems, FS_MAIN))]

proc_stem_dates <- as.Date(
  vapply(all_proc_stems, function(s) as.numeric(parse_linelist_date_from_stem(s)), numeric(1)),
  origin = "1970-01-01"
)
valid_proc      <- !is.na(proc_stem_dates)
all_proc_stems  <- all_proc_stems[valid_proc]
proc_stem_dates <- proc_stem_dates[valid_proc]
ord             <- order(proc_stem_dates)           # oldest first
all_proc_stems  <- all_proc_stems[ord]
proc_stem_dates <- proc_stem_dates[ord]

# Column names only -- no rows parsed.
fs_header <- function(path) {
  tryCatch(names(read.csv(path, nrows = 0, check.names = FALSE)),
           error = function(e) NULL)
}

# The alert_id column only. colClasses "NULL" skips converting every other
# column, which is most of the cost of read.csv on a 16 MB file.
fs_read_alert_ids <- function(path) {
  hdr <- fs_header(path)
  if (is.null(hdr) || !"alert_id" %in% hdr) return(NULL)
  cc <- rep("NULL", length(hdr))
  cc[hdr == "alert_id"] <- "character"
  v <- tryCatch(
    read.csv(path, colClasses = cc, na.strings = "", check.names = FALSE)[[1]],
    error = function(e) NULL)
  if (is.null(v)) return(NULL)
  v <- trimws(v)
  unique(v[!is.na(v) & nzchar(v)])
}

# ---- Load the cache, and work out what changed -------------------------------
fs_cache <- tryCatch({
  if (!file.exists(fs_lookup_p) || !file.exists(fs_stems_p)) stop("no cache")
  lk <- read.csv(fs_lookup_p, colClasses = "character", na.strings = "")
  st <- read.csv(fs_stems_p,  colClasses = "character", na.strings = "")
  if (!all(c("alert_id", "first_seen_date") %in% names(lk)) ||
      !"stem" %in% names(st)) stop("bad cache")
  list(ids = lk$alert_id, dates = lk$first_seen_date, stems = st$stem)
}, error = function(e) NULL)

fs_full <- is.null(fs_cache) ||
  (exists("first_seen_full_rebuild") && isTRUE(first_seen_full_rebuild))

if (fs_full) {
  log_msg("  Full rebuild (",
          if (is.null(fs_cache)) "no usable cache" else "first_seen_full_rebuild set",
          "): scanning all ", length(all_proc_stems), " export(s).")
  fs_ids <- character(0); fs_dates <- character(0)
  fs_cutoff <- if (length(proc_stem_dates)) min(proc_stem_dates) else as.Date(NA)
} else {
  fs_new     <- setdiff(all_proc_stems, fs_cache$stems)
  fs_redone  <- intersect(intersect(first_seen_touched, all_proc_stems), fs_cache$stems)
  fs_removed <- setdiff(fs_cache$stems, all_proc_stems)
  fs_changed_dates <- c(
    proc_stem_dates[all_proc_stems %in% c(fs_new, fs_redone)],
    as.Date(vapply(fs_removed, function(s) as.numeric(parse_linelist_date_from_stem(s)),
                   numeric(1)), origin = "1970-01-01"))
  fs_changed_dates <- fs_changed_dates[!is.na(fs_changed_dates)]
  fs_cutoff <- if (length(fs_changed_dates)) min(fs_changed_dates) else as.Date(NA)

  log_msg("  Cache: ", length(fs_cache$ids), " alert_id(s) from ",
          length(fs_cache$stems), " export(s). New: ", length(fs_new),
          ", reprocessed: ", length(fs_redone), ", removed: ", length(fs_removed), ".")

  # Keep cache entries dated before the cutoff; the rest are recomputed.
  keep <- if (is.na(fs_cutoff)) rep(TRUE, length(fs_cache$ids))
          else !is.na(fs_cache$dates) & fs_cache$dates < format(fs_cutoff, "%Y-%m-%d")
  fs_ids   <- fs_cache$ids[keep]
  fs_dates <- fs_cache$dates[keep]
}

fs_rescan <- if (is.na(fs_cutoff)) character(0) else
  all_proc_stems[proc_stem_dates >= fs_cutoff]
if (!fs_full) {
  log_msg("  ", if (length(fs_rescan))
    paste0("Rescanning from ", format(fs_cutoff, "%Y-%m-%d"), ": ",
           length(fs_rescan), " export(s).")
    else "Nothing changed; no export rescanned.")
}

# ---- Pass 1: extend the map, oldest first (first occurrence wins) ------------
for (i in which(all_proc_stems %in% fs_rescan)) {
  ids <- fs_read_alert_ids(
    file.path(processed_dir, all_proc_stems[i], FS_MAIN))
  if (is.null(ids)) {
    log_msg("  ", all_proc_stems[i], ": no readable alert_id column — skipped.")
    next
  }
  new_ids <- ids[is.na(match(ids, fs_ids))]
  if (length(new_ids)) {
    fs_ids   <- c(fs_ids, new_ids)
    fs_dates <- c(fs_dates, rep(format(proc_stem_dates[i], "%Y-%m-%d"),
                                length(new_ids)))
  }
}

# Save the cache. alert_ids and dates only -- no names -- so it is as safe as
# the ctsafe export, which carries alert_id and first_seen_date too.
write_processed_csv(data.frame(alert_id = fs_ids, first_seen_date = fs_dates,
                               stringsAsFactors = FALSE), fs_lookup_p)
write_processed_csv(data.frame(stem = all_proc_stems, stringsAsFactors = FALSE),
                    fs_stems_p)

# ---- Pass 2: write first_seen_date where it can have changed -----------------
update_csv_first_seen <- function(path, ctsafe = FALSE) {
  if (!file.exists(path)) return(FALSE)
  tmp <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, na.strings = ""),
    error = function(e) NULL
  )
  if (is.null(tmp) || !"alert_id" %in% names(tmp)) return(FALSE)
  ids <- trimws(as.character(tmp$alert_id))
  m   <- match(ids, fs_ids)
  val <- ifelse(!is.na(ids) & nzchar(ids) & !is.na(m), fs_dates[m], "")

  # Already right? Then leave the file alone.
  if ("first_seen_date" %in% names(tmp)) {
    old <- as.character(tmp$first_seen_date)
    old[is.na(old)] <- ""
    if (identical(old, val)) return(FALSE)
  }
  tmp$first_seen_date <- val

  if (ctsafe) {
    # The ctsafe export keeps CT_COLUMNS order, where first_seen_date already
    # has its place (after date_of_sample_collection).
    tmp <- tmp[, c(intersect(CT_COLUMNS, names(tmp)),
                   setdiff(names(tmp), CT_COLUMNS)), drop = FALSE]
  } else {
    # Place first_seen_date after the whole alert_id family, not immediately
    # after alert_id itself. Inserting it at alert_id + 1 split the family:
    # alert_id sat at column 7 and dhis2_alert_id at 10, with first_seen_date
    # wedged between them, so the canonical ID and the raw value it came from
    # could not be read side by side.
    aid_family <- c("alert_id", "dhis2_alert_id", "alert_id_valid",
                    "alert_id_issue", "alert_id_assigned",
                    "alert_id_duplicated")
    present  <- intersect(aid_family, names(tmp))
    last_idx <- max(match(present, names(tmp)))
    # seq_len, not seq(last_idx + 1, ncol): if the family ended at the last
    # column, seq() would count DOWNWARDS and scramble the column order.
    others   <- setdiff(names(tmp), "first_seen_date")
    pre      <- others[seq_len(sum(match(others, names(tmp)) <= last_idx))]
    post     <- setdiff(others, pre)
    tmp      <- tmp[, c(pre, "first_seen_date", post), drop = FALSE]
  }
  write_processed_csv(tmp, path)
  TRUE
}

fs_n_written <- 0L
for (stem in all_proc_stems) {
  paths <- file.path(processed_dir, stem, FS_FILES)
  todo  <- if (stem %in% fs_rescan) file.exists(paths) else
    vapply(paths, function(p) {
      if (!file.exists(p)) return(FALSE)
      h <- fs_header(p)
      !is.null(h) && "alert_id" %in% h && !"first_seen_date" %in% h
    }, logical(1))
  if (!any(todo)) next
  wrote <- mapply(update_csv_first_seen, paths[todo],
                  grepl("_ctsafe\\.csv$", paths[todo]))
  if (any(wrote)) {
    fs_n_written <- fs_n_written + sum(wrote)
    log_msg("  first_seen_date updated: ", stem, " (",
            paste(basename(paths[todo])[wrote], collapse = ", "), ")")
  }
}
log_msg("first_seen_date update complete: ", length(fs_ids), " alert_id(s), ",
        fs_n_written, " file(s) rewritten.")

write_run_log()

# Ensure processed_dhis2_linelist reflects the latest file (with names, for CT pipeline)
latest_names_csv <- file.path(epi_latest_dir, "dhis2_processed_linelist_names.csv")
if (is.null(processed_dhis2_linelist) || !identical(raw_files[latest_idx], raw_file)) {
  if (!file.exists(latest_names_csv))
    stop("Latest linelist ", basename(epi_latest_dir), " has no ",
         basename(latest_names_csv), ". It was probably processed before the ",
         "no-names copy was dropped: reprocess it.", call. = FALSE)
  processed_dhis2_linelist <- read.csv(latest_names_csv, stringsAsFactors = FALSE, na.strings = "")
}


# ------------------------- Clean up environment -------------------------------
epi_linelist_cleaning_complete <- TRUE
keep_objects <- append(keep_objects, c("manifest", "setup_complete", "epi_linelist_cleaning_complete",
                  "get_script_dir", "repo_root", "pipeline_configs",
                  "processed_dhis2_linelist", "epi_latest_dir",
                  "resolve_epi_latest_dir", "resolve_latest_dhis2_linelist",
                  "parse_dhis2_linelist_date"))

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)


