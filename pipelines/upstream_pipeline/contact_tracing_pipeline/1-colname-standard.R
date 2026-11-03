#!/usr/bin/env Rscript
# Clean and standardise the DHIS2 MVE contact tracing export.

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
  x <- gsub("^MVE-FS-\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^MPOX-N-\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^LAB\\s*-\\s*", "", x, ignore.case = TRUE)
  x
}

standardize_column_name <- function(x) {
  x <- strip_column_prefixes(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  replacements <- c(
    "Date d'enregistrement" = "registration_date",
    "MVE - Nom, post nom et prénom du cas" = "case_name",
    "002 MVE-FS-Nom et prénom du contact" = "contact_name",
    "FS - Identifiant contact" = "contact_id",
    "Numero Epid - Alerte MVE" = "source_case_alert_id",
    "FS - Identifiant contact" = "contact_id",
    "Circonstances de cas" = "case_circumstances",
    "Relation avec le contact" = "relationship_to_case",
    "Type de contact" = "contact_type",
    "Date de dernier contact avec le cas source" = "last_contact_with_source_date",
    "Sexe" = "contact_sex",
    "Age(ans)" = "contact_age_years",
    "Age (Mois)" = "contact_age_months",
    "Contact notifies par" = "contact_notified_by",
    "Contact non vu - Raisons" = "contact_not_seen_reason",
    "Coordonnees geo" = "geo_coordinates",
    "Dernier jour de suivi ?" = "is_last_followup_day",
    "FS - Date de suivi" = "followup_date",
    "FS - Precisez symptomes" = "symptoms_specified",
    "FS - Resultat du Suivi" = "followup_result",
    "FS - Symptomes" = "symptoms",
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
  log_msg("RENAMING DHIS2 CONTACT TRACING COLUMNS")
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

# `label` (optional, e.g. "contact_sex (SEX_MAP)") logs the distinct raw
# values that had no entry in `mapping` — i.e. passed through untranslated —
# so gaps in data/maps/*.csv show up in the per-file log the same way
# unmatched health_zone/health_area values do.
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
  # `Nom` — the field name used by the older DRC_Health_zones.shp schema.
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

apply_contact_value_translations <- function(df) {
  for (col in intersect(c("contact_sex"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], SEX_MAP, label = paste0(col, " (SEX_MAP)"))
  }
  for (col in intersect(c("relationship_to_case"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], RELATIONSHIP_MAP, label = paste0(col, " (RELATIONSHIP_MAP)"))
  }
  for (col in intersect(c("contact_type"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], CONTACT_TYPE_MAP, label = paste0(col, " (CONTACT_TYPE_MAP)"))
  }
  for (col in intersect(c("case_circumstances"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], EXPOSITION_CIRCUMSTANCES_MAP, label = paste0(col, " (EXPOSITION_CIRCUMSTANCES_MAP)"))
  }
  for (col in intersect(c("followup_result"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], FOLLOWUP_RESULT_MAP, label = paste0(col, " (FOLLOWUP_RESULT_MAP)"))
  }
  for (col in intersect(c("contact_not_seen_reason"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], CONTACT_NOT_SEEN_REASON_MAP, label = paste0(col, " (CONTACT_NOT_SEEN_REASON_MAP)"))
  }
  for (col in intersect(c("is_last_followup_day"), names(df))) {
    df[[col]] <- translate_mapped_values(df[[col]], LAST_FOLLOWUP_DAY_MAP, label = paste0(col, " (LAST_FOLLOWUP_DAY_MAP)"))
  }

  date_cols <- intersect(
    c("registration_date", "last_contact_with_source_date", "followup_date"),
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

join_epi_linelist <- function(df) {
  contact_field <- contact_cfg$join$contact_alert_field
  if (!contact_field %in% names(df)) {
    df$source_case_alert_id_matched <- FALSE
    return(df)
  }

  epi_path <- file.path(
    repo_root,
    contact_cfg$join$epi_processed_subdir,
    contact_cfg$join$epi_processed_file
  )
  if (!file.exists(epi_path)) {
    log_msg("Epi linelist not found for join (skipping): ", epi_path)
    df$source_case_alert_id_matched <- FALSE
    return(df)
  }

  epi <- read.csv(epi_path, stringsAsFactors = FALSE)
  alert_field <- contact_cfg$join$epi_alert_field
  join_cols <- intersect(contact_cfg$join$epi_join_columns, names(epi))
  if (!alert_field %in% names(epi) || length(join_cols) == 0) {
    warning("Epi join fields missing from processed linelist; skipping join.", call. = FALSE)
    df$source_case_alert_id_matched <- FALSE
    return(df)
  }

  epi[[alert_field]] <- trimws(as.character(epi[[alert_field]]))
  df[[contact_field]] <- trimws(as.character(df[[contact_field]]))

  epi_unique <- epi[!duplicated(epi[[alert_field]]) & nzchar(epi[[alert_field]]), , drop = FALSE]
  match_idx <- match(df[[contact_field]], epi_unique[[alert_field]])

  for (col in join_cols) {
    prefixed <- paste0("source_case_", col)
    values <- epi_unique[[col]][match_idx]
    values[is.na(values)] <- ""
    df[[prefixed]] <- as.character(values)
  }

  df$source_case_alert_id_matched <- nzchar(df[[contact_field]]) & !is.na(match_idx)
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

# ===================  source_case_alert_id validation/repair  =================
#
# `source_case_alert_id` is the same namespace as the epi linelist's `alert_id`
# but arrives with no structural checking, so the same source case shows up
# under several corrupted spellings and looks like several different people.
# Observed in CONTACT-TRACING_01092026: one Lubero case appears under SIX
# values, all corruptions of RDC-AAA-BBB-26-0001236-MVE (REC- for RDC-, an
# underscore for a hyphen, a missing zone code, a truncated tail, and two
# combinations of those).
#
# Expected structure:  RDC-<PPP>-<HHH>-26-<6 or 7 digits>-MVE
# Both serial widths are valid; see the equivalent block in the epi pipeline's
# 1-colname-standard.R. The serial is an opaque string — leading zeros are
# never stripped.
#
# ---------------------------------------------------------------------------
# REPAIR IS TIERED BY WHICH COMPONENT IS BROKEN, because the components differ
# enormously in how recoverable they are.
#
#   Tier 1 — fixed-vocabulary tokens. The country token must be "RDC", the year
#     "26", the suffix "MVE", and the separator "-". Each has exactly one legal
#     value, so repair needs no lookup and cannot change which person the ID
#     refers to. Fixes REC-/RDV-, RDC_, RDC/, "26-043 MVE", "-MVEb", and a
#     missing year and/or suffix.
#
#   Tier 2 — province code. Verified or filled from the row's own `province`.
#     Safe: across all 239 structurally valid source IDs in
#     CONTACT-TRACING_01092026, the province code in the ID and the row's
#     province agreed 239/239. Fixes "LNKV" -> "NKV".
#
#   Tier 3 — zone code. Filled ONLY when corroborated (see below), else NNN.
#     The row's `health_zone` is NOT authoritative for the source case: it
#     describes the CONTACT. It agrees with the source case's linelist zone
#     235/239 times, but the disagreements are real — e.g.
#     RDC-AAA-BBB-26-0001237-MVE is Rwampara in the linelist while its CT rows
#     say Bunia, because the contact lives in Bunia. So health_zone is a
#     candidate, never a conclusion.
#
#   Tier 4 — the serial. NEVER repaired. There is no external source of truth
#     for it. Six of the thirteen invalid values in CONTACT-TRACING_01092026
#     fail here ("241", "239", "RDC-AAA-CCC-26-043-MVE", "RDC-AAA-DDD-26-001"
#     and variants) and none can be recovered: "241" is attached to three
#     different case names and "043" to two, so they are placeholders someone
#     typed rather than damaged IDs. These are left exactly as found and
#     flagged. A placeholder serial is deliberately NOT offered — it would
#     collapse every unknown in a zone into one apparent individual.
#
# WHY province+serial IS NOT USED TO DRIVE REPAIRS. 98.6% of province+serial
# combinations are unique in the linelist, which makes it look like a safe way
# to recover a zone code. It is not. "RDV-AAA-BBB-26-0001241-MVE" (case name
# MASIKA TEMBELE) has two independent errors: RDV for RDC, and a serial that
# is wrong by two. Province+serial AAA/0001241 resolves to exactly one linelist
# row — RDC-AAA-CCC-26-0001241-MVE, BARAKA NDOOLE, a different person in a
# different zone — while the real MASIKA TEMBELE is at ...-0001239-MVE. Using
# that route to repair would silently reassign her contacts to another person
# and corrupt a transmission chain. It is used for verification only.
# ===========================================================================

CT_ALERT_ID_RE <- "^RDC-[A-Z]{3}-[A-Z]{3}-26-[0-9]{6,7}-MVE$"
CT_ALERT_ZONE_PLACEHOLDER <- "NNN"

# Cosmetic normalisation. Never touches the serial's digits.
ct_tidy_alert_id <- function(x) {
  out <- toupper(trimws(as.character(x)))
  # Typographic dashes and the other separators seen in this field, listed by
  # code point rather than as a character-class range so this does not depend
  # on the running locale's collation order. "+" and "=" share a key with "-"
  # (shifted / next to it), so a slip of the finger reads as a separator:
  # rdc-aaa-bbb-26+201234-mve. "," and ";" are NOT
  # folded: in a free-text ID field they more likely separate two IDs, and
  # joining those with "-" would manufacture a plausible-looking wrong one.
  for (sep in c("‐", "‑", "‒", "–", "—", "―", "−", "－", "_", "/", "\\", ".", " ",
                "+", "=")) {
    out <- gsub(sep, "-", out, fixed = TRUE)
  }
  out <- gsub("[[:space:]]+", "", out)
  out <- gsub("-{2,}", "-", out)
  out <- gsub("^-+|-+$", "", out)
  out
}

# province/health_zone -> expected alert-code lookup. Same reference table the
# epi pipeline uses; see data/aliases/health_zone_alert_codes.csv.
ct_load_alert_code_lookup <- function(csv_path) {
  if (!file.exists(csv_path)) {
    log_msg("  NOTE: alert-code reference not found (", csv_path,
            ") — source_case_alert_id repair will be limited to fixed tokens.")
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
                                normalize_key(ref$province)),
    # The SETS of legal codes, as distinct from the name -> code maps above.
    # Needed because a code read out of the ID itself must be checked against
    # them: without that, any three-letter token would be accepted as a
    # province or zone code and the rebuilt ID marked valid on nonsense.
    known_zone = unique(toupper(trimws(ref$zone_code))),
    known_prov = unique(toupper(trimws(ref$province_code)))
  )
}

# Corroboration source: valid alert_id -> patient name, from a processed epi
# linelist. OPTIONAL BY DESIGN. This is the one place CT cleaning reads epi
# output (join_epi_linelist() below remains disabled); if no processed linelist
# is present the script runs exactly as before and simply fills no zone codes.
# The _names.csv is required rather than the de-identified CSV, because the
# corroboration test is a name match.
ct_load_alert_id_corroboration <- function(epi_processed_dir, run_stem = NULL) {
  if (!dir.exists(epi_processed_dir)) return(NULL)

  candidates <- character(0)
  # Prefer the linelist export dated the same as this CT export, since the
  # Shiny app processes them as a pair.
  if (!is.null(run_stem)) {
    dt <- sub("^[A-Z_-]*?([0-9]{8})$", "\\1", toupper(run_stem))
    if (grepl("^[0-9]{8}$", dt)) {
      candidates <- c(candidates,
                      file.path(epi_processed_dir, paste0("LINELIST_", dt),
                                "dhis2_processed_linelist_names.csv"))
    }
  }
  # Otherwise the latest linelist, per the epi_linelist section of latest.json.
  folder <- read_latest("epi_linelist")$folder
  if (length(folder) == 1 && !is.na(folder) && nzchar(folder)) {
    candidates <- c(candidates,
                    file.path(epi_processed_dir, folder,
                              "dhis2_processed_linelist_names.csv"))
  }
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path)) {
    log_msg("  NOTE: no processed linelist _names.csv found — zone codes will ",
            "not be filled (uncorroborated cases become ",
            CT_ALERT_ZONE_PLACEHOLDER, ").")
    return(NULL)
  }

  epi <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character")
  if (!all(c("alert_id", "name") %in% names(epi))) return(NULL)
  keep <- grepl(CT_ALERT_ID_RE, toupper(trimws(epi$alert_id)))
  epi <- epi[keep, , drop = FALSE]
  epi <- epi[!duplicated(epi$alert_id), , drop = FALSE]
  log_msg("  Corroboration source: ", path, " (", nrow(epi), " valid alert_ids)")

  aid <- toupper(trimws(epi$alert_id))
  nmk <- ct_name_key(epi$name)
  zone <- if ("health_zone" %in% names(epi)) as.character(epi$health_zone) else rep("", nrow(epi))

  # Reverse direction (name -> alert_id) is offered ONLY for names borne by
  # exactly one linelist person. A name shared by two people cannot identify
  # either of them, and quietly picking one would attach a contact to the wrong
  # source case.
  nm_counts <- table(nmk[nzchar(nmk)])
  unique_nm <- names(nm_counts)[nm_counts == 1L]
  keep_u <- nzchar(nmk) & nmk %in% unique_nm

  list(
    name_by_id = stats::setNames(nmk, aid),
    id_by_name = stats::setNames(aid[keep_u], nmk[keep_u]),
    zone_by_id = stats::setNames(zone, aid)
    # NOTE: the space-joined token key the CI- variant check needs used to be
    # built here too. The CI-/CX- assignment now lives in
    # 2-assign-contact-ids.R (it has to run after contact_id is filled), and
    # that script builds its own from the same linelist.
  )
}

# Deterministic accent folding, replacing iconv(to = "ASCII//TRANSLIT").
#
# iconv's TRANSLIT behaviour comes from the platform's C library and is not
# portable: glibc renders "Gisèle" as "Gisele", macOS commonly yields "Gis?le",
# and some builds return NA. Since "?" then acts as a token separator, the same
# name normalises to a different key on different machines — which for a
# name-based corroboration test means a repair that succeeds on one analyst's
# laptop and fails on another's. Mapping the characters explicitly removes the
# platform dependency. (Matches translit_ascii() in the epi pipeline's
# 2-deduplicate_linelist.R.)
.CT_TRANSLIT_FROM <- paste0(
  "àáâãäåèéêë", "ìíîïòóôõöù", "úûüýÿçñðþ",
  "ÀÁÂÃÄÅÈÉÊË", "ÌÍÎÏÒÓÔÕÖÙ", "ÚÛÜÝÇÑÐÞ"
)
.CT_TRANSLIT_TO <- paste0(
  "aaaaaaeeee", "iiiiooooou", "uuuyycndt",
  "AAAAAAEEEE", "IIIIOOOOOU", "UUUYCNDT"
)

ct_translit_ascii <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("ß", "ss", x, fixed = TRUE)
  x <- gsub("æ", "ae", x, fixed = TRUE)
  x <- gsub("Æ", "AE", x, fixed = TRUE)
  x <- gsub("œ", "oe", x, fixed = TRUE)
  x <- gsub("Œ", "OE", x, fixed = TRUE)
  x <- chartr(.CT_TRANSLIT_FROM, .CT_TRANSLIT_TO, x)
  gsub("[^ -~]", "", x)
}

# Order-insensitive name key: transliterate, strip punctuation, sort tokens.
# Name order varies constantly between these two files.
ct_name_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- ct_translit_ascii(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  vapply(strsplit(trimws(x), "[[:space:]]+"), function(tk) {
    tk <- tk[nzchar(tk)]
    if (!length(tk)) return("")
    paste0(sort(tk), collapse = "")
  }, character(1))
}

# Do two names agree closely enough to corroborate a zone-code fill?
ct_names_agree <- function(a, b, max_dist = 0.10) {
  if (!nzchar(a) || !nzchar(b)) return(FALSE)
  if (identical(a, b)) return(TRUE)
  d <- stringdist::stringdist(a, b, method = "jw", p = 0.1)
  !is.na(d) && d <= max_dist
}

# Validate and repair a vector of source_case_alert_id values.
# Returns value / original / valid / issue, same shape as the epi validator.
ct_repair_source_alert_ids <- function(raw, province, health_zone, case_name,
                                       lookup, corroboration) {
  n <- length(raw)
  original <- as.character(raw)
  value <- ct_tidy_alert_id(original)
  issue <- rep(NA_character_, n)

  exp_prov <- if (!is.null(lookup)) unname(lookup$prov_code[normalize_key(province)]) else rep(NA_character_, n)
  exp_zone <- if (!is.null(lookup)) unname(lookup$zone_code[normalize_key(health_zone)]) else rep(NA_character_, n)
  nm_key <- ct_name_key(case_name)

  is_blank <- blank_or_na(value)
  issue[is_blank] <- "missing"

  out <- value

  # Repairs are collected and logged once per DISTINCT value after the loop,
  # not once per row. The CT export has ~20,000 rows across ~250 source cases,
  # so per-row logging printed the same seven repairs 52 times and buried the
  # summary underneath.
  rep_from <- character(0); rep_to <- character(0); rep_issue <- character(0)
  for (i in seq_len(n)) {
    if (is_blank[i]) next
    if (grepl(CT_ALERT_ID_RE, value[i])) {
      # Conforming after tidying alone. Still record it: a value that only
      # needed its separators or case fixed WAS repaired, and leaving the issue
      # blank would hide that (e.g. "RDC_AAA-BBB-26-0001236-MVE").
      if (!identical(value[i], original[i])) {
        issue[i] <- "separators_or_case_repaired"
      }
      next
    }

    tk <- strsplit(value[i], "-", fixed = TRUE)[[1]]
    tk <- tk[nzchar(tk)]

    # --- Tier 4 first: no recoverable serial means nothing else is worth doing.
    ser_at <- which(grepl("^[0-9]{6,7}$", tk))
    if (length(ser_at) != 1L) {
      issue[i] <- "serial_unrecoverable"
      next
    }
    serial <- tk[ser_at]

    # Year sits immediately before the serial when present at all.
    before <- if (ser_at >= 2L) tk[seq_len(ser_at - 1L)] else character(0)
    if (length(before) && identical(before[length(before)], "26")) {
      before <- before[-length(before)]
    }
    # First token is the country code whatever it says (REC, RDV, ...).
    if (length(before)) before <- before[-1L]
    alpha <- before[grepl("^[A-Z]{2,4}$", before)]

    # --- Tier 2: province code.
    prov_tok <- if (length(alpha) >= 1L) alpha[1] else NA_character_
    zone_tok <- if (length(alpha) >= 2L) alpha[2] else NA_character_

    # A single alpha token is ambiguous: province or zone? Decide by which
    # expectation it matches.
    if (!is.na(prov_tok) && is.na(zone_tok)) {
      if (!is.na(exp_zone[i]) && identical(prov_tok, exp_zone[i]) &&
          !identical(prov_tok, exp_prov[i])) {
        zone_tok <- prov_tok
        prov_tok <- NA_character_
      }
    }

    # A code read out of the ID itself is only trusted when the reference table
    # actually knows it — otherwise a typo like "XXX" would be accepted as a
    # province code and the rebuilt ID marked valid.
    known_prov <- if (!is.null(lookup)) lookup$known_prov else character(0)
    known_zone <- if (!is.null(lookup)) lookup$known_zone else character(0)

    prov_final <- if (!is.na(exp_prov[i])) {
      exp_prov[i]
    } else if (!is.na(prov_tok) && prov_tok %in% known_prov) {
      prov_tok
    } else {
      NA_character_
    }
    if (is.na(prov_final)) {
      issue[i] <- "province_unresolvable"
      next
    }

    # --- Tier 3: zone code.
    zone_final <- NA_character_
    zone_issue <- NA_character_
    if (!is.na(zone_tok) && !(zone_tok %in% known_zone)) {
      # Possibly a genuinely new zone missing from the reference, so keep the
      # token rather than overwriting it — but do not mark the row valid, so
      # somebody looks at it.
      out[i] <- paste0("RDC-", prov_final, "-", zone_tok, "-26-", serial, "-MVE")
      issue[i] <- "zone_not_in_code_reference"
      rep_from  <- c(rep_from, value[i])
      rep_to    <- c(rep_to, out[i])
      rep_issue <- c(rep_issue, issue[i])
      next
    }
    if (!is.na(zone_tok)) {
      zone_final <- zone_tok
    } else if (!is.na(exp_zone[i])) {
      # Candidate from the row's health_zone — fill ONLY if corroborated by a
      # linelist ID that exists AND whose name matches this row's case_name.
      cand <- paste0("RDC-", prov_final, "-", exp_zone[i], "-26-", serial, "-MVE")
      ok <- FALSE
      if (!is.null(corroboration) && cand %in% names(corroboration$name_by_id)) {
        ok <- ct_names_agree(nm_key[i], unname(corroboration$name_by_id[cand]))
      }
      if (ok) {
        zone_final <- exp_zone[i]
        zone_issue <- "zone_code_filled_corroborated"
      } else {
        zone_final <- CT_ALERT_ZONE_PLACEHOLDER
        zone_issue <- if (is.null(corroboration)) {
          "zone_code_unresolved_no_corroboration_source"
        } else {
          "zone_code_unresolved"
        }
      }
    } else {
      zone_final <- CT_ALERT_ZONE_PLACEHOLDER
      zone_issue <- "zone_code_unresolved"
    }

    rebuilt <- paste0("RDC-", prov_final, "-", zone_final, "-26-", serial, "-MVE")
    out[i] <- rebuilt

    if (!is.na(zone_issue)) {
      issue[i] <- zone_issue
    } else if (!is.na(prov_tok) && !identical(prov_tok, prov_final)) {
      issue[i] <- "province_code_corrected"
    } else {
      issue[i] <- "fixed_tokens_repaired"
    }
    rep_from  <- c(rep_from, value[i])
    rep_to    <- c(rep_to, rebuilt)
    rep_issue <- c(rep_issue, issue[i])
  }

  # One line per distinct repair, with the number of rows it covered.
  if (length(rep_from)) {
    key <- paste(rep_from, rep_to, rep_issue, sep = "\r")
    tab <- table(key)
    first <- !duplicated(key)
    ord <- order(rep_issue[first], rep_from[first])
    uf <- rep_from[first][ord]; ut <- rep_to[first][ord]
    ui <- rep_issue[first][ord]; uk <- key[first][ord]
    log_msg("  Repairs applied (", length(rep_from), " row(s), ", length(uf),
            " distinct value(s)):")
    for (k in seq_along(uf)) {
      log_msg(sprintf("    %-30s -> %-28s  [%s]  x%d rows",
                      uf[k], ut[k], ui[k], as.integer(tab[[uk[k]]])))
    }
  }

  ok <- grepl(CT_ALERT_ID_RE, out) &
    !grepl(paste0("-", CT_ALERT_ZONE_PLACEHOLDER, "-"), out, fixed = FALSE)

  list(
    value    = out,
    original = ifelse(out != original & !is.na(original), original, NA_character_),
    valid    = ok,
    issue    = issue
  )
}


# ============  Blank source_case_alert_id: fill the confident few  ============
#
# 81 rows of CONTACT-TRACING_01092026 arrive with no source_case_alert_id — we
# do not know which case exposed the contact. Unlike a blank alert_id this does
# not leave a person unidentified (the contact themselves is fully identified);
# it breaks the transmission chain.
#
# WHAT DOES NOT WORK, measured across all 43 exports:
#   * same contact_id elsewhere in the SAME export with a filled source: 0 rows
#   * same contact_id in ANY earlier export with a filled source:        0 rows
#   * same case_name anywhere in CT history with a filled source:        0 rows
# All 81 rows do carry a contact_id, but not one of those 75 contacts has ever
# had a source case recorded. The blank is permanent, not an intermittent gap,
# so there is nothing to carry forward.
#
# WHAT DOES WORK, barely: matching case_name against the linelist. 13 of the 81
# rows match a linelist name, covering 4 distinct names — and corroboration
# splits them:
#
#   ODRAMA LIKOBE        3 rows  2 tokens  Boma Mangbetu = Boma Mangbetu  fill
#   TALIKO TEKA FISTON   3 rows  3 tokens  Isiro         = Isiro          fill
#   ESTELA BOLIA         1 row   2 tokens  Mangala       = Mangala        fill
#   OKWERA               6 rows  1 token   Boma Mangbetu / Isiro          REFUSE
#
# OKWERA fails on two counts at once: a single short name token, and geography
# that disagrees. That is the profile of a coincidental match, so it is
# reported rather than written.
#
# So this fills 7 of 81 rows. The remaining 68 — 48 whose case name appears
# nowhere in the linelist, and 20 with no case name at all (15 contacts, all in
# Damas and Boma Mangbetu) — cannot be resolved from any data held here. That
# is a field data-collection gap, and the report exists to hand it back.
#
# NOTE on the geography test: a CT row's health_zone describes the CONTACT, not
# the source case, and the two legitimately differ (235/239 agreement among
# valid source IDs). So a zone match is supporting evidence, never proof, and
# it is required here only because the alternative is filling on a bare name.
CT_SOURCE_FILL_MIN_NAME_TOKENS <- 2

# How many name tokens a value has. Separate from ct_name_key(), which joins
# its sorted tokens with no separator.
ct_name_token_count <- function(x) {
  s <- ct_translit_ascii(tolower(trimws(as.character(x))))
  s <- gsub("[^a-z0-9 ]", " ", s)
  vapply(strsplit(trimws(s), "[[:space:]]+"),
         function(tk) sum(nzchar(tk)), integer(1))
}

ct_fill_blank_source_ids <- function(source_id, case_name, health_zone,
                                     corroboration) {
  n <- length(source_id)
  out <- as.character(source_id)
  action <- rep(NA_character_, n)
  suggested <- rep(NA_character_, n)

  blank <- blank_or_na(out)
  if (!any(blank)) {
    return(list(value = out, action = action, suggested = suggested,
                n_filled = 0L))
  }

  nm_key <- ct_name_key(case_name)
  # Counted from the RAW name, not from nm_key: ct_name_key() joins its sorted
  # tokens with no separator (it exists for Jaro-Winkler comparison, where
  # spaces are noise), so splitting the key on spaces always yields 1 and
  # nothing would ever pass the token test.
  n_tokens <- ct_name_token_count(case_name)

  no_name <- blank & !nzchar(nm_key)
  action[no_name] <- "not_filled_no_case_name"

  n_filled <- 0L
  for (i in which(blank & nzchar(nm_key))) {
    hit <- if (is.null(corroboration)) NA_character_ else
      unname(corroboration$id_by_name[nm_key[i]])
    if (is.na(hit)) {
      action[i] <- "not_filled_name_not_in_linelist"
      next
    }
    suggested[i] <- hit
    ll_zone <- unname(corroboration$zone_by_id[hit])
    zone_ok <- !is.na(ll_zone) && nzchar(ll_zone) &&
      identical(normalize_key(ll_zone), normalize_key(health_zone[i]))
    tokens_ok <- n_tokens[i] >= CT_SOURCE_FILL_MIN_NAME_TOKENS

    if (zone_ok && tokens_ok) {
      out[i] <- hit
      action[i] <- "filled_from_linelist_name"
      n_filled <- n_filled + 1L
    } else if (!tokens_ok && !zone_ok) {
      action[i] <- "not_filled_single_token_name_and_zone_mismatch"
    } else if (!tokens_ok) {
      action[i] <- "not_filled_single_token_name"
    } else {
      action[i] <- "not_filled_zone_mismatch"
    }
  }

  list(value = out, action = action, suggested = suggested, n_filled = n_filled)
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
# that happen before/after the per-file loop (latest.json), written once at
# the end. Both go to this run's logs/ folder.
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
# as contact_tracing_run_log.txt.
write_file_log <- function(stem) {
  write_log_file(.file_log_lines, paste0(stem, "_processing_log.txt"))
}

write_run_log <- function() {
  if (!length(.run_log_lines)) return(invisible(NULL))
  write_log_file(.run_log_lines, "contact_tracing_run_log.txt")
}


# --------------------------- Column Names and Maps ----------------------------
CONTACT_EXPLICIT_RENAMES <- load_map_csv("contact_tracing_column_renames")

SEX_MAP <- load_map_csv("sex_map")

#### Note - the below maps need checking!!

RELATIONSHIP_MAP <- load_map_csv("relationship_map")

EXPOSITION_CIRCUMSTANCES_MAP <- load_map_csv("exposition_circumstances_map")

CONTACT_TYPE_MAP <- load_map_csv("contact_type_map")

FOLLOWUP_RESULT_MAP <- load_map_csv("followup_result_map")

CONTACT_NOT_SEEN_REASON_MAP <- load_map_csv("contact_not_seen_reason_map")

LAST_FOLLOWUP_DAY_MAP <- load_map_csv("last_followup_day_map")

# Columns specifically kept in the contact tracing data.
# dhis2_processed_linelist_ctsafe.csv has these and removes all others (safe for git).
CT_SAFE_COLUMNS <- c('source_case_alert_id',
                     # As in the epi pipeline: the raw value travels downstream
                     # so a consumer can tell a repaired ID from an untouched
                     # one. Additive, so existing consumers are unaffected.
                     'dhis2_source_case_alert_id',
                     'contact_id', 'registration_date', 'case_circumstances',
                     'relationship_to_case', 'contact_type', 'last_contact_with_source_date',
                     'is_last_followup_day',
                     'followup_date', 'followup_result', 'symptoms',
                     # Added 2026-10-30; same name as in the linelist.
                     'vaccine_status',
                     # System IDs, in exports from 21 Oct 2026 on. contact_uid
                     # is 1:1 with contact_id and never blank; visit_uid is one
                     # per row. Codes only, no personal information.
                     'contact_uid', 'visit_uid')



# ------------------- Get specific config and set other paths ------------------
contact_cfg <- pipeline_configs$contact_tracing
raw_dir <- file.path(repo_root, contact_cfg$data$raw_subdir)
processed_dir <- file.path(repo_root, contact_cfg$data$processed_subdir)
data_repo <- normalizePath(file.path(repo_root, "..", "BDBV2026-Data"), mustWork = FALSE)
# Health-zone shapefile now matches the epi pipeline's approach: the local
# GRID3_COD_health_zones_v8 shapefile instead of the sibling repo's
# DRC_Health_zones.shp (see load_health_zone_lookup() below for the matching
# column-name fix that goes with this).
shapefile_path <- file.path(repo_root, "data", "geo", "GRID3_COD_health_zones_v8", "GRID3_COD_health_zones_v8_0.shp")
aliases_path <- file.path(repo_root, "data", "aliases", "health_zone_aliases.csv")
health_area_names_path <- file.path(repo_root, "data", "geo", "GRID3_COD_health_areas_v8", "GRID3_COD_health_areas_v8_0.csv")
health_area_aliases_path <- file.path(repo_root, "data", "aliases", "health_area_aliases.csv")

# zone_aliases used to be a hardcoded named vector; now loaded from this
# repo's own data/aliases/health_zone_aliases.csv (observed_name/canonical_nom
# columns), keyed by normalize_key(observed_name) to match how it's consumed
# in load_health_zone_lookup(). Deliberately NOT the same file as aliases_path
# above (that's BDBV2026-Data's sibling-repo aliases.csv, a separate table).
zone_aliases <- load_zone_aliases_csv(
  file.path(repo_root, "data", "aliases", "health_zone_aliases.csv"),
  normalize_key
)
log_msg("Loaded ", length(zone_aliases), " zone_aliases entries from data/aliases/health_zone_aliases.csv")

# Same idea as zone_aliases, but for health_area names GRID3 doesn't
# recognise. Starts empty (header row only) — add rows to
# data/aliases/health_area_aliases.csv as unmatched names are reviewed.
health_area_aliases <- load_zone_aliases_csv(health_area_aliases_path, normalize_key)
log_msg("Loaded ", length(health_area_aliases), " health_area_aliases entries from ", health_area_aliases_path)

# province/health_zone -> expected alert_id code, for source_case_alert_id
# validation. Shared with the epi pipeline — one reference, both files.
alert_code_ref_path <- file.path(repo_root, "data", "aliases", "health_zone_alert_codes.csv")
alert_code_lookup <- ct_load_alert_code_lookup(alert_code_ref_path)
if (!is.null(alert_code_lookup)) {
  log_msg("Loaded ", length(alert_code_lookup$zone_code),
          " health-zone alert-code entries from ", alert_code_ref_path)
}

# Columns containing personally identifiable information. The list lives in
# pipeline_configs.json (pii_columns.contact_tracing) so every script and
# report shares one copy. contact_tracing_processed_names.csv keeps them -- it
# is the full table, and the only per-export copy apart from the ctsafe
# allowlist. Anything that must not see PII drops these on load.
CT_NAME_COLUMNS <- unlist(pipeline_configs$pii_columns$contact_tracing)
if (!length(CT_NAME_COLUMNS))
  stop("pipeline_configs.json has no pii_columns.contact_tracing list", call. = FALSE)

# ---- Discover all raw contact tracing files ----
raw_files <- list.files(raw_dir,
  pattern     = "\\.(xlsx|xls|csv)$",
  full.names  = TRUE,
  ignore.case = TRUE
)
if (length(raw_files) == 0) stop("No raw contact tracing files found in: ", raw_dir, call. = FALSE)

# If the Shiny app set ct_force_stems, restrict processing to only those files.
if (exists("ct_force_stems") && length(ct_force_stems) > 0) {
  all_stems <- tools::file_path_sans_ext(basename(raw_files))
  keep      <- all_stems %in% ct_force_stems
  if (!any(keep)) stop("None of the selected stems found in raw dir: ",
                       paste(ct_force_stems, collapse = ", "), call. = FALSE)
  raw_files <- raw_files[keep]
  log_msg("Processing ", sum(keep), " selected file(s): ",
          paste(tools::file_path_sans_ext(basename(raw_files)), collapse = ", "))
}

processed_contact_tracing <- NULL   # will hold the latest processed df

for (raw_file in raw_files) {
  current_stem       <- tools::file_path_sans_ext(basename(raw_file))
  current_output_dir <- file.path(processed_dir, current_stem)
  CONTACT_TRACING_XLSX   <- raw_file
  # The full table, PII included. Its existence is also what marks an export
  # as processed (here and in the Shiny app's file lists).
  CONTACT_TRACING_OUTPUT <- file.path(current_output_dir, "contact_tracing_processed_names.csv")
  .file_log_lines        <- character(0)  # reset per-file log for this raw file

  if (file.exists(CONTACT_TRACING_OUTPUT)) {
    log_msg("Skipping ", current_stem, " — already processed.")
    next
  }
  dir.create(current_output_dir, recursive = TRUE, showWarnings = FALSE)
  log_msg("\n=== Processing: ", current_stem, " ===")
  tryCatch({

# ------------------------- Process DHIS2 Contact Tracing ---------------------
ext <- tolower(tools::file_ext(raw_file))
df <- if (ext == "csv") {
  read.csv(CONTACT_TRACING_XLSX, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
} else {
  read_excel(CONTACT_TRACING_XLSX, sheet = 1)
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
names(df) <- rename_columns(df, CONTACT_EXPLICIT_RENAMES)

# Check if there are any cols to drop
drop_cols <- vapply(names(df), function(nm) {
  grepl("^\\.{3}|^unnamed_|^[0-9]+$", nm) || all(blank_or_na(df[[nm]]))
}, logical(1))
if (sum(drop_cols) > 0) {
  log_msg("Dropping column(s): ", paste0(colnames(df)[drop_cols], collapse = ", "))
}
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
if ("health_area" %in% names(df)) {
  ha_lookup <- load_health_area_lookup(health_area_names_path, health_area_aliases)
  ha_std <- standardize_health_area_column(df$health_area, ha_lookup)
  df$health_area <- ha_std$value
  df$health_area_unmatched <- ha_std$unmatched
}

# Tidy ages
log_msg("TIDYING AGE FIELD")
if ("contact_age" %in% names(df)) {
  age_months_col <- if ("contact_age_months" %in% names(df)) df$age_months else rep(NA, nrow(df))
  parsed_age <- normalize_age_fields(df$contact_age, age_months_col)
  df$contact_age <- parsed_age$age
  if ("contact_age_months" %in% names(df)) {
    df$contact_age_months <- parsed_age$age_months
  }
}

log_msg("TIDYING OTHER COLUMNS USING PROVIDED MAPS")
df <- apply_contact_value_translations(df)

# Validate and repair source_case_alert_id. Runs after geo cleaning so
# province/health_zone are standardised and usable as repair inputs.
log_msg("VALIDATING SOURCE_CASE_ALERT_ID STRUCTURE")
if ("source_case_alert_id" %in% names(df)) {
  prov_vec <- if ("province" %in% names(df)) df$province else rep(NA_character_, nrow(df))
  hz_vec   <- if ("health_zone" %in% names(df)) df$health_zone else rep(NA_character_, nrow(df))
  cn_vec   <- if ("case_name" %in% names(df)) df$case_name else rep(NA_character_, nrow(df))

  original_sid <- df$source_case_alert_id

  # Same convention as the epi pipeline's dhis2_alert_id: this column holds the
  # value exactly as the export supplied it, for every row including blanks,
  # while source_case_alert_id becomes our canonical (repaired) value.
  df$dhis2_source_case_alert_id <- as.character(df$source_case_alert_id)

  ct_corroboration <- ct_load_alert_id_corroboration(
    file.path(repo_root, "data", "processed", "dhis2_linelist_processed"),
    current_stem
  )

  sid <- ct_repair_source_alert_ids(df$source_case_alert_id, prov_vec, hz_vec,
                                    cn_vec, alert_code_lookup, ct_corroboration)

  df$source_case_alert_id       <- sid$value
  df$source_case_alert_id_valid <- sid$valid
  df$source_case_alert_id_issue <- sid$issue

  log_msg("  Expected structure: RDC-<PPP>-<HHH>-26-<6 or 7 digits>-MVE")
  n_nonblank <- sum(!blank_or_na(df$source_case_alert_id))
  log_msg("  Conforming: ", sum(sid$valid), " of ", n_nonblank, " non-blank row(s)")
  log_msg("  Distinct source_case_alert_id values before repair: ",
          length(unique(original_sid[!blank_or_na(original_sid)])),
          " | after repair: ",
          length(unique(df$source_case_alert_id[!blank_or_na(df$source_case_alert_id)])))

  if (any(!is.na(sid$issue))) {
    log_msg("  source_case_alert_id issues:")
    log_print(table(sid$issue, useNA = "no"))
    for (iss in sort(unique(stats::na.omit(sid$issue)))) {
      rows_i <- which(sid$issue == iss)
      # Report the value as it arrived, which is what a reviewer needs to go
      # and look up. dhis2_source_case_alert_id now holds that for every row,
      # so no need to fall back to the repaired value.
      shown <- unique(ifelse(blank_or_na(df$dhis2_source_case_alert_id[rows_i]),
                             df$source_case_alert_id[rows_i],
                             df$dhis2_source_case_alert_id[rows_i]))
      log_msg("    ", iss, " (", length(rows_i), " row(s), ",
              length(shown), " distinct value(s)): ",
              paste(utils::head(shown, 20), collapse = ", "),
              if (length(shown) > 20) ", ..." else "")
    }
    # A repaired ID that still isn't in the linelist is either a linelist gap
    # or an unrepaired serial error — worth surfacing, not silently accepting.
    if (!is.null(ct_corroboration)) {
      # `ct_corroboration` is a LIST OF THREE LOOKUPS, not a lookup.
      # names() on it returns c("name_by_id", "id_by_name", "zone_by_id"), so
      # comparing alert IDs against it made setdiff() a no-op and every valid
      # source case was reported absent: 239 of 239 on CONTACT-TRACING_01092026,
      # where the true figure is 12. The warning fired on every run and was
      # therefore worth nothing -- a check that always fails is a check nobody
      # reads. The keys live one level down, in name_by_id.
      absent <- setdiff(
        unique(df$source_case_alert_id[sid$valid]),
        names(ct_corroboration$name_by_id)
      )
      if (length(absent)) {
        # Rows as well as distinct values: the two say different things. On
        # CONTACT-TRACING_01092026 the 12 absent IDs carry 875 rows, and two of
        # them account for 804 of those, so a count of distinct values alone
        # badly understates how much of the export hangs off an unresolvable
        # source case.
        n_rows <- sum(df$source_case_alert_id %in% absent)
        log_msg("  WARNING: ", length(absent), " valid source_case_alert_id(s) ",
                "not present in the processed linelist (", n_rows, " row(s)): ",
                paste(utils::head(sort(absent), 20), collapse = ", "),
                if (length(absent) > 20) ", ..." else "")
        log_msg("    Either a linelist gap or a serial error the repair left ",
                "alone (Tier 4: serials are never repaired, since nothing ",
                "corroborates them). These rows DO carry a structurally valid ",
                "source case, so nothing downstream is blocked -- the link is ",
                "simply to a case the linelist has not got.")
      }
    }
  } else {
    log_msg("  No source_case_alert_id issues found.")
  }

  # ---- Blank source_case_alert_id: fill the confident few, report the rest --
  log_msg("RESOLVING BLANK SOURCE_CASE_ALERT_ID")
  hz_for_fill <- if ("health_zone" %in% names(df)) df$health_zone else rep(NA_character_, nrow(df))
  fill <- ct_fill_blank_source_ids(df$source_case_alert_id, cn_vec,
                                   hz_for_fill, ct_corroboration)
  df$source_case_alert_id <- fill$value
  df$source_case_alert_id_fill_note <- fill$action
  # Carried in a column because the CI-/CX- assignment now runs in
  # 2-assign-contact-ids.R and needs it for suggested_linelist_alert_id.
  df$source_case_alert_id_suggested <- fill$suggested

  n_blank_before <- sum(blank_or_na(original_sid))
  log_msg("  Rows with no source_case_alert_id: ", n_blank_before,
          " | filled from a corroborated linelist name match: ", fill$n_filled)
  if (any(!is.na(fill$action))) {
    log_print(table(fill$action, useNA = "no"))
  }

} else {
  log_msg("  source_case_alert_id column not present — skipping.")
}

# I don't want to join the Epi linelist at the moment, just clean the contact tracing data
# log_msg("JOINING EPI LINELIST")
# df <- join_epi_linelist(df)

# Reorder columns
log_msg("REORDERING COLUMNS")
geo_cols <- c("country", "province", "health_zone", "health_zone_unmatched", "health_area", "health_area_unmatched")
id_cols <- c("source_case_alert_id", "contact_id", "case_name", "contact_name")
remaining <- setdiff(names(df), c(geo_cols, id_cols))
df <- df[, c(geo_cols, id_cols, remaining), drop = FALSE]


# Write output: two files per export.
#   contact_tracing_processed_names.csv   the full table, PII included (always
#                                         written, so every export has one)
#   contact_tracing_processed_ctsafe.csv  the CT_SAFE_COLUMNS allowlist
# There used to be a third, "no names" copy (contact_tracing_processed.csv).
# It was dropped: it was the full table minus CT_NAME_COLUMNS, so anything
# that needs it can drop those columns from _names.csv on load.
log_msg("WRITING PROCESSED CSV")
ct_pii_present  <- CT_NAME_COLUMNS[CT_NAME_COLUMNS %in% names(df)]
ct_cols_to_keep <- CT_SAFE_COLUMNS[CT_SAFE_COLUMNS %in% names(df)]
write_processed_csv(df, CONTACT_TRACING_OUTPUT)
log_msg("  Full table written to ", basename(CONTACT_TRACING_OUTPUT), " (PII columns present: ",
        if (length(ct_pii_present)) paste(ct_pii_present, collapse = ", ") else "none", ")")

# A no-names copy left over from before the change above would otherwise sit
# beside the new output looking current. Remove it.
legacy_plain <- file.path(current_output_dir, "contact_tracing_processed.csv")
if (file.exists(legacy_plain)) {
  unlink(legacy_plain)
  log_msg("  Removed old no-names copy: ", basename(legacy_plain))
}

df_ct_safe <- df[, intersect(ct_cols_to_keep, names(df)), drop = FALSE]
write_processed_csv(df_ct_safe, file.path(current_output_dir, "contact_tracing_processed_ctsafe.csv"))


log_msg("\n-------CLEANING REPORT------")
log_msg("Contact tracing read: ", CONTACT_TRACING_XLSX)
log_msg("Contact tracing wrote: ", CONTACT_TRACING_OUTPUT)
log_msg("Rows: ", nrow(df), " | columns: ", ncol(df))
log_msg("Unique cases: ", length(unique(df$source_case_alert_id)), " | contacts: ", length(unique(df$contact_id)), " | case-contact pairs: ", nrow(dplyr::distinct(dplyr::select(df, source_case_alert_id, contact_id))))
if ("province" %in% names(df)) {
  log_msg("Provinces: ", paste(sort(unique(df$province[df$province != ""])), collapse = ", "))
}
if ("followup_result" %in% names(df)) {
  log_msg("followup_result:")
  log_print(table(df$followup_result, useNA = "ifany"))
}
if ("source_case_alert_id_matched" %in% names(df)) {
  matched <- sum(df$source_case_alert_id_matched, na.rm = TRUE)
  unique_alerts <- length(unique(df$source_case_alert_id[df$source_case_alert_id != ""]))
  matched_alerts <- length(unique(df$source_case_alert_id[
    df$source_case_alert_id_matched & df$source_case_alert_id != ""
  ]))
  log_msg(
    "Source alert join: ",
    matched, " / ", nrow(df),
    " rows matched; ",
    matched_alerts, " / ", unique_alerts,
    " unique source alerts matched to epi linelist"
  )
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

  # Store processed df (with names, for 4-build-id-lookup.R)
  processed_contact_tracing <- df

  write_file_log(current_stem)

  }, error = function(e) {
    log_msg("\n!!! ERROR processing ", current_stem, ": ", conditionMessage(e),
            "\n    Skipping this file and continuing.")
    write_file_log(current_stem)
    # Remove the incomplete output so it won't be treated as processed on the
    # next run. The log is kept: it is in logs/, not in this folder.
    if (dir.exists(current_output_dir) && !file.exists(CONTACT_TRACING_OUTPUT)) {
      unlink(current_output_dir, recursive = TRUE)
    }
  }) # end tryCatch

} # ---- end for (raw_file in raw_files) ----

# ---- Persist the assigned case-ID registry ---------------------------------
# Written once per run rather than per file. Saved even if a file errored,
# because IDs already handed out must never be reissued to someone else.
# ---- Determine latest processed folder and record it in latest.json ----
parse_ct_date_from_stem <- function(stem) {
  m <- regmatches(stem, regexpr("\\d{8}$", stem))
  if (length(m) == 1 && nzchar(m)) {
    d <- tryCatch(as.Date(m, format = "%d%m%Y"), error = function(e) as.Date(NA))
    if (!is.na(d)) return(d)
  }
  as.Date(NA)
}

# Scan ALL raw files for latest.json (not just the selected subset)
all_raw_files_ct <- list.files(raw_dir,
  pattern = "\\.(xlsx|xls|csv)$", full.names = TRUE, ignore.case = TRUE)

stems      <- tools::file_path_sans_ext(basename(all_raw_files_ct))
# vapply() drops the Date class, which is how latest.json once recorded
# ct_date as a bare day count ("20728").
stem_dates <- as.Date(vapply(stems, function(s) as.numeric(parse_ct_date_from_stem(s)),
                             numeric(1)), origin = "1970-01-01")
# Only processed exports can be "latest" (see the same fix in the epi
# cleaning step, 2026-10-27): a newer raw file a date-restricted run skipped
# must not become latest.json's pointer.
processed  <- file.exists(file.path(processed_dir, stems, "contact_tracing_processed_names.csv"))
valid      <- !is.na(stem_dates) & processed
newer_unprocessed <- stems[!is.na(stem_dates) & !processed &
                           (!any(valid) | stem_dates > suppressWarnings(max(stem_dates[valid])))]
if (length(newer_unprocessed))
  log_msg("NOTE: newer raw export(s) not processed yet, so not 'latest': ",
          paste(newer_unprocessed, collapse = ", "), " -- run Contact Tracing cleaning for them.")

latest_idx <- if (any(valid)) {
  which(stem_dates == max(stem_dates[valid]))[1L]
} else if (any(processed)) {
  which(processed)[which.max(file.mtime(all_raw_files_ct[processed]))]
} else {
  which.max(file.mtime(all_raw_files_ct))
}

latest_stem   <- stems[latest_idx]
latest_raw    <- all_raw_files_ct[latest_idx]
latest_date   <- if (!is.na(stem_dates[latest_idx])) as.character(stem_dates[latest_idx]) else NA_character_
ct_latest_dir <- file.path(processed_dir, latest_stem)

latest_info <- list(
  folder        = latest_stem,
  raw_file      = basename(latest_raw),
  ct_date       = latest_date,
  processed_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
)
write_latest("contact_tracing", latest_info)
log_msg("\nLatest contact tracing: ", latest_stem,
        if (!is.na(latest_date)) paste0(" (", latest_date, ")") else "")
log_msg("latest.json (contact_tracing) updated: ", latest_json_path())

write_run_log()

# Ensure processed_contact_tracing reflects the latest file (with names)
if (is.null(processed_contact_tracing) || !identical(raw_files[latest_idx], raw_file)) {
  latest_names_csv <- file.path(ct_latest_dir, "contact_tracing_processed_names.csv")
  if (file.exists(latest_names_csv))
    processed_contact_tracing <- read.csv(latest_names_csv, stringsAsFactors = FALSE, na.strings = "")
}


# ---------------------- Warn if step 2 has not yet run ------------------------
# This script used to assign CI-/CX- IDs itself. It no longer does: they moved
# to 2-assign-contact-ids.R so that CX- could key on a contact_id that CT- had
# already filled. The consequence is that THIS SCRIPT'S OUTPUT IS NO LONGER
# FINISHED. Running it alone rewrites contact_tracing_processed_names.csv with blank
# contact_id and blank source_case_alert_id, silently discarding IDs a previous
# complete run had assigned — which is exactly what happened once and looked
# like assignment being broken rather than skipped.
local({
  if (is.null(processed_contact_tracing) ||
      !is.data.frame(processed_contact_tracing)) return(invisible(NULL))
  d <- processed_contact_tracing
  blank <- function(col) {
    if (!col %in% names(d)) return(NA_integer_)
    v <- d[[col]]
    sum(is.na(v) | !nzchar(trimws(as.character(v))))
  }
  n_contact <- blank("contact_id")
  n_source  <- blank("source_case_alert_id")
  assigned_ran <- "contact_id_assigned" %in% names(d)
  if (!assigned_ran && (isTRUE(n_contact > 0) || isTRUE(n_source > 0))) {
    # Worded to read correctly in BOTH contexts. In a chained run this prints
    # immediately before 2-assign-contact-ids.R starts, so "NOT FINISHED — has
    # not run" looked like a failure when nothing was wrong. It states what is
    # still outstanding rather than asserting something went wrong.
    log_msg("")
    log_msg("  Step 1 output is not final — IDs are assigned in the next step:")
    if (isTRUE(n_contact > 0))
      log_msg("    ", n_contact, " row(s) still have no contact_id (CT- fills these)")
    if (isTRUE(n_source > 0))
      log_msg("    ", n_source, " row(s) still have no source_case_alert_id ",
              "(CI-/CX- fill these)")
    log_msg("    If nothing runs 2-assign-contact-ids.R after this, the export ",
            "is incomplete: run it, or use the app's CT Cleanup, which chains ",
            "the whole sequence.")
  }
})


# ------------------------- Clean up environment -------------------------------
contact_tracing_cleaning_complete <- TRUE
keep_objects <- append(keep_objects, c("processed_contact_tracing", "contact_tracing_cleaning_complete",
                                       "ct_latest_dir", "resolve_ct_latest_dir"))

rm(list = setdiff(ls(), keep_objects), envir = .GlobalEnv)
