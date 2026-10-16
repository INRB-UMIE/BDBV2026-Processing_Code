#!/usr/bin/env Rscript
# Step 1: Infector-infectee pairs for the contact tracing pipeline.
#   - Reads contact_tracing_combined_ctsafe.csv and dhis2_processed_linelist_ctsafe.csv
#     from the latest data/<DATE>/ folder holding both (or CT_PIPELINE_DATE)
#   - Optionally reads imputed onset dates from the epi linelist pipeline's
#     step 2 output for the same date (outputs/<DATE>/dhis2_linelist_with_imputed_onset.csv)
#   - Writes outputs/<DATE>/contact_tracing/infector_infectee_pairs.csv: one row
#     per infector-infectee pair (source_case_person_id -> contact_person_id)
#   - Writes outputs/<DATE>/contact_tracing/infector_infectee_dq_issues.csv:
#     self-pairs (dropped), reciprocal pairs, unmapped follow-up results
#
# Duplicates: where a person has several linelist rows, or a pair has several
# contact tracing rows, that disagree, ALL distinct values are kept, sorted and
# separated by "/" (e.g. infector_sex = "female/male"). Blanks are ignored when
# collapsing, except for case classification, where a blank is recorded as
# "blank" (matching epi step 3) so blank-vs-classified disagreements show up.
#
# Final follow-up result = the result on the latest dated follow-up that has
# a result (all results on that date, "/"-separated if they disagree).

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

ct_cfg <- pipeline_configs$contact_tracing

# --------------- Determine which processed dir to handle ----------------------
ct_dir <- resolve_ct_latest_dir()
run_date <- basename(ct_dir)
ct_outputs_dir <- file.path(data_root, "outputs", run_date, ct_cfg$data$outputs_subdir)
dir.create(ct_outputs_dir, recursive = TRUE, showWarnings = FALSE)

message("======== Infector-infectee pairs: ", run_date, " =========")

# ------------------------------- Load data ------------------------------------
message("")
message("A. LOADING DATA")

ct_raw <- read.csv(
  file.path(ct_dir, ct_cfg$data$files$contact_tracing),
  stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
  na.strings = c("", "NA", "N/A")
)
ct_raw <- ct_raw |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), trimws),
    followup_date = parse_linelist_date(followup_date),
    registration_date = parse_linelist_date(registration_date),
    last_contact_with_source_date = parse_linelist_date(last_contact_with_source_date)
  )
message("  Contact tracing: ", nrow(ct_raw), " rows")

linelist <- load_linelist_properly(file.path(ct_dir, ct_cfg$data$files$linelist))
linelist$person_id <- trimws(as.character(linelist$person_id))
message("  Linelist: ", nrow(linelist), " rows, ", dplyr::n_distinct(linelist$person_id), " people")

# Imputed onset (optional): joined on alert_id, since the step 2 output has no
# person_id column. Collapsed per alert_id first so a duplicated alert_id
# can't duplicate linelist rows.
imputed_path <- file.path(data_root, "outputs", run_date, ct_cfg$data$files$linelist_with_imputed_onset)
if (file.exists(imputed_path)) {
  imputed <- read.csv(imputed_path, stringsAsFactors = FALSE, colClasses = "character",
                      na.strings = c("", "NA", "N/A")) |>
    dplyr::group_by(alert_id) |>
    dplyr::summarise(
      onset_imputed = collapse_values(as.character(parse_linelist_date(date_of_symptom_onset_imputed))),
      onset_was_imputed = collapse_values(onset_date_was_imputed),
      .groups = "drop"
    )
  linelist <- dplyr::left_join(linelist, imputed, by = "alert_id")
  message("  Imputed onset dates loaded from outputs/", run_date)
} else {
  warning("No imputed onset file for ", run_date, " (", imputed_path, "). ",
          "Run the epi linelist pipeline steps 1-2 for this date to fill the ",
          "*_onset_date_imputed columns; they will be NA for now.", call. = FALSE)
  linelist$onset_imputed <- NA_character_
  linelist$onset_was_imputed <- NA_character_
}

# --------------------- Person-level linelist summary --------------------------
message("")
message("B. SUMMARISING LINELIST PER PERSON")

person_summary <- linelist |>
  dplyr::mutate(
    classification = ifelse(is.na(final_mve_case_classification) | final_mve_case_classification == "",
                            "blank", final_mve_case_classification)
  ) |>
  dplyr::group_by(person_id) |>
  dplyr::summarise(
    n_linelist_rows = dplyr::n(),
    onset_date = collapse_values(as.character(date_of_symptom_onset)),
    onset_date_imputed = collapse_values(onset_imputed),
    onset_was_imputed = collapse_values(onset_was_imputed),
    age = collapse_values(age, numeric = TRUE),
    sex = collapse_values(sex),
    final_linelist_classification = collapse_values(classification),
    .groups = "drop"
  ) |>
  # Conflict = any compared field has more than one value. Imputed onset is
  # deliberately left out: two rows both missing onset get independently
  # sampled imputed dates, which would look like a conflict but isn't one.
  dplyr::mutate(
    has_conflict = n_linelist_rows > 1 & (
      grepl("/", onset_date) | grepl("/", age) | grepl("/", sex) |
        grepl("/", final_linelist_classification)
    ) %in% TRUE
  )

message("  People with >1 linelist row: ", sum(person_summary$n_linelist_rows > 1),
        " (", sum(person_summary$has_conflict), " with conflicting values)")

# ------------------------------ Build pairs -----------------------------------
message("")
message("C. BUILDING PAIRS")

ct <- ct_raw |>
  dplyr::filter(!is.na(source_case_person_id), !is.na(contact_person_id)) |>
  dplyr::rename(infector_person_id = source_case_person_id,
                infectee_person_id = contact_person_id)
n_missing_ids <- nrow(ct_raw) - nrow(ct)
if (n_missing_ids > 0) message("  Dropped ", n_missing_ids, " rows with a missing person ID")

# Self-pairs (infector == infectee) are ID-matching errors: drop, but log.
self_pairs <- ct |>
  dplyr::filter(infector_person_id == infectee_person_id) |>
  dplyr::distinct(infector_person_id, infectee_person_id)
ct <- dplyr::filter(ct, infector_person_id != infectee_person_id)
message("  Dropped ", nrow(self_pairs), " self-pairs (infector = infectee)")

# Final follow-up result per pair: latest dated row(s) with a result. If no
# row with a result has a date, all results are used.
final_result <- ct |>
  dplyr::filter(!is.na(followup_result)) |>
  dplyr::group_by(infector_person_id, infectee_person_id) |>
  dplyr::filter(if (all(is.na(followup_date))) TRUE else followup_date %in% max(followup_date, na.rm = TRUE)) |>
  dplyr::summarise(
    infectee_final_followup_result = collapse_values(followup_result),
    final_followup_result_date = collapse_values(as.character(followup_date)),
    .groups = "drop"
  )

# Infector's first record as a source case: earliest registration_date of any
# of their contacts (an infector-level value, repeated on each of their pairs).
infector_first_record <- ct |>
  dplyr::group_by(infector_person_id) |>
  dplyr::summarise(
    infector_first_source_record_date = if (all(is.na(registration_date))) as.Date(NA) else min(registration_date, na.rm = TRUE),
    .groups = "drop"
  )

pairs <- ct |>
  dplyr::group_by(infector_person_id, infectee_person_id) |>
  dplyr::summarise(
    infector_id_status = collapse_values(source_case_person_id_status),
    infectee_id_status = collapse_values(contact_person_id_status),
    n_ct_rows = dplyr::n(),
    n_contact_ids = dplyr::n_distinct(contact_id, na.rm = TRUE),
    first_followup_date = if (all(is.na(followup_date))) as.Date(NA) else min(followup_date, na.rm = TRUE),
    last_followup_date = if (all(is.na(followup_date))) as.Date(NA) else max(followup_date, na.rm = TRUE),
    last_contact_with_source_date = collapse_values(as.character(last_contact_with_source_date)),
    contact_type = collapse_values(contact_type),
    relationship_to_case = collapse_values(relationship_to_case),
    case_circumstances = collapse_values(case_circumstances),
    contact_status = collapse_values(contact_status),
    .groups = "drop"
  ) |>
  dplyr::left_join(final_result, by = c("infector_person_id", "infectee_person_id")) |>
  dplyr::left_join(infector_first_record, by = "infector_person_id")

# ------------------------ Attach linelist info --------------------------------
add_person_info <- function(df, role) {
  info <- person_summary
  names(info) <- paste0(role, "_", names(info))
  names(info)[1] <- paste0(role, "_person_id")
  df <- dplyr::left_join(df, info, by = paste0(role, "_person_id"))
  df[[paste0(role, "_in_linelist")]] <- df[[paste0(role, "_person_id")]] %in% person_summary$person_id
  df[[paste0(role, "_has_conflict")]] <- df[[paste0(role, "_has_conflict")]] %in% TRUE
  df
}

pairs <- pairs |>
  add_person_info("infector") |>
  add_person_info("infectee")

# Infectees with several infectors; pairs recorded in both directions.
pairs <- pairs |>
  dplyr::group_by(infectee_person_id) |>
  dplyr::mutate(infectee_n_infectors = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    reciprocal_pair = paste(infector_person_id, infectee_person_id) %in%
      paste(infectee_person_id, infector_person_id),
    contact_info_has_conflict = (
      grepl("/", infectee_final_followup_result) | grepl("/", contact_type) |
        grepl("/", relationship_to_case) | grepl("/", last_contact_with_source_date)
    ) %in% TRUE,
    pair_has_conflict = infector_has_conflict | infectee_has_conflict | contact_info_has_conflict
  )

# Requested columns first, extras after.
pairs <- pairs |>
  dplyr::select(
    infector_person_id, infector_in_linelist,
    infector_onset_date, infector_age, infector_sex, infector_final_linelist_classification,
    infectee_person_id, infectee_in_linelist,
    infectee_onset_date, infectee_age, infectee_sex, infectee_final_linelist_classification,
    infectee_final_followup_result,
    # extras
    infector_onset_date_imputed, infector_onset_was_imputed,
    infectee_onset_date_imputed, infectee_onset_was_imputed,
    infector_id_status, infectee_id_status,
    infector_first_source_record_date,
    infector_n_linelist_rows, infectee_n_linelist_rows,
    infectee_n_infectors, reciprocal_pair,
    infector_has_conflict, infectee_has_conflict, contact_info_has_conflict, pair_has_conflict,
    final_followup_result_date, n_ct_rows, n_contact_ids,
    first_followup_date, last_followup_date, last_contact_with_source_date,
    contact_type, relationship_to_case, case_circumstances, contact_status
  ) |>
  dplyr::arrange(infector_person_id, infectee_person_id)

message("  ", nrow(pairs), " pairs (", dplyr::n_distinct(pairs$infector_person_id), " infectors, ",
        dplyr::n_distinct(pairs$infectee_person_id), " infectees)")

# --------------------------- Data quality log ---------------------------------
message("")
message("D. DATA QUALITY LOG")

known_results <- ct_cfg$known_followup_results
unmapped_results <- ct |>
  dplyr::filter(!is.na(followup_result), !followup_result %in% known_results) |>
  dplyr::count(infector_person_id, infectee_person_id, followup_result, name = "n_rows") |>
  dplyr::transmute(issue = "unmapped_followup_result", infector_person_id, infectee_person_id,
                   detail = paste0("followup_result '", followup_result, "' (", n_rows, " rows)"))

dq_issues <- dplyr::bind_rows(
  self_pairs |>
    dplyr::transmute(issue = "self_pair_dropped", infector_person_id, infectee_person_id,
                     detail = "infector and infectee have the same person ID; pair excluded"),
  pairs |>
    dplyr::filter(reciprocal_pair) |>
    dplyr::transmute(issue = "reciprocal_pair", infector_person_id, infectee_person_id,
                     detail = "pair also recorded in the opposite direction; kept"),
  unmapped_results
)
message("  ", nrow(dq_issues), " issues logged: ",
        paste(names(table(dq_issues$issue)), table(dq_issues$issue), sep = " = ", collapse = ", "))

# -------------------------------- Write ---------------------------------------
pairs_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$pairs)
dq_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$dq_issues)
readr::write_csv(pairs, pairs_path, na = "")
readr::write_csv(dq_issues, dq_path, na = "")

message("")
message("Wrote ", pairs_path)
message("Wrote ", dq_path)
