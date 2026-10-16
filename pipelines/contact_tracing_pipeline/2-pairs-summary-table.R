#!/usr/bin/env Rscript
# Step 2: Summary table of infector-infectee pairs.
#   - Reads step 1's outputs/<DATE>/contact_tracing/infector_infectee_pairs.csv
#     (and the DQ issues file, for the number of self-pairs dropped)
#   - Writes outputs/<DATE>/contact_tracing/infector_infectee_summary.csv: one
#     row per metric, with counts
#
# "Conflicting" = duplicate records disagree (i.e. a "/" in that value in the
# pairs table). Two independent sources:
#   - linelist info: a person's duplicate linelist rows disagree on onset
#     date, age, sex or classification
#   - contact tracing info: a pair's contact tracing rows disagree on final
#     follow-up result, contact type, relationship or last contact date
# Infector/infectee rows count unique people ("contact tracing" = at least
# one of their pairs has a contact tracing conflict). Pair rows count pairs.

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

ct_cfg <- pipeline_configs$contact_tracing

ct_dir <- resolve_ct_latest_dir()
run_date <- basename(ct_dir)
ct_outputs_dir <- file.path(data_root, "outputs", run_date, ct_cfg$data$outputs_subdir)

message("======== Infector-infectee summary: ", run_date, " =========")

pairs_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$pairs)
if (!file.exists(pairs_path)) {
  stop("Pairs file not found: ", pairs_path,
       ". Run 1-make-infector-infectee-pairs.R first.", call. = FALSE)
}
pairs <- readr::read_csv(pairs_path, col_types = readr::cols(.default = "c")) |>
  dplyr::mutate(dplyr::across(
    c(infector_in_linelist, infectee_in_linelist, infector_has_conflict,
      infectee_has_conflict, contact_info_has_conflict, pair_has_conflict),
    ~ .x == "TRUE"
  ))

dq_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$dq_issues)
n_self_pairs <- if (file.exists(dq_path)) {
  sum(readr::read_csv(dq_path, col_types = readr::cols(.default = "c"))$issue == "self_pair_dropped")
} else {
  NA_integer_
}

# -------------------------------- Summary -------------------------------------
# Number of unique people (infectors or infectees) with at least one pair
# where `flag` is TRUE.
count_people <- function(ids, flag) dplyr::n_distinct(ids[flag])

summary_table <- tibble::tribble(
  ~metric, ~n, ~notes,
  "Total infector-infectee pairs",
    nrow(pairs), "Unique pairs of person IDs, after removing self-pairs",
  "Pairs: only infector in linelist",
    sum(pairs$infector_in_linelist & !pairs$infectee_in_linelist), "",
  "Pairs: only infectee in linelist",
    sum(!pairs$infector_in_linelist & pairs$infectee_in_linelist), "",
  "Pairs: both in linelist",
    sum(pairs$infector_in_linelist & pairs$infectee_in_linelist), "",
  "Pairs: neither in linelist",
    sum(!pairs$infector_in_linelist & !pairs$infectee_in_linelist), "The four rows above add up to the total",
  "Infectors with conflicting linelist info",
    count_people(pairs$infector_person_id, pairs$infector_has_conflict),
    "Unique people; their duplicate linelist rows disagree",
  "Infectors with conflicting contact tracing info",
    count_people(pairs$infector_person_id, pairs$contact_info_has_conflict),
    "Unique people; at least one of their pairs has disagreeing contact tracing rows",
  "Infectors with any conflicting info",
    count_people(pairs$infector_person_id, pairs$infector_has_conflict | pairs$contact_info_has_conflict),
    "Unique people; either of the two rows above",
  "Infectees with conflicting linelist info",
    count_people(pairs$infectee_person_id, pairs$infectee_has_conflict),
    "Unique people; their duplicate linelist rows disagree",
  "Infectees with conflicting contact tracing info",
    count_people(pairs$infectee_person_id, pairs$contact_info_has_conflict),
    "Unique people; at least one of their pairs has disagreeing contact tracing rows",
  "Infectees with any conflicting info",
    count_people(pairs$infectee_person_id, pairs$infectee_has_conflict | pairs$contact_info_has_conflict),
    "Unique people; either of the two rows above",
  "Pairs with conflicting contact tracing info",
    sum(pairs$contact_info_has_conflict),
    "Duplicate contact tracing rows disagree (follow-up result, contact type, relationship, last contact date)",
  "Pairs with any conflicting info",
    sum(pairs$pair_has_conflict), "Infector, infectee or contact tracing info conflicts",
  "Self-pairs removed",
    n_self_pairs, "Infector and infectee had the same person ID; see DQ issues file"
) |>
  dplyr::mutate(run_date = run_date, .before = 1)

print(as.data.frame(summary_table[, c("metric", "n")]), row.names = FALSE)

summary_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$summary)
readr::write_csv(summary_table, summary_path, na = "")
message("")
message("Wrote ", summary_path)
