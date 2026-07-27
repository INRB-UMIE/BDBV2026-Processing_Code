#!/usr/bin/env Rscript
# Gate 2: a seeded fit must be bit-identical run in the main session vs a
# multisession worker. Uses the committed 2026-07-20 linelist so no upstream
# data is needed. Run inside the container on the runner.
source("pipelines/0-setup.R")

epi_dir <- file.path(data_root, "outputs", "2026-07-20", "delay_distributions")
df <- load_linelist_properly(
  file.path(epi_dir, "dhis2_linelist_with_delays.csv"),
  date_columns = pipeline_configs$epi_linelist$qc$date_columns
)

cutoff <- as.Date(pipeline_configs$epi_linelist$delay_distribution_fit$onset_cutoff_start)
eligible <- df |>
  dplyr::filter(final_mve_case_classification == "confirmed_case") |>
  dplyr::filter(date_of_symptom_onset > cutoff, date_of_symptom_onset < Sys.Date()) |>
  dplyr::filter(!is.na(onset_to_reporting_days), onset_to_reporting_days >= 0)

seed <- epidist_fit_seed("onset_to_reporting_days", "gamma")

fit_once <- function() {
  eligible |>
    epidist::as_epidist_linelist_data(
      pdate_lwr = "date_of_symptom_onset", sdate_lwr = "reporting_date"
    ) |>
    epidist::as_epidist_marginal_model() |>
    epidist::epidist(
      formula = mu ~ 1, family = stats::Gamma(link = "log"),
      chains = 4, cores = 4, iter = 2000, backend = "cmdstanr", seed = seed
    )
}

main_fit <- fit_once()
future::plan(future::multisession, workers = 1)
worker_fit <- furrr::future_map(1, ~ fit_once(),
  .options = furrr::furrr_options(seed = TRUE))[[1]]
future::plan(future::sequential)

d1 <- posterior::as_draws_matrix(main_fit)
d2 <- posterior::as_draws_matrix(worker_fit)
stopifnot("seeded fit not reproducible across sessions" = isTRUE(all.equal(d1, d2)))
cat("GATE2 OK\n")
