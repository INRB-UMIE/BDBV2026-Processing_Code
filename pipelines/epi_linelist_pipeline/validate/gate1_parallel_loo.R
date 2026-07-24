#!/usr/bin/env Rscript
# Gate 1: parallel_loo() must equal brms::loo() on a fixed committed fit
# (deterministic -- no RNG). Run inside the container on the runner.
source("pipelines/0-setup.R")

fit_path <- file.path(
  data_root, "outputs", "2026-07-20", "delay_distributions",
  "onset_to_reporting_days", "gamma_fit.rds"
)
stopifnot("committed gamma_fit.rds not found" = file.exists(fit_path))
fit <- readRDS(fit_path)

serial   <- brms::loo(fit)
parallel <- parallel_loo(fit, cores = N_CORES_AVAILABLE)

s_elpd <- serial$estimates["elpd_loo", "Estimate"]
p_elpd <- parallel$estimates["elpd_loo", "Estimate"]
s_se   <- serial$estimates["elpd_loo", "SE"]
p_se   <- parallel$estimates["elpd_loo", "SE"]

cat(sprintf("serial  elpd=%.6f se=%.6f\n", s_elpd, s_se))
cat(sprintf("parallel elpd=%.6f se=%.6f\n", p_elpd, p_se))

stopifnot("elpd_loo mismatch > 1e-6" = abs(s_elpd - p_elpd) < 1e-6)
stopifnot("elpd SE mismatch > 1e-3 (chain_id/r_eff suspect)" = abs(s_se - p_se) < 1e-3)
cat("GATE1 OK\n")
