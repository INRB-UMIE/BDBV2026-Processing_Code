![Public](https://img.shields.io/badge/Repository-Public-brightgreen)
![De-identified](https://img.shields.io/badge/Data-De--identified-yellow)

# Ebola Bundibugyo DRC 2026 — Processing Code

Code-only repo for the downstream epi linelist pipeline: delay-distribution fitting, date imputation, aggregation, dashboard-plot generation, reporting-backfill (late-reporting) analysis, nowcasting prep, residence-vs-alert mapping/reporting, and a summary report (steps 1-8 of the pipeline that starts in `BDBV2026-Linelist_Processing`). It contains no data of its own.

**Project leads:** Institut National de Recherche Biomédicale (INRB) / One Health Institute for Africa (INOHA) (Dav Ebengo, Placide Mbala-Kingebeni, Tania Bishola) and Institut National de Santé Publique (INSP) (Pierre Akilimali, Adelard Lofungola), in collaboration with the University of Oxford and Northeastern University. Contact: dav.ebengo@umie-inrb.org or pierre.akilimali@insp.cd.

---

## Code / data split

This repo holds only R scripts. All input and output data — the de-identified ("ctsafe") linelist/contact-tracing snapshots, the health-area shapefile, lookup maps, and every pipeline output (aggregates, nowcasting files, dashboard plots, residence maps/reports) — lives in the sibling repo [`BDBV2026-Processed_Sensitive_Data`](https://github.com/INRB-UMIE/BDBV2026-Processed_Sensitive_Data).

```
pipelines/
├── 0-setup.R                       # resolves code_root (this repo) and data_root (the data repo), loads packages/config
├── pipeline_configs.json
├── upstream_pipeline/              # read-only mirror of the upstream cleaning/matching code (see below)
└── epi_linelist_pipeline/
    ├── 1-fit_delay_distributions.R # delay intervals + lognormal/gamma distribution fits (epidist/cmdstanr)
    ├── 2-date_imputation.R         # imputes missing/out-of-range onset dates by sampling step 1's best-fit delay distribution
    ├── 3-aggregate-linelist.R      # status + rolling positivity aggregation (national/province/health-zone), lab positivity table
    ├── 4-make-dashboard-plots.R    # ggplot2 SVGs + outputs/manifest.json (see "Automatic runs (CI)" below)
    ├── 5-map-residence-health-areas.R      # also sources 8- internally -- manual only, not run by CI (see below)
    ├── 6-reporting-backfill.R              # how complete recent daily counts are, from past exports; updates outputs/across_exports/ -- run by CI
    ├── 7-nowcasting_prep.R                 # manual only, not run by CI
    ├── 8-residence-vs-alert-report.R       # manual only, not run by CI (normally run via step 5)
    ├── summary-report.Rmd                  # PDF summary of steps 1-4 (writes 8-summary-report.pdf) -- run by CI
    └── qc_codes.csv
```

Every path in these scripts is one of two roots, resolved once in `0-setup.R` and passed down:

- **`code_root`** — this repo (wherever it's checked out) — used for `pipelines/pipeline_configs.json`, `qc_codes.csv`, and sourcing the other step scripts.
- **`data_root`** — the `BDBV2026-Processed_Sensitive_Data` repo — used for everything under `data/` and `outputs/`.

## Upstream pipeline (`pipelines/upstream_pipeline/`)

A read-only copy of the upstream code that cleans the raw exports and matches people across them (column standardisation, de-duplication, person matching and the link classifier steps, quality checks). It is copied automatically from the private `BDBV2026-Secure_Raw_Data` repo whenever that code changes. A check blocks the copy if it finds case IDs, real names or secrets. Do not edit it here, because changes are overwritten. See `pipelines/upstream_pipeline/README.md`. None of the steps here source it.

## Finding the data repo

`resolve_data_root()` in `pipelines/0-setup.R` resolves `data_root` in this order:

1. **`BDBV_DATA_ROOT` environment variable**, if set — an explicit override. Used in CI, where the two repos are checked out into subdirectories of a shared workspace rather than true sibling folders.
2. **A sibling folder** next to wherever this repo is checked out, named either `BDBV2026-Processed_Sensitive_Data` (the usual local folder name) or `BDBV2026-Contact_Tracing_Analysis` (that repo's GitHub name), containing a `data/` directory.

For local development, clone the two repos side by side and everything resolves with no configuration:

```
some-folder/
├── BDBV2026-Processing_Code/            # this repo
└── BDBV2026-Processed_Sensitive_Data/   # sibling — data/, outputs/, assets/
```

## Running the pipeline

**Prerequisites:** R 4.x with the packages listed in `pipelines/0-setup.R` (auto-installed on first run), plus a working `sf`/GDAL install (`libudunits2-dev libgdal-dev libgeos-dev libproj-dev` on Ubuntu, or the equivalent for your OS).

From this repo's root, with `BDBV2026-Processed_Sensitive_Data` cloned as a sibling folder:

```bash
Rscript pipelines/epi_linelist_pipeline/1-fit_delay_distributions.R
Rscript pipelines/epi_linelist_pipeline/2-date_imputation.R
Rscript pipelines/epi_linelist_pipeline/3-aggregate-linelist.R
Rscript pipelines/epi_linelist_pipeline/4-make-dashboard-plots.R
Rscript pipelines/epi_linelist_pipeline/5-map-residence-health-areas.R   # manual only -- also runs 8-residence-vs-alert-report.R
Rscript pipelines/epi_linelist_pipeline/6-reporting-backfill.R           # needs step 3's status_aggregated.csv, for this and past dates
Rscript pipelines/epi_linelist_pipeline/7-nowcasting_prep.R              # manual only
```

Each script resolves the latest synced `data/YYYY-MM-DD/dhis2_processed_linelist_ctsafe.csv` in the data repo automatically -- or a specific date instead, by setting an `EPI_PIPELINE_DATE` (YYYY-MM-DD) environment variable before running any step (e.g. to keep working against yesterday's sync while a newer one is still being checked, or to reproduce a past run). Every step sources `0-setup.R` first, so the override applies uniformly whichever step you start from.

Step 4 depends on step 3's output files existing on disk (`status_aggregated.csv`, `rolling_positivity.csv`, `lab_positivity_aggregated.csv`), and step 7 depends on step 1's (`delay_distributions/dhis2_linelist_with_delays.csv`), so run them in that order. Step 6 needs step 3's `status_aggregated.csv` for this run's date (plus every earlier `outputs/<DATE>/` folder it can find, to compare against). Steps 5, 7 and 8 currently have to be run manually (see "Automatic runs (CI)" below) -- note step 7 (nowcasting prep) itself runs after steps 4-6 rather than before, so those steps fall back to a static config default rather than step 7's freshly-computed run metadata when its output isn't there yet; if that ever matters, step 7 may need to move earlier instead.

## Contact tracing pipeline

`pipelines/contact_tracing_pipeline/` (manual only, not run by CI). Uses the latest `data/YYYY-MM-DD/` folder that has both `contact_tracing_combined_ctsafe.csv` and the ctsafe linelist; pin a date with `CT_PIPELINE_DATE`. Outputs go to `outputs/YYYY-MM-DD/contact_tracing/`.

```bash
Rscript pipelines/contact_tracing_pipeline/1-make-infector-infectee-pairs.R  # one row per infector-infectee pair + DQ issues log
Rscript pipelines/contact_tracing_pipeline/2-pairs-summary-table.R           # counts: pairs by linelist presence, conflicts
Rscript pipelines/contact_tracing_pipeline/3-pairs-plots.R                  # ggplot2 SVGs + PNGs in outputs/YYYY-MM-DD/contact_tracing/plots/
```

Where duplicate records disagree, all distinct values are kept, sorted and joined with `/`. Imputed onset dates are filled only if the epi pipeline's step 2 has run for the same date.

## Automatic runs (CI)

`BDBV2026-Processed_Sensitive_Data`'s `.github/workflows/run-processing-code-pipeline.yml` runs **steps 1-4, step 6 and the summary report** automatically, on every push there that adds a new `data/*/dhis2_processed_linelist_ctsafe.csv` (i.e. every ctsafe sync from `BDBV2026-Linelist_Processing`). It checks out this repo, sets `BDBV_DATA_ROOT` explicitly (since CI checkouts aren't literal sibling folders), runs steps 1-4, then step 6 (allowed to fail without blocking the rest, as its outputs are diagnostic only), then renders the summary report, and commits the results back to the data repo. Steps 5, 7 and 8 (residence mapping, nowcasting prep, residence-vs-alert reporting) are deliberately not part of this automatic run yet -- run them manually as above, or extend the workflow if/when that changes.

The same workflow can also be triggered manually (Actions -> "Run Processing_Code pipeline" -> Run workflow) with an optional `pipeline_date` input, which sets `EPI_PIPELINE_DATE` for that run -- leave it blank to use the latest synced date, as usual. The `not_a_case` delay distribution fits in step 1 are **skipped by default** (on push-triggered runs and local runs alike) for speed; the `cases` fits used for date imputation always run. To fit `not_a_case` too, set `SKIP_NOT_A_CASE_DELAY_FIT=false` locally, or untick the `skip_not_a_case_delay_fit` checkbox on a manual run.

Step 1 also excludes delays longer than `nowcasting.delay.max_days` (60 days) from the fits, and sets the number of parallel fit workers from the machine's core count (`delay_distribution_fit.fit_workers` in `pipeline_configs.json` is only an upper cap; `N_CORES_OVERRIDE` overrides the detected core count).

Step 4 also writes a single stable `outputs/manifest.json` (overwritten every run with that run's date and dashboard-plot file locations) -- that file changing is what triggers `trigger-dashboard-rebuild.yml`, which in turn notifies `BDBV2026-Epidemic_Dashboard` to rebuild.

## License

MIT — see `LICENSE`.
