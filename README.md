![Private](https://img.shields.io/badge/Repository-Private-red)
![De-identified](https://img.shields.io/badge/Data-De--identified-yellow)

# Ebola Bundibugyo DRC 2026 — Processing Code

Code-only repo for the downstream epi linelist pipeline: delay-distribution fitting, date imputation, aggregation, dashboard-plot generation, residence-vs-alert mapping/reporting, and nowcasting prep (steps 1-7 of the pipeline that starts in `BDBV2026-Linelist_Processing`). It contains no data of its own.

**Project leads:** Institut National de Recherche Biomédicale (INRB) / One Health Institute for Africa (INOHA) (Dav Ebengo, Placide Mbala-Kingebeni, Tania Bishola) and Institut National de Santé Publique (INSP) (Pierre Akilimali, Adelard Lofungola), in collaboration with the University of Oxford and Northeastern University. Contact: dav.ebengo@umie-inrb.org or pierre.akilimali@insp.cd.

---

## Code / data split

This repo holds only R scripts. All input and output data — the de-identified ("ctsafe") linelist/contact-tracing snapshots, the health-area shapefile, lookup maps, and every pipeline output (aggregates, nowcasting files, dashboard plots, residence maps/reports) — lives in the sibling repo [`BDBV2026-Processed_Sensitive_Data`](https://github.com/INRB-UMIE/BDBV2026-Processed_Sensitive_Data).

```
pipelines/
├── 0-setup.R                       # resolves code_root (this repo) and data_root (the data repo), loads packages/config
├── pipeline_configs.json
└── epi_linelist_pipeline/
    ├── 1-fit_delay_distributions.R # delay intervals + lognormal/weibull/gamma distribution fits (epidist/cmdstanr)
    ├── 2-date_imputation.R         # imputes missing/out-of-range onset dates by sampling step 1's best-fit delay distribution
    ├── 3-aggregate-linelist.R      # status + rolling positivity aggregation (national/province/health-zone), lab positivity table
    ├── 4-make-dashboard-plots.R    # ggplot2 SVGs + outputs/manifest.json (see "Automatic runs (CI)" below)
    ├── 5-map-residence-health-areas.R      # also sources 6- internally -- manual only, not run by CI (see below)
    ├── 6-residence-vs-alert-report.R       # manual only, not run by CI
    ├── 7-nowcasting_prep.R                 # manual only, not run by CI
    └── qc_codes.csv
```

Every path in these scripts is one of two roots, resolved once in `0-setup.R` and passed down:

- **`code_root`** — this repo (wherever it's checked out) — used for `pipelines/pipeline_configs.json`, `qc_codes.csv`, and sourcing the other step scripts.
- **`data_root`** — the `BDBV2026-Processed_Sensitive_Data` repo — used for everything under `data/` and `outputs/`.

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
Rscript pipelines/epi_linelist_pipeline/5-map-residence-health-areas.R   # manual only -- also runs 6-residence-vs-alert-report.R
Rscript pipelines/epi_linelist_pipeline/7-nowcasting_prep.R              # manual only
```

Each script resolves the latest synced `data/YYYY-MM-DD/dhis2_processed_linelist_ctsafe.csv` in the data repo automatically -- or a specific date instead, by setting an `EPI_PIPELINE_DATE` (YYYY-MM-DD) environment variable before running any step (e.g. to keep working against yesterday's sync while a newer one is still being checked, or to reproduce a past run). Every step sources `0-setup.R` first, so the override applies uniformly whichever step you start from.

Step 4 depends on step 3's output files existing on disk (`status_aggregated.csv`, `rolling_positivity.csv`, `lab_positivity_aggregated.csv`), and step 7 depends on step 1's (`delay_distributions/dhis2_linelist_with_delays.csv`), so run them in that order. Steps 5-7 currently have to be run manually (see "Automatic runs (CI)" below) -- note step 7 (nowcasting prep) itself runs after steps 4-6 rather than before, so those steps fall back to a static config default rather than step 7's freshly-computed run metadata when its output isn't there yet; if that ever matters, step 7 may need to move earlier instead.

## Automatic runs (CI)

`BDBV2026-Processed_Sensitive_Data`'s `.github/workflows/run-processing-code-pipeline.yml` runs **steps 1-4 only** automatically, on every push there that adds a new `data/*/dhis2_processed_linelist_ctsafe.csv` (i.e. every ctsafe sync from `BDBV2026-Linelist_Processing`). It checks out this repo, sets `BDBV_DATA_ROOT` explicitly (since CI checkouts aren't literal sibling folders), runs steps 1-4, and commits the results back to the data repo. Steps 5-7 (residence mapping, residence-vs-alert reporting, nowcasting prep) are deliberately not part of this automatic run yet -- run them manually as above, or extend the workflow's "Run pipeline steps 1-4" step if/when that changes.

The same workflow can also be triggered manually (Actions -> "Run Processing_Code pipeline" -> Run workflow) with an optional `pipeline_date` input, which sets `EPI_PIPELINE_DATE` for that run -- leave it blank to use the latest synced date, as usual.

Step 4 also writes a single stable `outputs/manifest.json` (overwritten every run with that run's date and dashboard-plot file locations) -- that file changing is what triggers `trigger-dashboard-rebuild.yml`, which in turn notifies `BDBV2026-Epidemic_Dashboard` to rebuild.

## License

MIT — see `LICENSE`.
