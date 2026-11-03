# Data quality checks

Watches the DHIS2 exports over time and flags when they change in ways that could
quietly break the pipeline or the analysis: columns going missing, being renamed
or added, the shape of the data shifting, or the mix of values (e.g. share of
confirmed cases) jumping from one export to the next.

Covers the **epi linelist** and **contact tracing** exports. Lab files are out of
scope.

Every output here is counts, percentages, column names, and the values of small
category columns (e.g. `POS`/`NEG`). **No patient names or other row-level data
are ever written** (see "What is stored" below), so these files are safe to keep
in git or share.

---

## Status

| Stage | What | Status |
|---|---|---|
| 1 | Column checks on each raw export | **built** (`1-raw-export-columns.R`) |
| 2 | Structure checks on each raw export (rows, % blank, types, date window, unknown category values) | **built** (`2-raw-export-structure.R`) |
| 3 | Growing metrics history (counts and shares in the cleaned data) + share-jump flags + charts | **built** (`3-metrics-history.R`, `plot-metrics.R`) |
| 4 | Shiny **Data Quality** tab (`shiny/server_quality.R`) | **built**; PDF report section still planned |

---

## Decisions

| Topic | Decision |
|---|---|
| Datasets | Epi linelist and contact tracing. No lab. |
| Export date | Read from the file name, `_DDMMYYYY` (e.g. `LINELIST_01072026.xls` = 1 July 2026). |
| Undated files | Left out (`LINELIST_02` … `_07`, from 7–9 June). History starts 9 June. |
| Incremental | Each run reads only exports not seen before, or whose contents changed (MD5 fingerprint). Old exports are never re-read unless `QC_REBUILD=1`. Comparisons use the stored history, so old raw files do not need to be on disk. |
| Critical problem | A critical column missing from the **latest** export stops the pipeline, before any cleaning runs. Everything else is recorded only. |
| Critical columns | Set in `pipeline_configs.json` → `quality_checks.critical_columns`. Epi: alert_id, case classification, reporting date, onset date, health zone (via the org-unit hierarchy), name, sex, age. CT: contact ID, source case alert ID, contact name, registration date, follow-up date, follow-up result, health zone. |
| Proportions tracked (stage 3) | Case classification, lab result, health zone, province, sex, age group, outcome. |
| Rows compared (stage 3) | All rows in each export, and separately only rows new since the previous export (by alert_id, or the assigned ID for rows without one; CT: by contact_id). |
| Proportion alert (stage 3) | Share moves > 10 percentage points **and** the change is unlikely to be chance (two-proportion test, p < 0.01). Tune later. |
| Where results show | CSV files (now), Shiny tab and PDF run report (stage 4). The PDF report revamp is a separate piece of work. |

### Contact tracing: two levels

Since 27 Aug each CT export holds only ~4 weeks (rolling window by registration
date); `contact_tracing_pipeline/2b-combine-exports.R` joins them into an
all-time table. So CT is checked twice:

1. **Each raw export, before cleaning** — the DHIS2 format: columns (stage 1),
   then rows, blanks, types and the registration-date window, including a gap
   between consecutive exports' windows (a gap means contacts can never reach
   the combined table) (stage 2).
2. **The combined table, after step 2b** — proportions (all-time, and contacts
   first seen in the latest export), plus counts of `missing_from_latest` and
   `source_case_changed` contacts (stage 3). Proportions are not measured on a
   single raw CT export: its window moves daily, so shares shift for no real
   reason.

### Known events

`known_events.csv` lists changes we already know about (e.g. 27 Aug: CT switched
to a rolling window). A finding that matches an event (same dataset and export
date, and the same check / column if the event names one) is marked
`status = expected` instead of `new`, and the event will be drawn on the plots.
Add a row whenever DHIS2 is changed on purpose.

---

## Stage 1: column checks (`1-raw-export-columns.R`)

Runs first in the full pipeline, before any cleaning. For each dated raw export:

1. Reads the export the way cleaning does (`readxl::read_excel`, which skips
   leading blank rows, or `read.csv`); headers are tidied with the cleaning
   step's own `trim_names()`.
2. Records each column's raw name, position, and the standard name cleaning will
   give it. The standard name comes from the **cleaning script's own functions**
   (`find_explicit_rename()`, `standardize_column_name()`), parsed out of
   `<pipeline>/1-colname-standard.R`, so the two can't drift apart.
   `mapping = explicit` means the column is in `data/maps/*_column_renames.csv`;
   `builtin` means the cleaning script's own hard-coded replacements rename it;
   `fallback` means cleaning made up a name from the raw label.
3. Profiles each column's **contents**: its type (date / number / text /
   blank), its share of blank cells, the number of distinct values, and — only
   for small category columns — the values themselves.
4. Compares each export with the previous dated export of the same dataset.

### Renames: names and contents

A column that disappeared and one that appeared are paired as a rename when:

1. the rename map gives both the same standard name (`renamed_mapped`), or
2. their names are similar, or one contains the other (`likely_renamed`, basis *name*), or
3. their names are unalike but both are category columns holding the same values:
   ≥ 80% overlap and ≥ 3 shared values (`likely_renamed`, basis *contents*).

Every rename finding then gets a **contents** verdict, comparing the old column
(in the previous export) with the new one:

| Verdict | Meaning |
|---|---|
| `match` | Same type, similar blank share, and the stored values overlap ≥ 50% |
| `consistent` | Same type and similar blank share; no stored values to compare (dates, numbers, free text) |
| `differ` | Different type, blank shares more than 30 points apart, or values overlapping < 50%. **Check this rename by hand.** A `renamed_mapped` that differs becomes a warning. |
| `unknown` | Both columns entirely blank, or no profile (exports checked before contents were added: rerun with `QC_REBUILD=1`) |

### What is stored

Column values are stored **only** for a column with at most 20 distinct values,
each seen at least 5 times and at most 30 characters long, whose raw name doesn't
suggest personal data (`nom`, `name`, `téléphone`, `adresse`, `chef`,
`contaminante`, `coordonn`). Settings in `pipeline_configs.json` →
`quality_checks.columns.contents`. Names, phone numbers, addresses, IDs and free
text never qualify; `tests/test_quality_checks.py` checks the stored file
obeys this.

| Check | Severity | Meaning |
|---|---|---|
| `critical_missing` | critical | A critical column is absent. On the latest export this stops the pipeline. |
| `read_failed` | critical | The header could not be read. |
| `column_missing` | warning | In the previous export, not in this one, and no likely replacement. |
| `likely_renamed` | warning | A column disappeared and another appeared that looks like it by name (≥ 0.8 similarity after stripping DHIS2 prefixes, or one name containing the other, the shorter ≥ 8 characters) or by contents (see above). The rename map doesn't send both to the same name. |
| `renamed_mapped` | info (warning if contents differ) | Raw label changed but both labels map to the same standard name — cleaning copes. |
| `new_column_unmapped` | warning | New column with no rename (map or built-in); cleaning will give it a made-up name. |
| `new_column` | info | New column that the rename map or the built-in replacements already know. |
| `duplicate_column` | warning | Two columns with the same raw name in one export. |

### Outputs (`data/processed/quality_checks/`)

| File | One row per |
|---|---|
| `raw_export_files.csv` | export: dataset, file, export date, MD5, n rows, n columns, read ok / error, when checked |
| `raw_export_columns.csv` | column in each export: position, raw name, standard name, mapping, type, % blank, n distinct, values (category columns only) |
| `raw_export_column_findings.csv` | finding: dataset, file, export date, previous file, check, severity, column, detail, contents verdict, status |

`raw_export_files.csv` and `raw_export_columns.csv` are the history: rows are
added for new exports only. Standard names and mappings in the history are
recomputed every run from the current rename map, so a rename-map edit applies
to past exports straight away (no rebuild needed). The findings file is recomputed in full from that
history on every run (it's cheap), so a change to the rules or to
`known_events.csv` applies to past exports straight away.

### Running it

```bash
Rscript pipelines/quality_checks/1-raw-export-columns.R            # new exports only
QC_REBUILD=1 Rscript pipelines/quality_checks/1-raw-export-columns.R  # re-read every export
```

From the full pipeline it runs as step `[1b/10]`. Set `QC_NO_STOP=1` to record
critical findings without stopping (e.g. to look at a broken export).

## Stage 2: structure checks (`2-raw-export-structure.R`)

Compares each export with the previous one, from the profile stage 1 keeps --
no raw file is re-read, so it is recomputed in full every run. Runs at the end
of `1-raw-export-columns.R`. **All warnings: none stops the pipeline.**
Thresholds are in `pipeline_configs.json` -> `quality_checks.structure`.

| Check | Severity | Flags when |
|---|---|---|
| `row_change` | warning | rows change by more than 20% |
| `rows_dropped` | warning | epi linelist only: any drop in rows |
| `blank_jump` | warning | a column's blank share rises by more than 20 points (10 for critical columns) |
| `column_now_empty` / `column_now_filled` | warning / info | a column goes from at least 1% filled to empty, or back |
| `type_change` | warning | a column's type changes (date / number / text) |
| `stale_export` | warning | the newest reporting date (epi) / registration date (CT) is more than 3 days (epi) / 7 days (CT: registrations arrive in weekly batches) before the file's date |
| `window_gap` | warning | CT: this export's earliest registration date is more than 1 day after the previous export's latest -- contacts registered in between never reach the combined table |
| `mostly_untranslated` | warning | at least 20% of a category column's values are not in its `data/maps/` table (repeats every export until the table is fixed) |
| `unknown_category_values` | warning | values not in the table that are new since the previous export and cover at least 1% of the column |
| `duplicate_ids` / `duplicate_rows` | warning | epi: more alert_ids on two rows than last time / exact duplicate rows rise by more than 1 point |

Which table goes with which column is `structure.value_maps`, kept in step with
the cleaning scripts by a test. Free-text columns (`structure.free_text`, e.g.
`health_worker_position`) are not checked for unknown values.

Output: `raw_export_structure_findings.csv`, same columns as the column
findings. Stage 1 now also stores, per column, its earliest/latest plausible
date and its untranslated values (codes only, at most 10, each at most 30
characters), and per export, duplicate rows and duplicate alert_ids. **After
updating, run `QC_REBUILD=1` once** so the old exports get these too.

## Stage 3: metrics history (`3-metrics-history.R`, `plot-metrics.R`)

A **growing** file of counts and shares in the cleaned data, one set per
export, for plotting and for flagging sudden shifts. Runs after cleaning (and
after the CT combine) in the full, epi, CT and CT-combine jobs; never fatal.
Incremental: only new or changed processed exports are read
(`metrics_sources.csv`); `QC_REBUILD=1` recomputes everything.

`metrics_history.csv` is long format -- one row per export x subset x measure
x category:

| column | meaning |
|---|---|
| `dataset` | `epi_linelist`; `contact_tracing` (each processed export, its own window); `contact_tracing_combined` (the all-time table, one snapshot per build -- **starts the first time this runs**, as the combined table is rebuilt in place) |
| `subset` | `all`, or `new`: alert_ids / contact_ids not in the previous export (combined: first seen in its latest export) |
| `measure`, `category` | e.g. `case_classification` / `confirmed_case`. Blank values are `(blank)`. Counts (`n_rows`, `n_alert_ids`, `n_assigned_ids`, `n_new_rows`, `n_contacts`, `n_new_contacts`, `n_source_case_changed`) have no category |
| `n`, `denominator`, `share` | count, rows (or contacts) it is out of, and n / denominator |

Measures -- epi: case classification, GeneXpert result, sex, age group, type of
alert, status at sampling, type of alert x status at sampling, province, health
zone. CT: follow-up result (per follow-up row), sex, age group, health zone
(per contact); combined also contact status. Age groups 0-4, 5-14, 15-24,
25-44, 45-64, 65+ (`metrics.age_bands`), plus `(blank)` and `(invalid)`.

**Every column** (added 2026-10-30): on top of those, one measure per column
of the processed file, `col.<column>`, subset `all` only. Small category
columns get the share of each value plus `(blank)` (same privacy rule as stage
1's stored values: at most 20 values, each at most 30 characters; values seen
fewer than 5 times pooled as `(rare values)`); everything else -- IDs, dates,
numbers, free text, and any `pii_columns` column or one whose name looks
personal (`QM_COL_EXCLUDE`) -- gets only `(filled)` / `(blank)`. Old processed
exports are read with `data/maps/processed_column_aliases.csv`, so a renamed
column is one continuous history. Not drawn in the nightly PNGs; pick any in
the app's Metrics tab. Run once with `QC_REBUILD=1` to fill them in for past
exports (the combined table's past snapshots are kept, not recomputed).

**Flags** (`metrics_findings.csv`, warnings): a share that moved by more than
10 points **and** is unlikely to be chance (two-proportion test, p < 0.01),
against the previous export (`share_change`) and against the pooled previous 7
exports (`share_drift`, catches slow creep). Only subset `all` is flagged: the
day's new rows shift for real reasons. Known events apply.

**Charts** (`plot-metrics.R`, run at the end): `metrics_<dataset>.png/.pdf` --
rows over time, then one panel per measure with each category's share, lines
labelled at their end, red rings on flagged shares; a category absent from an
export is drawn at 0, so a code turning into a label shows as one line falling
as another rises; GeneXpert on a log10 scale. `metrics_<dataset>_stacked.png/.pdf`
-- the same measures as bands stacked to 100% (the whole mix at a glance;
GeneXpert over tested rows only). `metrics_<dataset>_counts.png/.pdf` -- the
same bands in rows: the row trajectory divided up by category.
`metrics_epi_alert_type_to_sample_status.png/.pdf` -- Sankey
of type of alert -> status at sampling for the latest linelist export.

## Charts and the Shiny tab

All charts are drawn by `qc-plot-functions.R` -- functions that take data and
return ggplots. The nightly scripts (`plot-column-timeline.R`,
`plot-metrics.R`) save them as PNG / PDF; the app's **Data Quality** tab
(after Contact Tracing Cleanup) draws them live, so the two cannot drift.

The tab: **Today** (latest exports, a card per dataset and every finding on
them), **Findings** (all findings, filterable), **Columns** (the column
timeline), **Metrics** (any dataset / measure / view: lines, stacked 100%,
stacked counts; all or new rows), **Alert type vs status** (the Sankey, any
export). The Home tab shows a one-line status of the latest exports.

**Mark as expected** (Today and Findings): records a finding -- or every
finding of that check on that export -- with the user's name and a note in
`data/processed/quality_checks/known_events_app.csv`. That file lives with the
data rather than the code, so the next code install on the server does not
overwrite it, and it syncs to S3. All three stages read it alongside
`known_events.csv`; the tab applies it immediately.

### Python reference

`tests/reference/quality_checks.py` is the executable specification of the
comparison rules (same as the rest of `tests/reference/`), tested by
`tests/test_quality_checks.py`. If you change a rule in the R, change it there
too.
