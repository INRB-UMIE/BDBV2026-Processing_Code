# Upstream pipeline (read-only mirror)

The code that cleans the raw DHIS2 exports and matches people across them:
column standardisation and de-duplication of the epi linelist, the lab
linelist and the contact-tracing data; person matching (`identify_persons/`,
including the link classifier steps); quality checks; and the pipeline run
report. Its outputs feed the other folders in `pipelines/` in this repo.

**Do not edit here.** This folder is copied automatically from a private
repository each time the code changes there. Edits made here are overwritten.

**No data.** These scripts run on a secured server against data that is not
published. Nothing in this folder contains case data.

**Example names and IDs are invented.** The comments explain matching choices
with worked examples. The names and case IDs in them (e.g. `ZAWADI ZAWADI
MARIE`, `RDC-AAA-BBB-26-0001234-MVE`) were made up to replace real ones. Scores
and counts quoted next to them come from the real pairs, so an invented name
will not always reproduce the exact number shown.

Not included: design notes that discuss individual cases, superseded
(`legacy/`) code, one-off maintenance scripts, and data files.
