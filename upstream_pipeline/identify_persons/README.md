# `identify_persons/`

**Status: all four steps built** — name rarity, person candidates, link ledger
and person registry — and driven from the **Identify Persons** tab in the Shiny
app and the full pipeline run.

> **Tiers and review buckets are retired.** A link exists when its link score
> clears `link_score.min_keep`, in all three matchers (the two dedups and
> person candidates); a cluster is auto-accepted when its weakest link clears
> `link_score.auto_accept` and it holds no confirmed-case / non-case
> contradiction. The review buckets went on 2026-09-23 and the tiers (A
> near-certain, B probable, C possible, S single-token, EXACT) on 2026-10-03.
> Much of what follows records the measurements made while those existed;
> where it describes tiers or buckets, read it as history.

Everything that needs **both** the linelist and the contact tracing data lives
here. The three cleaning pipelines each own one file and know nothing about the
others; this one consumes their outputs and works out which records refer to the
same human.

---

## Why it is a separate pipeline

Cross-file identity work was previously bolted onto `contact_tracing_pipeline/`,
which gave the CT pipeline a linelist dependency it has no business having.
`4-build-id-lookup.R` and `5-finalise_id_lookup.R` read both files, and so did
`explore-linelist-ct-cohesion.R`. Both specs were filed there for want of a
better home.

The dependency direction is now one-way and explicit:

```
epi_linelist_pipeline/       clean one file  (+ within-file dedup)
contact_tracing_pipeline/    clean one file  + assign its own IDs
lab_linelist_pipeline/       clean one file
                    |
                    v
identify_persons/            rarity -> matching -> person_id
```

### What deliberately stays outside

- **`epi_linelist_pipeline/2-deduplicate_linelist.R`** — it compares linelist
  rows to linelist rows, needs only that file, and **11 of the 51 linelist
  export dates have no matching CT export**. Moving it here would make those
  dates undedupable. This pipeline consumes `duplicate_candidates.csv` rather
  than producing it.
- **CT's `1-colname-standard.R` and `2-assign-contact-ids.R`** — they read the
  linelist to corroborate a `source_case_alert_id` repair or fill, but what they
  produce is the CT export. Consulting another file is not the same as spanning
  both.

---

## Intended scripts

| | script | produces | status |
|---|---|---|---|
| 1 | `1-build_name_rarity.R` | the pooled name rarity table (below) | **built** |
| 2 | *matching* | candidate pairs across all identifier populations | plan |
| 3 | *resolution* | `persons`, `person_identifiers`, `person_roles` | plan |
| 4 | *writeback* | `person_id` into both exports and both ctsafe files | plan |

Run from the **Identify Persons** tab in the Shiny app
(`run_identify_persons_bg()` in `shiny/global.R`, UI in `shiny/ui_tabs.R`,
server logic in `shiny/server_identify.R`).


---

## Where link scoring is going

**Target.** Every link carries one score in `[0, 1]`:

```
score = w_name * name_score + w_corroborator * corroborator_score
```

with both components themselves in `[0, 1]`. The weights are
`matching.score_weights` in `pipeline_configs.json`, set at `0.5 / 0.5` as a
stated null hypothesis rather than a measurement, and `score_weights()` in
`0-setup.R` errors if they do not sum to 1 — a pair that quietly does not makes
a score recorded on one run incomparable with one recorded on another.

**Where it has got to.** The name side is being built first, in order:

1. **Collocations** — which tokens travel together (`de` + `dieu`) and must be
   scored as one unit rather than summed as if independent. **Done**: detected
   once, over the pooled corpus, in `1-build_name_rarity.R`, and written to the
   running table.
2. **Token rarity** — decompose every name into tokens and score each by how
   unusual it is across everyone known to the outbreak. **Done**: same table,
   same run, and every matching script now reads it instead of counting its own.
3. **Turning that into a `name_score` in `[0, 1]`** — open, and the next thing
   to settle. See below.

Steps 1 and 2 deliberately happen **before** either deduplication, so the same
rarity and the same collocations apply to both data types. That ordering is the
substance of the change, not a convenience: a token's rarity is a property of
the outbreak's whole name population, not of the spreadsheet a name arrived in.

**The open question in step 3.** The current `name_weight` is a sum of IDFs in
nats. It is unbounded above, it rises with the *number* of matched name parts as
much as with their rarity, and the median name scores 16.99 against a range of
3.57 to 56.90. So mapping it into `[0, 1]` is a modelling choice, not a
rescaling, and the candidates behave differently:

- **A percentile** against the corpus distribution. Already computed as
  `name_rarity_pct` for the review tiles, interpretable ("rarer than 80% of
  names"), and bounded by construction — but it is *uniform* by construction
  too, so it discards how much rarer, which is the part that carries the
  evidence.
- **A saturating transform** of the weight, e.g. `1 - exp(-weight / k)`. Keeps
  the ordering and the spacing of the nats scale, needs one constant `k` chosen
  against reviewed links.
- **A calibrated probability** — fit `P(same person | weight)` on the reviewed
  clusters. The most defensible, and the most demanding: it wants enough
  adjudicated links to fit against, and it has to be refitted as the corpus
  grows.

**And the tiers.** `A_near_certain` / `B_probable` / `C_possible` /
`S_single_token_name`, the seven link classes and the auto-accept floor are all
thresholds on the nats scale, each with a measurement behind it. The score is
being computed *alongside* them first, so the two can be compared on real links
before anything is switched over.

**Deliberately not chained onto the cleaning runs.** The rarity table needs both
exports, and 11 of the 51 linelist export dates have no CT counterpart — so a
cleaning run is the wrong trigger. Its own tab means it is run when both sides
are ready, and the status card says which two runs the current table came from.
It reads both exports and writes neither, so re-running is free.

Staging, risks and dependencies are in
`specs/2026-09-03-person-id-entity-resolution.md` §11.

---

## Reviewing the candidates

**Measured on the first real run: 2,658 clusters from 3,504 links.**

| | clusters | |
|---|---|---|
| 2 identifiers | 2,267 (85%) | |
| 3+ identifiers | 391 (14%) | 1,237 edges between them |
| spanning >1 classification | 316 | the new information this phase exists to find |
| flagged implausible | 951 | |

### The edge is the unit of decision, not the cluster

A cluster is a *consequence* of its edges, and on this data the consequence is
often wrong while every individual edge looks defensible. The largest cluster
holds 14 identifiers; the 11-identifier one is `MBALISI TAMBOLI ANNIE` /
`ANNIE TAMBOLI` / `ANIE CHANTAL TAMBOLI` / `Tamboli Anni Rosette` / `ANI CHANTAL`
and more, spread across six health zones — several people who share two very
common given names.

So the review model is:

1. every edge starts **kept**;
2. the reviewer **breaks** the wrong ones;
3. the person is whatever connected component survives.

A 14-identifier cluster can resolve into five people without anyone making a
five-way decision. Accept-or-reject on the whole cluster cannot express that:
it can only take all fourteen or discard all fourteen.

### Decisions taken 2026-09-05

**One graph interface for every cluster, including pairs.** A pair renders as
two tiles and one line, which adds little at size 2 — but one interface means
one thing to learn and one code path, and the reviewer never has to switch
mental models partway through a queue.

**The 170 `EXACT_same_id` pairs are auto-accepted and never queued.** Both sides
are literally the same identifier string (`ct_case:RDC-…` and `ll:RDC-…`), so
there is nothing to judge. Queuing them would ask a reviewer to confirm that a
string equals itself, which mostly trains people to click through without
looking. They are recorded as confirmed with `evidence = exact_id`.

**Multi-reviewer, one vote file per person (since 2026-10-03).** Marking a
cluster (Confirm / Unsure) appends one row per link to
`person_matching/reviewer_history/<reviewer>.csv`. These files belong to no
run, so new data never strands a reviewer's work: on load the Review tab fills
in their earlier votes from any run. A cluster counts as done for them only
when every link in it has their vote — if new data adds links to a cluster
they judged before, it comes back with their old calls filled in. One file
per person means two people working at once never overwrite each other.
Harmonise verdicts go the same way, to `harmoniser_history/<harmoniser>.csv`.

How votes become decisions (`3-build-link-ledger.R`, applied to every ledger
row on every build):

1. A harmoniser's verdict decides the link (`settled`), or parks it
   (`unsure` / `escalate`).
2. Else an existing harmoniser decision in the ledger stands.
3. Else, if at least `matching.ledger.consensus_min_reviewers` (2) reviewers
   voted on it with the cluster marked **confirmed**, all made the same call,
   and no confirmed vote says the opposite, the votes decide it
   (`decided_by = reviewer`, `decision_basis = reviewer_consensus`). Votes in
   clusters marked **unsure** are not counted and do not block. A single
   confirmed dissent sends the link to the harmoniser rather than being
   outvoted.
4. Else any votes leave it `awaiting_harmonisation` (an automatic accept
   stands only if every vote agrees with it).
5. Else it is whatever the algorithm says this run: clusters whose every
   link is an exact ID match are accepted, and since 2026-10-07 every
   same-ID link (a contact-tracing source case and the linelist case with the
   same `alert_id`) is accepted on its own, whatever else is in its cluster
   (`decision_basis = auto_exact_id`). A reviewer vote to break one sends it to
   the harmoniser (rule 4).
6. A link still undecided that this run does not propose becomes `inactive`
   (`decision_basis = no_longer_proposed`): kept in the ledger, but no longer
   under consideration. It neither joins nor flags identifiers in the
   registry, and comes back for review if a later run proposes it again.

A single reviewer is never authoritative.

**Decided links are not re-judged.** The Review and Harmonise tabs read the
ledger: a link already accepted or rejected (by the rule, reviewer consensus
or a harmoniser) is drawn greyed with its decision, cannot be clicked, and
gets no new votes. A cluster with nothing left open leaves the queue. The
decisions imported straight into the ledger on 3 Oct 2026 also have files in
`harmoniser_history/` (`import_old_matcher.csv`, `import_reviewers_23sep.csv`),
so every decision in the ledger can be rebuilt from a history file.

**person_id in the exports (since 2026-10-07).** Every registry build also
writes person ids into the CURRENT exports -- the latest linelist, the latest
contact-tracing export and the combined contact-tracing files, `_names` and
`_ctsafe` alike -- so people can be counted without joining the registry:

| File | Columns | Looked up from |
|---|---|---|
| linelist | `person_id`, `person_id_status` | `alert_id` |
| contact tracing | `contact_person_id`, `contact_person_id_status` | `contact_id` |
| | `source_case_person_id`, `source_case_person_id_status` | `source_case_alert_id` |

`_status` is `accepted` (nothing pending could change the grouping) or
`undecided` (a link still under review could). person_ids are pseudonymous, so
they travel with the ctsafe files. They can change: a merge keeps the lowest
id and supersedes the others, a split retires the id -- both recorded in
`person_registry_history.csv`. A source case and its linelist record share a
person_id once the same-ID link between them is accepted.

**Decisions are not reopened in the app** (agreed 2026-10-03). For the rare
case where new evidence shows a decision was wrong, correct it by hand:
append one row to a file in `harmoniser_history/` (e.g. `corrections.csv`,
same columns as the others) with the link's `link_id`, `node_a`, `node_b`,
the new `final_decision` (kept / broken), `final_cluster_verdict = settled`,
the reason in `final_reasons` / `final_note`, and the current date-time in
`recorded_on`. Then run the ledger step. A harmoniser's latest verdict always
wins, and the correction is recorded alongside the original rather than
replacing it.

Rendered with `visNetwork` (added to `req_pkgs` in `0-setup.R`): node tiles
carrying name, age, sex and zone; edge thickness by tier; click an edge to
break it and watch the cluster split.

### The cluster timeline

One lane per identifier, in the same order as the tiles, on a **shared** date
axis. The point of one shared axis rather than a timeline per tile is that
dates only argue for or against a merge when they can be compared: two lanes
whose events interleave over four months are hard to read as one person, two
whose events sit a week apart are easy.

It earns its place immediately. Cluster 1824 — 14 identifiers, the largest in
the file — shows fourteen short, **non-overlapping** spans running from May to
August across BUN, MGL, MON, NIZ, NYA and RWA. That is a false merge, visible
at a glance, where the graph alone only showed a lot of plausible links.

Events are written by the pipeline to
`person_candidate_node_dates.csv`, long format (`node, id_type, id, event,
kind, date`) — **33,434 dated events across all 5,869 graph nodes**, minimum 2
per node, median 5, max 28.

Long rather than wide because the data is per *node*, not per link: the
alternative was nine more `_a`/`_b` column pairs on the edge table, and a
contact has up to 26 follow-up dates, which no fixed set of columns can hold.
Written by the pipeline rather than read from the exports by the app for the
same reason the rarity score is — that script already has both exports open and
knows how to parse their dates.

| what | why |
|---|---|
| Dates parsed from the **first 10 characters** | admission and discharge arrive as `2026-06-13 00:00:00.0`, and `as.Date()` on the full string returns NA for some formats — silently dropping all 2,663 admission dates |
| `reporting_date`, `date_of_notification` and `investigation_datetime` **all kept** | they look redundant; measured, they differ from one another in 52.5% and 53.5% of rows |
| Follow-ups emitted **individually** | the spread *is* the information — a contact under follow-up on a date another identifier was in hospital is two people |
| Source cases get **no linelist dates** | the linelist identifier is almost always its own node in the same cluster (that is what the `EXACT` link is), so copying them would draw the same dates on two lanes and invent an agreement |
| Source cases get **no contact-exposure dates** either | `Contact exposure reported` described when *other* people said they were exposed to this person — the contacts' timelines, not this identifier's. 335 dots across 233 lanes answering no question a reviewer was asking. The same dates remain on each contact's own lane as `Last contact with source`. Consequence: 175 of the 233 source-case lanes now hold a single event and so have no span line, which is honest — one date is one date — and no lane is left empty, since every source case has a first registration |

### Timeline label reference

Fourteen labels, all of them derived from a named column except the source-case
ones. Percentages are the share of that type's identifiers carrying the event.

**Linelist Entry** — straight from a column:

| plot label | full name | source column | means | coverage |
|---|---|---|---|---|
| `onset` | Symptom onset | `date_of_symptom_onset` | when the patient says symptoms began | 86.4% |
| `sample` | Sample collected | `date_of_sample_collection` | specimen taken | 46.7% |
| `lab` | Lab result | `lab_analysis_date` | specimen analysed | 40.6% |
| `notified` | Notified | `date_of_notification` | alert raised to the system | 99.9% |
| `investig.` | Investigated | `investigation_datetime` | field investigation performed | 86.7% |
| `reported` | Reported | `reporting_date` | row entered DHIS2 | 100% |
| `admitted` | Admitted | `pec_date_dadmission` | admitted to care | 8.8% |
| `discharged` | Discharged | `pec_date_de_sortie` | left care | 7.8% |

**Contact**:

| plot label | full name | source column | means | coverage |
|---|---|---|---|---|
| `registered` | Registered | `registration_date` | entered as a contact of a source case | 100% |
| `last contact` | Last contact with source | `last_contact_with_source_date` | last known exposure — the incubation window runs from here | 100% |
| `f/u` | Follow-up | `followup_date` | one per follow-up visit; median 7, up to 26 | 100% |

**Source Case** — **derived, not recorded**:

| plot label | full name | derived as | means | coverage |
|---|---|---|---|---|
| `1st as source` | First named as source | **min** of its contacts' `registration_date` | first time anyone was registered as their contact | 100% |
| `last as source` | Last named as source | **max** of the same | last time; omitted when it equals the first | 24.9% |

So a source-case lane is **not a clinical timeline** — it describes
contact-tracing activity around that person. A source case that is also a
linelist row therefore shows two lanes: one for what happened to them, one for
when others named them.

Reading the markers: dot size encodes the kind, largest first — onset or
registration (6) → exposure (5) → lab and care (4.5) → admin (3.5) →
follow-up (2.5). The same order decides which label survives when labels
compete for room, so what happened to the patient outranks when the paperwork
moved.

**An absent dot usually means *not recorded*, not *did not happen*** — `lab` is
on 40.6% of linelist identifiers and `admitted` on 8.8%. That matters when
comparing lanes: one showing a lab result and another not is weak evidence they
are different people.
| `first_seen_date` is **not shown at all** | It is when the pipeline first saw the row, not something that happened to the patient. It was the latest event on **87%** of linelist identifiers, so it sat at the right-hand end of nearly every lane, drew the eye, earned a label, and stretched the span line rightwards by a median of 2 days and up to 103 — 5,474 of 33,419 events (16%) for no decision value. First it was excluded from the span line only; then dropped entirely. Provenance belongs in the registry, not on a clinical timeline. Dropping it also raised labelling from 93% to **96%** of markers |

**Points are labelled, but not all of them can be.** Measured on the real
output: a lane holds a median of 5 events on 4 distinct dates, and **37% of the
gaps between consecutive dates on a lane are under 34 px** — roughly one short
label. Three things make labelling work anyway, and **93% of markers (19,839 of
21,299) end up labelled**:

1. **One marker per date, not per event.** Collapsing same-date events removes
   36% of markers, and is truthful: two dots at identical x were one dot drawn
   twice, with only the top one's tooltip reachable. A collapsed marker reads
   `sample +2`, and its tooltip lists all three.
2. **Sides alternate** above and below the lane in date order, which doubles
   the spacing between labels that can actually collide. Lane height went 24 →
   34 px so both rows stay inside the band; otherwise a label below one lane
   lands on the next.
3. **Placement runs in priority order, not left to right.** When a label has to
   be dropped it is the paperwork that goes, never the onset. Left-to-right
   greedy would keep whichever came first, which on these lanes is usually
   `notified`. Rank order is onset → exposure → lab → care → admin → follow-up
   → meta, the same order as the marker sizes: what happened to the patient
   outranks when the paperwork moved.

Label widths are measured on the text actually drawn, suffix included — sizing
from the base label while drawing `onset +2` would let precisely the most
informative labels overlap. Verified across all 2,658 clusters: 0 of 19,839
labels overflow the frame.

Dropping a label costs nothing but the label; the dot and its tooltip remain.

**Implausible dates are excluded, and listed.** One absurd date destroys the
axis for its whole cluster: the range is scaled to the events on it, so a
symptom onset recorded as `1997-05-04` spreads 29 years across the panel and
squeezes ten real events into a few pixels. **15 of 33,434 events** are
affected, ruining 15 of 2,658 clusters outright — every one a typo rather than
data:

| value | field | note |
|---|---|---|
| `261-02-26` | investigation | year 261; R writes it *without* the leading zero, so it isn't even valid ISO 8601 |
| `1997-05-04` | symptom onset | 29 years of axis |
| `2006-08-18`, `2020-06-24` … `2023-08-05` | notified / investigated | eleven more, all pre-outbreak |
| `2024-07-25` | symptom onset | |
| `2027-06-20` | notified | the only one in the *future* |

The window is **derived** from the export's own `reporting_date` range
(2026-05-03 to 2026-08-31), minus a year and plus a month — so it stays right
for the next outbreak instead of being pinned to 2026. `reporting_date` is the
anchor because it is 100% populated and records when the row entered the
system: an onset up to a year before the first report is clinically plausible,
one before that is not.

Dropped from the **timeline table only**; nothing is changed in the export, and
every excluded value is logged individually, because each is a mistyped date
sitting in the source data and naming it is how it gets fixed upstream.

Dates are written with `format(d, "%Y-%m-%d")` rather than `as.character(d)`:
`as.character()` on a Date before year 1000 drops the leading zero, which is
how `0261-02-26` became `261-02-26`.

`CONFIDENTIALITY`: this file holds identifiers and dates and **no names**, which
makes it less sensitive than `person_candidate_edges.csv` — not insensitive.
Dates of onset are patient data. It stays in `person_matching/`, and a test
asserts it stays name-free.

Two R traps worth recording, both caught before running:

- `for (t in ticks)` over a `Date` vector yields the underlying **numbers**, not
  Dates, so `as.Date(t)` inside the x-mapping would fail with *"'origin' must be
  supplied"*. The mapping works in numbers and accepts either.
- `<circle>` with a `<title>` child is standard SVG and renders fine in a
  browser, but ImageMagick silently drops it — worth knowing if these are ever
  rasterised for a report.

### The decision bar

Across the top, same layout and CSS as the Review/Harmonise reason bar so the
two read as one tool: tick the reasons, then Confirm. Confirm, Unsure,
Previous, Skip and Save moved into it from the right-hand card — **moved, not
copied**, because two Shiny inputs sharing an id is not an error: the second
silently shadows the first and the buttons that look wired simply do nothing.
`tests/test_person_review_tiles.py` asserts each id is defined exactly once.

Three reasons the old bar had no need for:

| reason | why it earns a tick of its own |
|---|---|
| `role_change` — "Contact later became a case" | the same human under two identifiers in two namespaces, and the single most valuable thing this step finds |
| `common_name` — "Name is common" | the tile now shows a rarity percentile, so a reviewer can *see* this and should be able to say it |
| `shared_name` — "Different people, shared name" | the explicit opposite of `role_change`, and the reason the big false merges exist (`JEAN DE DIEU` ×7, `TAMBOLI/ANNIE/CHANTAL` ×11) |

**Reasons are stored on every edge of the cluster** (`cluster_reasons`,
`;`-joined), the same way `cluster_reviewed` is. Denormalised deliberately: the
reviewer file is a flat edge table with no cluster-level row to hang them off,
and putting them on one arbitrary edge would lose them the moment edges are
re-sorted. They are written in `PR_REASONS` order rather than click order, so
two reviewers who tick the same boxes produce byte-identical strings and the
files can be diffed.

Never rename or renumber a reason **value**. The values are what sit in the
reviewer files on disk; a rename silently orphans them. Labels can change
freely.

**The bar depends on the cluster id only, never on `pr$edges`.**
`checkboxGroupInput` resets to `selected` whenever its `renderUI` invalidates,
so a dependency on `pr$edges` would mean that breaking a link — which writes to
`pr$edges` — wiped every reason ticked but not yet confirmed, at exactly the
moment the reviewer was justifying that break. Saved reasons are read through
`isolate()`; the live "Marked / n links broken" readout is a separate output
that may depend on `pr$edges` freely, because it holds no input to lose.

Confirming a **split** cluster with no reason ticked warns but is **not
blocked**. Forcing a tick trains people to click the first box to get past the
gate, and a box ticked to get past a gate is worse than a blank — it looks like
evidence.

### The review tiles

Added 2026-09-07. Each node tile reads:

```
Source Case · RDC-AAA-BBB-26-0001235-MVE
BASEKA LIONGO
rarer than 46% of names
Gender: —
Age: —
Health Zone: Rwampara
Case class: —
```

Rendered with `visNodes(font = list(multi = "html"))`, which supports only
`<b>`, `<i>` and `<code>` — hence bold headings and plain `Label: value` rows
rather than anything more structured. Rows keep their position even when empty
(em dash) so two tiles can be compared down the column.

Two rows are type-specific, shown only where the field can exist:

| row | on | why |
|---|---|---|
| `Alert type` | linelist only | `case` (28,971) or `death` (6,029). It earns a row *because* `Case class` is so often blank — 60% of the linelist identifiers in the graph have no `final_mve_case_classification`, while `type_of_alert` is recorded on 99% of rows, and `death` vs `case` is exactly the kind of thing that settles whether two records can be one person |
| `Final follow-up` | contacts only | result of the **last dated** follow-up. 33 contacts end on `contact_became_suspected_case` and 24 on `case_confirmed` — those 57 are contacts who became cases, the cross-namespace link this step exists to find, so the tile says it rather than leaving it to be inferred |

The final follow-up is deliberately **not** taken through `pc_collapse`'s
"most frequent value wins", which is right for a name spelling and wrong here:
a contact followed up 21 times as `not_a_case` and once, finally, as
`case_confirmed` would be reported as `not_a_case`, hiding the single most
decisive fact about them. It keys on `max(followup_date)`, not the
`is_last_followup_day` flag, which reads "yes" on only 390 of 20,632 rows
across 2,539 contacts and so cannot identify the last day for most of them.
Four contacts have two different results on their last date; both are shown,
joined by ` / `, because there is no principled way to rank them.

**The rarity score is a percentile, not the raw number.** The raw score is a
sum of token IDF, so it climbs with the *number* of name parts as much as with
their rarity — the median name scores 16.80 over the 37,905-name pooled corpus,
with a range of 3.57 to 56.90. "16.99" tells a reviewer nothing; "rarer than
46% of names" tells them what they need. The raw score is in the tooltip.

It is also **context, not evidence**: the number that drove a match is the
edge's `name_weight`, which scores only the tokens the two names *share*. A
node's own rarity answers the different question a reviewer is implicitly
asking — "is this a name I should expect to see twice by chance?"

Computed in the pipeline, not the app: scoring a name needs the tokeniser, and
that is already *copied* between the dedup and the rarity script (guarded by
`tests/test_name_rarity.py`). A third copy in the Shiny app would be a third
thing to keep in step.

**Case classification.** `final_mve_case_classification` is a linelist column.
230 of the 259 named source cases are exactly a linelist `alert_id`, so they
inherit it **for display only** — deliberately *not* extended to sex, age or
health area, because those are corroborator fields and inheriting them would
raise `pc_n_avail()` for `contact_to_source_case` links from 1 to 4, changing
which tier they reach. Contacts have no classification at all, so their tile
**omits the row** rather than showing a dash: an em dash means "should have one,
doesn't", and 62% of linelist rows genuinely have none, which is itself
information.

**Open — source-case tiles look empty.** Because `pop_cs` carries only a name
and a health zone, every source-case tile shows `Gender: —` and `Age: —`, even
for the 230 that resolve to a linelist row where both are known. That reads as
missing data when it is really un-inherited data. Fixing it for display alone
would also shift the advisory cluster flags (`flag_multiple_sexes`,
`flag_wide_age_span`), so it is left as a decision rather than done quietly.

### Why large clusters used to snap back together

Pulling a big cluster apart by hand was impossible, and the cause was not the
physics settings. Two independent things:

**1. The whole graph was rebuilt on every click.** `output$pr_graph` depended on
`pr_current()`, which depends on `pr$edges`. Breaking a link *writes* to
`pr$edges`, so each click tore down the network and re-ran stabilisation from
scratch, discarding every position the reviewer had dragged. On a 14-identifier
cluster the layout was recomputed faster than it could be arranged.

The render now depends on the **cluster identity** and an explicit re-layout
counter, and on nothing else. Decisions are read through `isolate()` — needed at
first render, to show an existing reviewer file's breaks — and later changes go
through `visNetworkProxy()`, which restyles one edge in place and leaves every
node position alone.

**2. A broken edge still had physics.** `dashes = TRUE` changes only how a link
looks; its spring stayed in the simulation, so the two halves remained welded
and "breaking" a link separated nothing. Broken edges now set
`physics = FALSE`, so a cluster visibly comes apart as it is cut — which is the
review model made visible: *the person is whatever component survives*.

Consequences of (2) worth recording. Physics stays **on**, so a cut cluster
springs apart on its own; `dragEnd` therefore pins each node where it was
dropped, or a tile dragged clear of the pile would drift straight back in.

That pinning must use **`physics: false`, not `fixed`**. The two sound
interchangeable and are not: vis.js's drag handler only writes a node's position
when `node.options.fixed.x === false`, so `fixed: {x: true, y: true}` excludes
the node from physics *and* makes it permanently undraggable — a tile could be
moved once and then never again. `physics: false` takes it out of the simulation
while leaving it draggable. Shipped the wrong way round once, so
`tests/test_person_review_tiles.py` now asserts it.
`centralGravity` is deliberately non-zero because a fully broken cluster has no
springs left at all, and with no central pull its pieces drift off the canvas.
`springLength` went 160 → 220 since the tiles are now seven lines tall. The
**Re-layout** button unpins everything and arranges again — the one place a full
rebuild is wanted rather than avoided.

Edge appearance lives in one function, `pr_vis_edges()`, called by both the
render and the click handler. Two copies would drift, and drift here would show
as a broken link that still looks intact.

### The health-zone map

Where the identifiers are. **847 of 2,658 clusters span more than one health
zone**, two of them six zones, so "could one person be in both of these
places?" is a live question for a third of the queue — and one no amount of
name similarity can settle. It replaced the old Decision card, whose
explanatory text moved up next to the checkboxes it explains.

Geometry is reduced **once** by `ensure_zone_geo()` in `pipelines/0-setup.R`,
which short-circuits unless the shapefile is newer than its outputs — so on a
normal run it costs one `file.mtime()` comparison, not a 66 MB read. It lives
in setup rather than in this pipeline because it reads reference geography and
no pipeline script consumes what it writes; putting it here made it a step that
neither depended on nor fed the run around it, and one that `run_full_pipeline_bg()`
never invoked:

| | |
|---|---|
| `GRID3_COD_health_zones_v8_0.shp` | 66 MB, **4,131,532 points**, 519 zones |
| `health_zone_centroids.csv` | 519 rows, 19 KB |
| `health_zone_outlines.csv` | ~27,000 points, ~1 MB |

`st_point_on_surface`, not `st_centroid`: a centroid can fall **outside** a
concave or horseshoe-shaped zone, which would put the dot in a neighbour.
`preserveTopology = TRUE`, or simplification can collapse a small zone to
nothing.

**The extent zooms to the cluster, with a scale bar.** My first design used a
fixed national extent, arguing that two zones 20 km apart and two 1,900 km
apart must not look alike on separately auto-scaled maps. Measuring the
clusters refuted it:

| within-cluster spread | |
|---|---|
| 1,812 of 2,658 clusters | **0 km** (single zone) |
| p75 | 17 km |
| p90 | 39 km |
| p99 | 135 km |
| worst in the file | **364 km** |
| clusters over 100 km | 47 |

The 1,933 km figure that motivated the national map was the spread of every
zone *in the linelist*, not within any cluster. On a 2,000 km-wide map every
real cluster is a single dot. So the map zooms — floored at a ~60 km window so
a single-zone cluster does not become a meaningless close-up — and honesty
about scale comes from the scale bar plus the stated kilometres. Every distance
is a haversine on the real coordinates; the projection is a plain
equirectangular locator and is never measured off.

The dashed red line joins the two most distant zones. Zones the cluster touches
are filled and outlined; their neighbours are grey, which is what makes
"adjacent zones" distinguishable from "five zones apart".

**One zone cannot be placed.** `Lubunga (Tshopo)` carries a parenthetical
province and matches no `zonesante`, so it has no alias and no geometry. The
panel names it and warns that the stated span may understate the spread, rather
than dropping it and making the cluster look tighter than it is. Worth an entry
in `data/aliases/health_zone_aliases.csv`.

`CONFIDENTIALITY`: both derived files are **public GRID3 reference geography**
with no patient data — the only artefacts in this pipeline that are shareable.

### The name rarity panel

Between "This cluster" and "Decision". It answers the question the graph
cannot: **why does this cluster exist?** A line held together by a token 554
people share looks identical to one held together by a token nobody else has.

Written by the pipeline to `person_candidate_node_tokens.csv` (`node, id_type,
token, idf, n_entries, rarity_band`) — **14,196 token instances across the
5,869 graph nodes, 4,290 distinct tokens**. Median 3 distinct tokens per
cluster, max 16, so no cluster needs truncating. The tokens come from the same
`pc_tokens()` the matching used and the IDF from the same `PC_IDF`, so the
panel reports the numbers that actually produced the edges rather than a second
opinion about them.

Bands are fractions of `log(N)`, the convention used for every threshold here:
**very rare** ≥ `PC_SINGLE_TOKEN_MULT × log(N)` = 8.01 (already the point at
which one token alone can propose a match), **uncommon** ≥ 5.27, else
**common**. Ceiling is 10.54.

**Shared tokens are listed first, then tokens unique to one identifier** — each
group rarest-first. A single list sorted by rarity alone actively misleads, and
the largest cluster in the file proves it:

| cluster 1824, 14 identifiers | shared by | in corpus |
|---|---|---|
| `jean` | 14/14 | 554 |
| `dieu` | 14/14 | 250 |
| `de` | 14/14 | 90 |
| `mbateko` | 4/14 | 191 |
| eight *very rare* tokens (1–10 in corpus) | **1/14 each** | |

Everything the fourteen share is among the commonest name parts in the
outbreak; every distinctive token belongs to exactly one identifier. Sorted by
rarity alone the panel would open "very rare, very rare, very rare" and read as
well-founded. Only shared tokens can be evidence for a merge, so they lead —
and a cluster with **no** shared token says so outright in red.

`CONFIDENTIALITY`: unlike the dates table, this file **does contain patient
names** — a name token is a name fragment, and a token with `n_entries = 1` is
one person's name. Handle it like `person_candidate_edges.csv`.

### Candidate run (no picker on Review / Harmonise)

Review Persons and Harmonise both load the **current** run — `latest.json`,
falling back to the newest folder — and show it as text. They used to have run
dropdowns, added so a reviewer could reopen the build a half-finished file
belonged to. Removed 2026-10-18: votes are saved per `link_id` in
`reviewer_history/` and refilled into whichever build is loaded, and the ledger
applies them by `link_id`, so an old build is never needed — and reviewing one
means judging clusters that have since changed. The two dropdowns also had
different defaults, so after a part-failed build the tabs could show different
clusters. Algorithm Tuning keeps its own picker for comparing builds.

Runs are ordered by a parsed date key, not by the stem: the stems carry
**DDMMYYYY**, so sorting the strings puts `01092026` before `02082026` — a
September run ahead of an August one. Same trap `conftest._latest_folder()`
already works around.

### Algorithm Tuning (rebuilt 2026-10-18)

The old tab audited review buckets (dropped X1/X2, auto-accepted A0-A2). Buckets
were replaced by the link score on 2026-09-23 and the clusters file stopped
carrying `review_bucket`, so both of its queues came up empty; its flags were
written into each run's `reviews/` folder and read by nothing. It now checks
the two link-score thresholds:

- **Thresholds** (`link_score.auto_accept`). From decisions already made: the
  ledger where a reviewer consensus or harmoniser settled a link, otherwise the
  majority of confirmed reviewer votes. Shows the share of links broken per
  0.1 score band, what each auto-accept setting would accept and how many of
  those clusters people split, and any identical-ID links someone broke.
- **Near misses** (`link_score.min_keep`). Each matcher writes pairs scoring
  in [`near_miss.min_score`, `min_keep`) to a separate file
  (`near_miss_candidates.csv`, `contact_near_miss_candidates.csv`,
  `source_case_near_miss_candidates.csv`) -- these never become edges. Step 2
  draws `near_miss.per_link_type` per link type into
  `person_candidate_near_misses.csv`, choosing by a hash of the `link_id` so
  the same pairs stay in the sample across nightly builds. Verdicts (same /
  different / unsure) are appended to `near_miss_history/<name>.csv` by
  `link_id`; the tab summarises the share judged "same" and, using each
  type's band size, roughly how many real matches the cutoff loses.

### Link classes, and the queue derived from them

Added 2026-09-07. There is **one** classification, and it is on the **link**.
Cluster review priority is *derived* from the classes a cluster contains rather
than being a second set of labels, because the same link earns different
treatment by context: a name-only `L3` link between two identifiers is a simple
pairwise judgement, and the same link inside a chain of five is a transitive
closure hazard that can drag a stranger into someone else's cluster.

| link class | n | what it is | what to check |
|---|---|---|---|
| `L0_identity` | 230 | same ID string both sides | nothing — not a judgement |
| `L1_within_strong` | 961 | one namespace, tier A | usually confirm a spelling variant |
| `L2_within_probable` | 1,395 | one namespace, tier B | whether the corroborators are coincidence |
| `L3_within_name_only` | 627 | one namespace, tier C | **is this just a common name?** |
| `L4_cross_namespace` | 133 | contact↔linelist or contact↔source case | could one person hold both roles? |
| `L5_single_token_corroborated` | 153 | one-token name, ≥2 fields agree | plausible; the name alone proves nothing |
| `L6_single_token_bare` | 5 | one-token name, ≤1 field agrees | **highest false-positive risk in the file** |

Tier `S` is assigned on **name rarity alone and never looks at corroborators**,
so it had been putting 153 links where a rare single name is backed by two to
five agreeing fields in the same tier as 5 links where nothing else agrees at
all. `L5`/`L6` split them.

**Queue order leads with the fast lane.** Clusters where *every link needing a
judgement* is tier A come first — **807 of the 2,488**, a third of the queue,
and 756 of them are simple pairs. Bucket order still applies *within* each
rank, so the 18 fast-lane clusters that also cross namespaces lead the fast lane
rather than being buried in it. That is why leading with the easy work costs
almost nothing: only 18 of the 807 are `P1`, so the highest-value clusters are
not pushed down behind a third of the queue.

`EXACT` links are excluded from that test rather than disqualifying a cluster.
They are auto-accepted at load and carry nothing to judge, so one `EXACT` plus
one tier-A link is exactly as easy to settle as one tier-A link. Strictly
"every tier is A" would be 791; ignoring `EXACT` gives 807. If the stricter
reading is ever wanted, the tier filter in `all_near_certain` is the one line to
change.

| review bucket | rule | clusters |
|---|---|---|
| `A0_auto_identity` | every link is `L0` — **accepted without review** | **170** |
| `A1_auto_near_certain` | every judged link tier A **and** every name above the 50th rarity percentile — **accepted without review** | **628** |
| `A2_auto_redundant_links` | the `EXACT` and tier-A links **alone** connect every identifier, and the rarity floor holds — **accepted without review** | **11** |
| `P1_cross_namespace` | contains an `L4` | 83 |
| `P2_bare_single_token` | contains an `L6` | 5 |
| `P3_large_cluster` | ≥6 identifiers | 12 |
| `P4_weak_link_in_chain` | an `L3` and ≥3 identifiers | 140 |
| `P5_routine` | everything else | 1,609 |

**809 auto-accepted, 1,849 to review** — a 26% cut from 2,488. The three auto
buckets are kept separate rather than merged, because their justifications
differ in kind: `A0` says there is nothing to judge, `A1` says a rule judged it
good enough, `A2` says the questionable link is not holding anything up. If a
rule is ever withdrawn, its bucket is how those clusters are found again.

### Measured 2026-09-23: what the tiers see that the link score does not

Run: `LINELIST_20092026 × CONTACT-TRACING_20092026`, the first build where every
link carried a `link_score` out of 1 and links below `link_score.min_keep`
(0.4) were rejected. 3,301 edges over 2,767 clusters, against 7,538 and 3,871
before the cutoff.

**Links, by tier and score**

| tier | 0.4–0.5 | 0.5–0.6 | 0.6–0.7 | 0.7–0.8 | 0.8–0.9 | 0.9–1.0 | 1.0 | total |
|---|---|---|---|---|---|---|---|---|
| `A_near_certain` | 433 | 932 | 624 | 403 | 199 | 97 | 0 | 2,688 |
| `B_probable` | 311 | 7 | 0 | 0 | 0 | 0 | 0 | 318 |
| `C_possible` | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
| `S_single_token_name` | 37 | 15 | 0 | 0 | 0 | 0 | 0 | 52 |
| `EXACT_same_id` | 0 | 0 | 0 | 0 | 0 | 0 | 242 | 242 |

**Clusters, by review bucket and the cluster's weakest link**

| bucket | 0.4–0.5 | 0.5–0.6 | 0.6–0.7 | 0.7–0.8 | 0.8–0.9 | 0.9–1.0 | 1.0 | total |
|---|---|---|---|---|---|---|---|---|
| `A0_auto_identity` | | | | | | | 193 | 193 |
| `A1_auto_near_certain` | 195 | 568 | 479 | 346 | 162 | 85 | | 1,835 |
| `A2_auto_redundant_links` | | | | | | | 8 | 8 |
| `P0_class_contradiction` | 18 | 33 | 20 | 10 | 4 | 3 | | 88 |
| `P1_cross_namespace` | 20 | 9 | | | | | | 29 |
| `P3_large_cluster` | 5 | | | | | | | 5 |
| `P5_routine` | 359 | 149 | 32 | | | | | 540 |
| `X1_dropped_contradiction` | 16 | 8 | | | | | | 24 |
| `X2_dropped_weak_evidence` | 31 | 14 | | | | | | 45 |

Four findings, in the order they matter.

**The 0.4 cutoff had already retired three tiers.** `C_possible` fell to a
single link and `B_probable` to 318, of which 311 sit in the lowest band.
81% of surviving edges are tier A and 7% are `EXACT_same_id`. As a way of
sorting what is left, the tiers had stopped discriminating before anything
was decided about them.

**Two review buckets emptied themselves.** `P2_bare_single_token` and
`P4_weak_link_in_chain` hold no clusters at all. The links that defined them
— `L6_single_token_bare`, `L3_within_name_only` — are exactly what the cutoff
removed, so the score was already doing those two buckets' job.

**Tier A is not one confidence class.** It spans 0.4 to 1.0 almost evenly.
433 tier-A links score in the same band as 311 tier-B links and the one
remaining C. Whatever distinction the tier draws, the score draws a finer one
inside it, and the auto-accept rule reads the tier.

**The two systems openly disagreed at the bottom.** `A1` accepted 195 clusters
scoring 0.4–0.5 with no human ever opening them, while `P5_routine` sent 359
clusters from that same band to review and `X2` dropped 31 of them outright.
Three different fates for the same weight of evidence, decided by a rule that
does not read it. Neither A1 condition does: "every judged link is tier A" is
satisfied by links scoring 0.41, and the floor tests name rarity or age
separately rather than the evidence as a whole.

What a score cutoff would accept, against the 2,036 the buckets accepted:

| cutoff | clusters |
|---|---|
| ≥ 0.9 | 288 |
| ≥ 0.8 | 455 |
| ≥ 0.7 | 811 |
| ≥ 0.6 | 1,342 |

`A0` and `A2` sit at exactly 1.0, so identity-only clusters accept under any
rule and the two systems agree completely there. The disagreement was entirely
about `A1`.

**Decided on this evidence:** auto-accept moves to `link_score >= 0.8` and the
tier-derived buckets are retired. The sections below describe the system that
replaced, and are kept because the rules they document were in force for every
build before this date — a cluster accepted in an earlier run was accepted by
`A1`, not by a score, and the ledger records it that way.

### The A1 auto-accept rule, and the evidence for it

Added 2026-09-08 after a 50-cluster review, drawn from the front of the queue —
deliberately the hardest cases available (74% carried a warning sign, against
27% across the population).

- **Zero links broken in all 50.** 95% upper bound on the fast lane's
  false-merge rate: **5.8%**.
- **28 of the 50 satisfied this rule. All 28 confirmed, fully intact, none
  unsure.** 95% upper bound on the rule's error rate: **10.1%**.
- **The rarity floor turned out to be doing real work**, which was a surprise:

  | | confident | unsure |
  |---|---|---|
  | min percentile **> 50** | 28 | **0** |
  | min percentile **≤ 50** | 18 | **4** |

  Fisher exact **p = 0.032**. The four unsure clusters sit at percentiles 37,
  40, 43 and 45 — just under the floor — and every one had `common_name` and
  `age_disc` ticked. Without the floor, tier A alone carried 4 doubts in 50;
  the floor removed 22 clusters and all 4 doubts were among them.

An earlier analysis in this repo concluded the rarity condition bought no
safety. That analysis tested rarity against the *demographic flags* (two
sexes, wide age span) and found nothing — but those flags are largely
data-quality noise, so their silence was not evidence. Tested against reviewer
judgement, the floor discriminates.

**What this does not establish.** Zero errors bounds the rate; it does not
explain the failure mode, because no fast-lane error was observed. The
boundary is also unpinned: doubts stop at 45 and confidence starts at 52, so
anywhere in 45–52 fits the data equally well. 50 is the round number in that
gap. And the floor is a percentile of *whole-name* rarity, a sum of token IDF
that rewards long names as much as unusual ones — a better-constructed measure
exists (how many other identifiers the matcher would rate a tier-A name match:
43 for the `JEAN DE DIEU` false merge, 0 for a genuine pair the percentile
scores lower) but it has no review evidence behind it, and this floor does.

**Two properties of the accepted set worth knowing.** 41 of the 628 are chains
of 3+ identifiers, so transitive closure is being accepted unreviewed — though
36 of the reviewed 50 were 3+ chains with zero breaks, which is evidence
against that being a problem at tier A. And **10 cross identifier types**,
which is the output this whole phase exists to produce; the rule as specified
accepts those without a human ever seeing them.

`A1` cannot fire when any identifier's name could not be scored. A missing
score is not evidence of clearing the floor, and defaulting an unknown to
"accept" is the one direction this must never fail in.

### The A2 redundancy rule

Added 2026-09-08, from a shape a reviewer spotted while reviewing. Accept without
review when the `EXACT` and tier-A links **alone** already connect every
identifier in the cluster, and the rarity floor holds. Any weaker link present
is then adding nothing: delete it and the cluster is unchanged.

The motivating shape:

```
    ct_case:X  --EXACT--  ll:X  --A--  ct_contact:Y
         \________________ B ______________/
```

The `B` link looks like a weak third leg. It is not. `ct_case:X` and `ll:X` are
**the same identifier string** — the `EXACT` link is an identity, not evidence —
so the tier-A evidence between `ll:X` and Y *is* the evidence between X and Y.
The `B` link is a shadow of the `A` link.

Her own argument was narrower: tier A is arithmetically unreachable for
`contact_to_source_case` (one comparable field, and A needs three of it), so `B`
is that link type's *ceiling* rather than a weak result. True, and it covers 3
clusters. The redundancy test covers 11 and does not depend on knowing which
link types have low ceilings — it asks only whether the cluster would stand
without the weak link.

`A1` is a strict **subset** of this test: if every judged link is tier A then
the strong subgraph is already the whole cluster. `A1` is evaluated first, so
the better-evidenced label wins and `A2` holds only the extra 11.

**What this rule does not have: review evidence.** None of the 11 were among
the 50 clusters reviewed, so unlike `A1` there is no measured error rate behind
it — the case is structural, not empirical. That is why `A2` **leads** the
accepted tail in the queue: audit effort should go where the evidence is
thinnest.

**8 of the 11 rest on a redundant `within_linelist` link, where the ceiling
argument would not apply.** Those links compare five fields, so tier A was
reachable and they did not reach it. They are genuinely weaker evidence, and
the rule accepts them purely because they are redundant — a slightly bolder
claim than the source-case case, and worth knowing. Every redundant link in all
11 is tier `B`; nothing weaker is being waved through. Two of the clusters carry
more than one redundant link (one has 2, one has 4) — the rule is per cluster,
not per link.

The node set is passed into `pc_strong_spans()` rather than derived from the
strong edges. A node reachable *only* by a weak link would simply be absent
from the strong subgraph, so deriving it would quietly ask "are the connected
nodes connected" — always true — and would auto-accept a cluster held together
by the very link the rule claims is redundant. That is the one way this test
could be wrong in the dangerous direction, and
`test_strong_spans_is_false_when_a_node_hangs_off_a_weak_link` pins both
directions.

`A2` carries the same rarity floor as `A1`, and cannot fire when any
identifier's name could not be scored.

### Accepted clusters stay viewable, at the back

Removing them from the queue made the rule **unauditable** — the only way to
see what it had accepted was to open the CSV. They are now the tail of the
queue instead:

| rank | | count |
|---|---|---|
| 0 | near-certain on tier, floor **not** cleared — reviewed first | 178 |
| 1 | everything else needing review | 1,671 |
| 2 | `A2_auto_redundant_links` — accepted, least evidence, audit first | 11 |
| 3 | `A1_auto_near_certain` — accepted, 28 reviewed clean | 628 |
| 4 | `A0_auto_identity` — accepted, nothing to judge | 170 |

Rank 0 is where **all four reviewer doubts fell**, so it leads. The accepted
tail runs **least-validated first**, which inverts `PC_BUCKET_ORDER`
deliberately: that order is about risk, this is about where audit effort should
go. `A2` rests on a structural argument and no review evidence at all, `A1` has
28 reviewed clusters behind it, and `A0` is arithmetic — X equals X — with
nothing to audit. Bucket order would bury the 11 clusters most worth checking
behind 798 that are not.

Three things express the state in the interface:

- **The counter changes what it counts.** Inside the review queue it reads
  `12 of 1849 to review`; past the end, `1 of 809 accepted`. A single running
  total over both would claim 2,658 clusters of work when 809 are settled, and
  progress would look permanently unfinished.
- **A green banner names the rule** that accepted the cluster, with its
  reason (*"near-certain throughout, rarest name at the 78th percentile"*).
  The controls stay live, because overriding is the entire point of being able
  to see these — but presenting a settled cluster as an open question would be
  a lie.
- **Overrides are recorded as overrides.** Breaking a link the rule accepted
  rewrites its `decision_reason` from `auto:near_certain` (or
  `auto:redundant_weak_link`, or `auto:exact_id`) to the same string prefixed
  `override:`, and the progress line counts them separately.
  A row marked `broken` while its reason still said `auto:` would contradict
  itself *and* hide the override from anyone counting how often the rule was
  overturned — which is the only evidence that would ever revise it.
  Restoring the link undoes the marker.

**`P1`+`P2`+`P3` is 100 clusters** — the front of what remains, holding nearly
all of the risk and nearly all of the new information.

There is deliberately **no "weakest link" column**. `PC_LINK_CLASSES` is an
enumeration, not a risk ordering, and reading its highest index as "weakest"
would assert that a cross-namespace `L4` link is weaker evidence than a
name-only `L3` one. They are different *kinds* of evidence, not different
amounts of it.

**Not used as an axis: demographic contradiction.** An earlier draft had
`_contradictory` cluster classes. Corroborators are counted over
sex/zone/area/age/onset, so "contradiction" is largely the complement of
`n_corroborators` and flagging both double-counts one piece of evidence — 100%
of `C_possible` edges are "contradictory", which is definitional rather than
informative. It also cuts against a decision already recorded in
`2-deduplicate_linelist.R`: *disagreement is never read as evidence that two
rows are different people*, justified there on 127 near-certain pairs where age
disagrees by more than 10 years in 12.7% of cases.

### The corroborator denominator

Found while measuring the classes, and fixed at the same time. `n_corroborators`
was compared against an **absolute** threshold, but the number of fields each
comparison can actually compare differs:

> **Updated 2026-09-24.** The figures below are **ceilings** — how many fields a
> comparison carries when nothing is blank. They were the denominator itself
> until this date, which was a second bug inside the fix; see *The denominator
> was still per link type* at the end of this section.

| link type | comparable fields (at most) | old A/B | new A/B |
|---|---|---|---|
| `within_linelist` | 5 — sex, zone, area, age, onset | 3 / 2 | 3 / 2 |
| `within_ct_contact` | 5 — sex, age, zone, registration, source case | 2 / 1 | 3 / 2 |
| `within_ct_source_case` | 1 — the *contact's* zone | 2 / 1 | **unreachable** / 1 |
| `contact_to_linelist` | 4 — sex, age, zone, area | 2 / 1 | 3 / 2 |
| `contact_to_source_case` | 1 — zone only | 2 / 1 | **unreachable** / 1 |
| `source_case_to_linelist` | 1 — zone only | — | **unreachable** / 1 |
| `exact_source_alert` | 0 — the ID strings are equal | — | — |

`within_ct_contact` gained a fifth field on 2026-09-23, when the source case
stopped being pass 1's blocking key and became a corroborator. `need_b` is
unchanged at 2 either way, since `ceiling(0.4 x 4)` and `ceiling(0.4 x 5)` are
both 2; what changed is that a pair can now earn a fifth agreement.

`source_case_to_linelist` was added the same day. Before it, the only path from
a source case to a linelist row was exact equality of `source_case_alert_id`
and `alert_id` — which held for 242 of 256, leaving the 14 that carry an
assigned `CI-`/`CX-` ID with no route at all. Those are exactly the cases DHIS2
gave no usable identifier for, so they can never equal an `alert_id`, and the
one pairing whose two sides are the same person *by definition* was the one
pairing with no name matching. It runs over all 256 rather than only the 14:
a source case that already has its exact edge simply produces a redundant one
that the pair de-duplication discards, and a source case matching a *different*
linelist row by name is worth surfacing, because it means the linelist holds a
second record of that case.

So `n_corroborators >= 2` meant "2 of 5" in one place and "2 of 1" — arithmetically
impossible — in another. Tier A was empty for every source-case pair not by
judgement but by accident, and nothing said so. This is the same failure as the
absolute IDF thresholds, which silently disabled the single-token tier in the
CT dedup.

The requirement is now a **fraction of the fields actually compared**, anchored
on the linelist dedup's calibrated 3-of-5 and 2-of-5 (`0.6` and `0.4`, which
reproduce them exactly). Where too few fields exist for tier A to be strictly
stronger than tier B, A is reported **unreachable** and those pairs cap at B —
the same outcome as before, but declared and logged rather than accidental.

Effect on the calibration run: **62 of 3,504 edges change tier**, all on the CT
and cross-file side (11 `A`→`B` and 50 `B`→`C` in `contact_to_linelist`, 1
`A`→`B` in `within_ct_contact`). The 3,099 within-linelist edges are untouched,
which is the point — that side was already calibrated. Every **bucket** count
above is unchanged by the fix, since the one within-file edge that moves goes
`L1`→`L2` and no bucket rule mentions either.

Each edge now carries `n_corroborators_available`, so `corroborators 2` can be
read as the fraction it is.

### The denominator was still per link type

Found 2026-09-24, while writing the methods description of the corroborator
score. The fix above made the *requirement* a fraction of the fields compared,
but left the denominator itself a constant for the comparison:

| script | how `n_avail` was computed | |
|---|---|---|
| `epi_linelist_pipeline/2-deduplicate_linelist.R` | per pair, term for term against its five agreement tests | correct |
| `contact_tracing_pipeline/3-deduplicate_contacts.R` | `CTD_PASS1_N_AVAIL <- 5L`, `CTD_PASS2_N_AVAIL <- 1L` | constant |
| `identify_persons/2-build-person-candidates.R` | `pc_n_avail(A, B)`, asking `any(!pc_blank(A[[f]]))` over each **population** | constant |

An agreement test is FALSE when either side is blank, so a field counted in the
denominator but missing for this pair lands in `n_avail - n_corr` **exactly as a
contradiction would**. `corrob_score()` reads `2*n_corr - n_avail`, so that is
not a rounding difference:

| a contact-to-linelist pair agreeing on sex and zone, age and area blank | net | score |
|---|---|---|
| as computed | `2×2 − 4` = 0 | **0.50** |
| against the fields it actually carries | `2×2 − 2` = +2 | **0.79** |

Worse, a pair agreeing on the **one** field it could be compared on, with three
blank, scored `2×1 − 4 = −2` → **0.21** — below the 0.50 a pair with nothing
comparable is supposed to get, and below a pair that compared four fields and
disagreed on two. It was punished for what was never recorded.

The config's claim that a blank is *"neither credit nor penalty"* was therefore
true only of a field absent from a **whole population** — a source case has no
sex, ever — and false of ordinary missing data, which is most of it. The
population-level test was written for the former and quietly did duty for the
latter. Nothing is lost by moving the question to the pair: a field NA across
all of `pop_cs` is necessarily blank in every pair drawn from it, so the
population case is the limit of the per-pair one.

All three matchers now compute it per pair, with **one availability term per
agreement term** — `ctd_cmp_chr`/`ctd_cmp_num` and `pc_cmp_chr`/`pc_cmp_num`,
sitting beside the agreement tests they pair with. Age tests the numeric
coercion rather than non-blankness, since an age of `unknown` is not blank and
cannot agree. Each matcher asserts `n_corr <= n_avail`, as the linelist dedup
always has.

The population-level test survives in the cross-file matcher as
`pc_max_avail()`, used **only** for the log line that reports what a comparison
can reach at best, so an unreachable tier A stays visible. Keeping it under a
name that says *ceiling* is what stops the two being confused again.

**What this costs.** Corroborator scores, link scores and tiers recorded before
2026-09-24 are not comparable with ones after it **on the CT and cross-file
sides**. The linelist side is untouched — it was always per pair, and its 3-of-5
/ 2-of-5 is the anchor `tier_a_frac` and `tier_b_frac` were derived from, so the
calibration itself is unaffected. Direction of travel is one way: every pair
with a blank field gains, none loses, so scores rise and more links clear
`link_score.min_keep`. A re-run is needed before the next review queue is built,
and `test_edges_carry_the_denominator_for_the_reviewer` will then be asserting
something new — that at least one pair of each link type carries every field.
If that fails, read it as a fact about coverage in the export before treating it
as a fault in the denominator.

**Still open, and deliberately not changed here.** `2-deduplicate_linelist.R`
and `3-deduplicate_contacts.R` both record that *disagreement is never read as
evidence that two rows are different people* — justified on 127 near-certain
pairs where age disagrees by more than 10 years in 12.7% of cases. But
`2*n_corr - n_avail` subtracts disagreements, so a pair agreeing on 2 of 5
scores 0.35, below the 0.5 it would get with nothing comparable. The tier system
honours the rule and the score does not. This fix only stops **blanks** being
counted as disagreements; whether genuine disagreement should count against a
pair at all is a separate decision, and the methods text cannot claim both.

---

## The name rarity table

**Output:** `data/processed/person_matching/<run>/name_rarity/name_rarity.csv`

A measure of how unusual a name token is across everyone known to the outbreak.
This is not new machinery — `2-deduplicate_linelist.R` already computes token
IDF internally and it is the reason that matcher works. What is new is that the
measure becomes **shared and inspectable** rather than private to one script, so
the linelist dedup and the cross-file matching cannot drift onto different
notions of "unusual".

### What counts as a token

Defined by `dedup_tokens()` in `2-deduplicate_linelist.R`, ported to
`tests/reference/strings.py:name_tokens()`. Any new script must use the same
definition or the scores are not comparable.

1. lowercase, trim
2. `translit_ascii()` — an explicit `chartr` map for accents, plus ß→ss, æ→ae,
   œ→oe, then drop anything still non-ASCII. Deliberately **not**
   `iconv(to = "ASCII//TRANSLIT")`, which is platform-dependent: glibc renders
   `Gisèle` as `Gisele` while macOS commonly gives `Gis?le`, and the `?` then
   acts as a separator — the same name became one token on Linux and two on
   macOS, changing every rarity weight
3. **apostrophes are deleted, not spaced** — see below
4. every other non-alphanumeric becomes a space; **digits are kept**
   (`[^a-z0-9 ]`)
5. split on whitespace
6. drop tokens shorter than `min_chars`: **2 for scoring and IDF**, 3 for
   blocking
7. keep only the first **6** tokens (`MAX_TOKENS_PER_NAME`)

**Why apostrophes are deleted rather than treated as separators.** 640 linelist
names (1.8%) contain one, and they are Alur and Lendu orthography from Ituri —
`DZ'KANA BOLO`, `UDJONG'A MBELE`, `KITWALE MBANG'A`, `ALING'THA NOZA`. The
apostrophe is part of the name, not a word break. Splitting on it would mint
`dz` as a token in **160** unrelated people, plus `da`, `ang`, `na` and other
2–3 character fragments. Those would then be treated as shared name evidence
between people who share nothing, dragging the IDF of genuinely distinctive
names down toward common ones. Deleting keeps `dzkana` as the single distinctive
token it is.

The rule is narrow and worth stating because it is easy to get wrong: an
off-the-shelf `[^a-z ]` → space substitution splits on apostrophes, and it also
silently discards digits. Measured against the correct tokeniser, that
alternative changes 1.82% of names — small in aggregate, but concentrated
entirely in one community's names, which is the worst place for a systematic
bias.

### Three rules, each measured rather than assumed

**1. Pool both files.** Measured on `LINELIST_01092026` ×
`CONTACT-TRACING_01092026`:

| corpus | entries | distinct tokens | tokens seen once |
|---|---|---|---|
| linelist only | 35,108 | 22,929 | 15,926 |
| CT only | 2,798 | 2,552 | 1,820 (71%) |
| **pooled** | **37,906** | **23,812** | 16,455 (69%) |

Neither file works alone. **CT alone saturates**: with 2,798 entries its IDF
ceiling is `log(2798) = 7.94`, so a genuinely rare name and a merely-unseen one
score identically — `zilongo` is 10.47 in the linelist and 7.94 in CT purely
because CT is small. **Linelist alone under-covers**: 881 of 2,552 CT tokens
(34%) never appear in the linelist, so they fall back to `IDF_UNSEEN` and look
maximally rare, inflating confidence on exactly the CT↔linelist links that need
the most care.

**2. Count once per identifier, never once per row.** CT is one row per
follow-up — median 7, up to 26. Counting rows would inflate a contact's name up
to 26-fold, making a rare name look common. That error suppresses true matches,
silently, and would be very hard to spot afterwards. Collapse to distinct
`alert_id` / `contact_id` / `source_case_alert_id` first.

**3. The linelist dedup does not need this table.** Pooling changes
linelist-internal IDF by a median of **0.077**, with 44 of 22,929 tokens moving
by more than 1.0 — the linelist is 93% of the pooled corpus. So
`2-deduplicate_linelist.R` may keep computing its own and stay runnable on
linelist-only dates. If it is ever switched to read this table, that is a
convenience, not a correctness fix.

### What it is for

The tail is where every problem so far has been, and the table makes it
measurable instead of a judgement call per case:

| | |
|---|---|
| entries sharing a normalised name with another | 4,584 (12.1%) |
| worst collision | `jean kambale`, 19 distinct entries |
| one-token names | 1,401 (3.7%) |
| …rare enough to trust alone (IDF ≥ 8) | 794 |
| …**must never** be matched on name alone | **607** |

*(Figures recomputed 2026-09-05 using `name_tokens()` rather than an ad-hoc
tokeniser. The corrections are ±1 to ±7 — the aggregate is insensitive to the
apostrophe rule even though 640 individual names are not.)*

That last row is the `okwera` / `sifa` / `taliko` refusals with a threshold
attached rather than a per-case decision.

### A running file for the pipeline, a snapshot for the audit

Two requirements pull in opposite directions.

Every matching script needs a path it can rely on. Token rarity is an *input* to
both deduplications now, not a cross-file artefact produced after them, and a
consumer cannot be asked to work out which pair of export dates was last built.
Under the old dated-folder-only layout there was no such path — which is exactly
why each script grew its own copy of this computation, and why one pipeline
carried three incompatible answers to "how rare is this name".

But IDF moves as the corpus grows, so a score behind a reviewed decision would
not reproduce next month if the table were simply overwritten. A reviewer's
judgement is only auditable against the numbers they actually saw.

So each run writes its own folder, and `data/processed/latest.json` says which
one is current:

```
data/processed/person_matching/
  LINELIST_01092026__CONTACT-TRACING_01092026/  <- one folder per run, kept for audit
    name_rarity/
      name_rarity.csv
      name_collocations.csv
      name_rarity_meta.json
    candidates/                                 <- step 2's output for the same run
                                                   (incl. person_candidate_near_misses.csv)
    matching_params.json
  reviewer_history/<name>.csv                   <- every vote, all runs (one file per reviewer)
  harmoniser_history/<name>.csv                 <- every harmonise verdict, all runs
  near_miss_history/<name>.csv                  <- Algorithm Tuning near-miss verdicts, all runs
  link_ledger.csv, person_registry.csv
data/processed/latest.json                      <- name_rarity.stem = the current one
```

Every matching script reads the snapshot `latest.json` names
(`load_name_rarity()` → `current_name_rarity_dir()` in `0-setup.R`). The
snapshot is written first and `latest.json` updated last, so a run that dies
partway leaves the pipeline on the previous complete table. There used to be a
separate `current/` folder holding a byte copy of the newest snapshot; with
`latest.json` as the pointer it was redundant, and one copy cannot drift from
another. `tests/test_name_rarity.py::test_latest_json_names_a_complete_snapshot`
checks the pointer and the snapshot agree. The log is in
`logs/<run>/name_rarity_log.txt`.

The stem names both source runs, so a score can always be traced to the corpus
it came from. A linelist-only build is stemmed `<epi_run>__no-ct` and
the `name_rarity` section of `data/processed/latest.json` carries `"ct_run": null`, because that build is materially
different — 34% of CT tokens are absent from the linelist — and a consumer must
be able to tell without inferring it.

The running file also settles the objection that used to keep
`2-deduplicate_linelist.R` computing its own IDF: 11 of 51 linelist export dates
have no CT counterpart, so it could not depend on a table that might not exist
for its date. A running file always exists — on a linelist-only date it carries
the last pooled build, which is a better corpus than that date's linelist alone,
not a worse one.

`matching_params.json` is recorded beside the table because scores depend on
`pipeline_configs.json`'s `matching` block, which is editable and unversioned:
the same corpus under different collocation thresholds gives different numbers,
so the table alone would not be enough to reproduce them.

**Output columns:** `token`, `n_entries` (document frequency), `idf`, and
`n_linelist` / `n_ct_contact` / `n_ct_case` so a reader can see whether a token
is CT-only, which is what makes the pooling necessary.

`idf_unseen` is recorded in the metadata rather than left to the caller. A token
the corpus has never seen must not default to 0, which would read as maximally
*common* — the dangerous direction, and an easy mistake to make.

### Open: the bootstrap

The ideal corpus is one entry per **person**, which is this pipeline's own
output, while rarity is an input to producing it. Bootstrap with one entry per
identifier and recompute afterwards. The distortion is small — the ~230 known
duplicate identifiers are 0.6% of the corpus — so one iteration should be
enough, but the second pass is worth actually running rather than assuming.

---

## The link training table

**Script:** `5-build-link-training.R`, added 2026-10-31. It runs after the
ledger and registry on every path that rebuilds them, the nightly ledger-only
run included, because new votes arrive every day. It is never fatal: a failure
is logged and the run carries on.

**Output:** `data/processed/person_matching/training/link_training_<date>.csv`
(one per day, plus `link_training_latest.csv` and a `_meta.json` beside each).
Mirrored to GitHub with the other allow-listed files (the `link_training_*` files only; the rest of `training/` stays on S3). `link_id` is a pair of identifiers, so the repo must stay access-restricted.

One row per link a human has judged, for the link classifier built in the
separate **BDBV2026-Link_Classification** repo. Same-ID links are left out
(they are taken as given).

**The label, `label_surety`**, is how sure the humans were that the link is
real, out of 1. Each reviewer's latest vote is worth 1 (kept, cluster
confirmed), 0.75 (kept, unsure), 0.25 (broken, unsure) or 0 (broken,
confirmed). The latest harmoniser verdict counts as two votes: 1 or 0 when
settled, 0.5 when unsure; escalate is left out. The score is the weighted mean.
Disagreement and doubt both pull it towards 0.5. Values are in
`matching.training`.

**Columns:** `label_*` is what humans said (the target, never a predictor);
`x_*` are the predictors; `group_id` keeps links that share an identifier
together when the data is split for training and testing; `review_source` is
`review` until a random audit sample exists, when audited links will be marked
`audit`.

**Predictors** (all name-free; per-side values given as `_min` / `_max`):

| group | columns |
|---|---|
| name | `x_name_score`, `x_name_distance`, `x_name_rarity_pct_*`, `x_name_shared_outside_*` |
| other evidence | `x_link_score`, `x_corrob_score`, `x_n_corroborators`, `x_n_corroborators_available` |
| person | `x_same_sex`, `x_age_gap`, `x_age_digit_slip`, `x_same_vaccine_status`, `x_health_worker_match` |
| place | `x_same_zone`, `x_zone_km`, `x_same_area`, `x_same_village`, `x_etc_in_zone_*` |
| contact | `x_same_phone` |
| time | `x_date_gap_days`, `x_n_death_alerts`, `x_days_alive_after_death` |
| plausibility | `x_contact_of_self`, `x_class_clash`, `x_class_pair` |
| kind of link | `x_link_type` |

Pair-level evidence (scores, sex, age, zone, area, classification) comes from
the candidate run that last proposed the link (`features_run`); everything else
is looked up in today's processed exports. Some predictors only exist for some
link types: phone, village and health worker are linelist-to-linelist only,
because the contact-tracing cleaning does not keep phone numbers or villages.
Head of household was considered and dropped for the same reason.

