#!/usr/bin/env Rscript
# Unit tests for the Ituri consensus + consensus_ct derivation.
#
# These exercise the PURE functions (no heavy deps, no I/O). We source the
# pipeline file with the package loads masked and the main run skipped, so the
# tests run anywhere without ../BDBV2026-Data, pammtools, sf, or googledrive.
#
# Run:  Rscript src/lab_linelist_pipeline/test_consensus_ct.R

# ── Mask heavy setup before sourcing ────────────────────────────────────────────
# suppressPackageStartupMessages is lazy, so masking it prevents the library()
# block from ever evaluating; requireNamespace->TRUE skips the install loop.
suppressPackageStartupMessages <- function(...) invisible(NULL)
requireNamespace <- function(...) TRUE
install.packages  <- function(...) invisible(NULL)
options(linelist.skip_main = TRUE)

.here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(.here, "1-colname-standard.R"))

# ── Tiny test harness ───────────────────────────────────────────────────────────
.pass <- 0L; .fail <- 0L
ok <- function(desc, cond, got = NULL) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat(sprintf("  [PASS] %s\n", desc)) }
  else { .fail <<- .fail + 1L
    cat(sprintf("  [FAIL] %s%s\n", desc,
                if (!is.null(got)) sprintf("  (got: %s)", paste(got, collapse=",")) else "")) }
}

cat("== compute_ituri_consensus: returns category + source ==\n")

# Columns: r = bench Résultat, a = Kinshasa Altona, b = Bunia Altona
# 1) Kinshasa Altona present -> wins, source kinshasa_altona (even over bench/bunia)
r1 <- compute_ituri_consensus("Negatif", "Positif", "Negatif")
ok("returns a list/df with category + source",
   all(c("category", "source") %in% names(r1)))
ok("kinshasa altona wins category", r1$category[1] == "Positive", r1$category[1])
ok("kinshasa altona source", r1$source[1] == "kinshasa_altona", r1$source[1])

# 2) bench only -> bench_radione
r2 <- compute_ituri_consensus("Positif", "", "")
ok("bench-only category", r2$category[1] == "Positive", r2$category[1])
ok("bench-only source", r2$source[1] == "bench_radione", r2$source[1])

# 3) Bunia only -> bunia_altona
r3 <- compute_ituri_consensus("", "", "Positif")
ok("bunia-only category", r3$category[1] == "Positive", r3$category[1])
ok("bunia-only source", r3$source[1] == "bunia_altona", r3$source[1])

# 4) bench + Bunia agree -> bench_radione
r4 <- compute_ituri_consensus("Positif", "", "Positif")
ok("bench+bunia agree category", r4$category[1] == "Positive", r4$category[1])
ok("bench+bunia agree source = bench", r4$source[1] == "bench_radione", r4$source[1])

# 5) bench + Bunia disagree -> Undetermined, source NA
r5 <- compute_ituri_consensus("Positif", "", "Negatif")
ok("disagree category Undetermined", r5$category[1] == "Undetermined", r5$category[1])
ok("disagree source NA", is.na(r5$source[1]), r5$source[1])

# 6) neither -> Not yet run, source NA
r6 <- compute_ituri_consensus("", "", "")
ok("none category Not yet run", r6$category[1] == "Not yet run", r6$category[1])
ok("none source NA", is.na(r6$source[1]), r6$source[1])

# 7) Kinshasa present but unclassifiable -> source still kinshasa_altona
r7 <- compute_ituri_consensus("", "blah", "")
ok("kinshasa unclassified category", r7$category[1] == "Unclassified", r7$category[1])
ok("kinshasa unclassified source still kinshasa", r7$source[1] == "kinshasa_altona", r7$source[1])

cat("\n== build_consensus_ct: maps source -> assay Ct, positives only, strict NA ==\n")

# build_consensus_ct(source, category, radione_ct, altona_ct_bunia, altona_ct_kinshasa)
src    <- c("kinshasa_altona", "bench_radione", "bunia_altona", NA,            "bench_radione")
cat0   <- c("Positive",        "Positive",      "Positive",     "Undetermined","Positive")
rad    <- c("99",              "26.5",          "99",           "10",          "")  # bench picks rad
a_bun  <- c("99",              "99",            "20.1",         "11",          "99")
a_kin  <- c("31.2",            "99",            "99",           "12",          "99")
cct <- build_consensus_ct(src, cat0, rad, a_bun, a_kin)

ok("kinshasa -> altona_ct_kinshasa", isTRUE(all.equal(cct$consensus_ct[1], 31.2)), cct$consensus_ct[1])
ok("bench -> radione_ct_gp",         isTRUE(all.equal(cct$consensus_ct[2], 26.5)), cct$consensus_ct[2])
ok("bunia -> altona_ct_bunia",       isTRUE(all.equal(cct$consensus_ct[3], 20.1)), cct$consensus_ct[3])
ok("source NA -> ct NA",             is.na(cct$consensus_ct[4]), cct$consensus_ct[4])
ok("strict NA when driving Ct blank", is.na(cct$consensus_ct[5]), cct$consensus_ct[5])

# source is the provenance of the Ct: NA wherever consensus_ct is NA
ok("source NA wherever ct NA",
   identical(cct$consensus_ct_source,
             c("kinshasa_altona", "bench_radione", "bunia_altona", NA, NA)),
   cct$consensus_ct_source)
ok("non-numeric Ct coerces to NA",
   { z <- build_consensus_ct("bench_radione", "Positive", "Neg", "", ""); is.na(z$consensus_ct[1]) })

# positivity gate: a Negative with a real Ct in its driving column -> NA + source NA
g <- build_consensus_ct("bench_radione", "Negative", "29.0", "", "")
ok("negative gated: ct NA",     is.na(g$consensus_ct[1]),        g$consensus_ct[1])
ok("negative gated: source NA", is.na(g$consensus_ct_source[1]), g$consensus_ct_source[1])
# same row called Positive -> kept
p <- build_consensus_ct("bench_radione", "Positive", "29.0", "", "")
ok("positive keeps ct",     isTRUE(all.equal(p$consensus_ct[1], 29.0)), p$consensus_ct[1])
ok("positive keeps source", p$consensus_ct_source[1] == "bench_radione", p$consensus_ct_source[1])

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
