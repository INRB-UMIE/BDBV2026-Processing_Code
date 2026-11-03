#!/usr/bin/env Rscript
# Unit tests for health_zone province-prefix stripping + symmetric fallback.
#
# Exercises the PURE functions (no I/O, no shapefile). We mask the heavy package
# setup and skip the main run so this runs anywhere without ../BDBV2026-Data.
#
# Run:  Rscript pipelines/epi_linelist_pipeline/test_health_zone_match.R

# ── Mask heavy setup before sourcing ────────────────────────────────────────────
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

# A minimal lookup keyed by normalize_key(canonical) -> canonical name.
# Only Ituri/NK outbreak zones are present (mirrors aliases coverage); Kinshasa
# (Gombe) and Kasai (Luebo) are deliberately absent.
lookup <- c(
  bunia = "Bunia",
  katwa = "Katwa"
)

cat("== strip_province_prefix ==\n")
ok("strips kn prefix", strip_province_prefix("kn Gombe") == "Gombe", strip_province_prefix("kn Gombe"))
ok("strips ks prefix", strip_province_prefix("ks Luebo") == "Luebo", strip_province_prefix("ks Luebo"))
ok("strips it prefix", strip_province_prefix("it Bunia") == "Bunia", strip_province_prefix("it Bunia"))
ok("leaves clean Title-case name untouched", strip_province_prefix("Bunia") == "Bunia", strip_province_prefix("Bunia"))
ok("leaves two-word zone untouched", strip_province_prefix("Kasa Vubu") == "Kasa Vubu", strip_province_prefix("Kasa Vubu"))

cat("\n== match_health_zone: returns value + unmatched ==\n")
res <- match_health_zone(c("Bunia", "kn Gombe", "Katwa", "", "ks Luebo"), lookup)
ok("returns list with value + unmatched", all(c("value", "unmatched") %in% names(res)))

cat("\n== matched zones (in lookup) ==\n")
ok("known zone -> canonical value", res$value[1] == "Bunia", res$value[1])
ok("known zone -> not flagged", res$unmatched[1] == FALSE, res$unmatched[1])
ok("nk Katwa style already-clean known zone -> canonical", res$value[3] == "Katwa", res$value[3])
ok("Katwa not flagged", res$unmatched[3] == FALSE, res$unmatched[3])

cat("\n== prefix-stripped unknown zones: symmetric fallback ==\n")
# kn Gombe is NOT in lookup -> keep stripped raw value, flag unmatched (NOT blanked)
ok("kn Gombe -> kept as 'Gombe' (not blank/NA)", res$value[2] == "Gombe", res$value[2])
ok("kn Gombe -> flagged unmatched", res$unmatched[2] == TRUE, res$unmatched[2])
ok("ks Luebo -> kept as 'Luebo'", res$value[5] == "Luebo", res$value[5])
ok("ks Luebo -> flagged unmatched", res$unmatched[5] == TRUE, res$unmatched[5])

cat("\n== empty input ==\n")
ok("empty stays empty value", res$value[4] == "", res$value[4])
ok("empty is NOT flagged unmatched", res$unmatched[4] == FALSE, res$unmatched[4])

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
