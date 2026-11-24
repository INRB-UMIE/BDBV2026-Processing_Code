# ensure_name_rarity(): keep the current name rarity table, or build one.
#
# Sourced by each job that needs the table (full pipeline, Identify Persons, and
# the weekly "rarity" job). It is a file, not a function in shiny/global.R,
# because callr runs each job in a fresh R session that cannot see global.R.
# Source it AFTER 0-setup.R (it uses current_name_rarity_dir()).
#
NAME_RARITY_MAX_AGE_DAYS <- 8L

# The current table, per latest.json: does it exist, which run built it, when,
# and how many days ago. Needs 0-setup.R to have run (current_name_rarity_dir).
name_rarity_status <- function() {
  out <- list(exists = FALSE, stem = NA_character_, built_at = NA_character_,
              age_days = NA_real_)
  d <- tryCatch(current_name_rarity_dir(), error = function(e) NA_character_)
  if (is.na(d) || !file.exists(file.path(d, "name_rarity.csv"))) return(out)
  out$exists <- TRUE
  meta <- tryCatch(jsonlite::fromJSON(file.path(d, "name_rarity_meta.json")),
                   error = function(e) list())
  out$stem <- if (is.null(meta$stem)) basename(dirname(d)) else meta$stem
  if (!is.null(meta$built_at)) {
    out$built_at <- meta$built_at
    # Written by 1-build_name_rarity.R in the server's own time, read the same way.
    t <- as.POSIXct(meta$built_at, format = "%Y-%m-%dT%H:%M:%S")
    if (!is.na(t)) out$age_days <- as.numeric(difftime(Sys.time(), t, units = "days"))
  }
  out
}

# Build the table if asked to (force), if there is none, or if it is too old;
# otherwise keep the current one and say so. TRUE if it was (re)built.
ensure_name_rarity <- function(force = FALSE) {
  st <- name_rarity_status()
  why <- if (isTRUE(force)) "a rebuild was asked for"
         else if (!st$exists) "there is no table yet"
         else if (is.na(st$age_days)) "its build time is unknown"
         else if (st$age_days > NAME_RARITY_MAX_AGE_DAYS)
           sprintf("the table is %.1f days old (limit %d)", st$age_days, NAME_RARITY_MAX_AGE_DAYS)
         else ""
  if (nzchar(why)) {
    message("  Building the name rarity table: ", why, ".")
    source("pipelines/identify_persons/1-build_name_rarity.R", local = FALSE)
    return(invisible(TRUE))
  }
  message("  Keeping the name rarity table built ", st$built_at, " (",
          sprintf("%.1f", st$age_days), " days old, from ", st$stem, ").")
  message("  It is rebuilt weekly by the 'Name rarity table' job (Identify Persons ",
          "tab, or the weekly timer on the instance), and here only if it is ",
          "missing or over ", NAME_RARITY_MAX_AGE_DAYS, " days old.")
  invisible(FALSE)
}
