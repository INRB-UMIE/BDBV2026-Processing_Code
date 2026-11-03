#!/usr/bin/env Rscript
# run_pipeline.R -- runs one pipeline job in a process of its own.
#
# Every pipeline run goes through here: the app's run buttons start it (see
# pipeline_launch() in shiny/global.R) and so does the nightly timer on the
# instance. Because the run is not a child of a browser session, a dropped
# connection no longer stops it; the app shows its progress from the log file.
#
#   Rscript --no-init-file pipelines/run_pipeline.R <job> [options]
#
#   <job>       full | epi | ct | ct_combine | identify | ledger | nightly
#               (nightly = full if any raw export is not yet processed,
#               otherwise ledger: link ledger + person registry only)
#   --repo DIR  repo root (default: the working directory)
#   --trigger   app | nightly | manual -- recorded, shown in the app
#   --by NAME   who started it from the app
#   --epi S,S   linelist export stems to (re)process     (full, epi)
#   --ct S,S    contact tracing export stems to (re)process (full, ct)
#
# What it writes:
#   logs/pipeline_runs/<run_id>.log     everything the run prints
#   logs/pipeline_runs/last_<job>.dcf   how the job's latest run went
#   logs/pipeline_runs/last_nightly.dcf the same, for the nightly run
#   .pipeline_run.lock/                 held while running; a second run is
#                                       refused (the nightly one skips instead)
# The steps' own logs still go to logs/<date-time>/ as before.
#
# Exit status: 0 succeeded (or nightly skipped), 1 failed, 3 another run going.

# Everything runs inside local(): each pipeline step ends by clearing the
# global environment (rm(list = setdiff(ls(), keep_objects))), which would
# delete this script's own variables mid-run. A local() frame is untouched.
local({
  args <- commandArgs(trailingOnly = TRUE)
  opt <- function(name, default = "") {
    i <- match(paste0("--", name), args)
    if (is.na(i) || i == length(args)) default else args[[i + 1L]]
  }
  split_stems <- function(s) if (nzchar(s)) strsplit(s, ",", fixed = TRUE)[[1]] else character(0)

  job       <- if (length(args) && !startsWith(args[[1]], "--")) args[[1]] else ""
  repo_root <- normalizePath(opt("repo", getwd()), mustWork = FALSE)
  trigger   <- opt("trigger", "manual")
  by        <- opt("by")
  epi_stems <- split_stems(opt("epi"))
  ct_stems  <- split_stems(opt("ct"))

  if (!file.exists(file.path(repo_root, "pipelines", "0-setup.R")))
    stop("not a repo root: ", repo_root, " (use --repo or start from the repo root)",
         call. = FALSE)
  setwd(repo_root)

  app_fns <- new.env()
  sys.source(file.path(repo_root, "shiny", "global.R"), envir = app_fns)
  jobs  <- app_fns$PIPELINE_JOBS
  stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  # ---- nightly: decide what tonight's run is --------------------------------
  nightly <- identical(job, "nightly")
  note <- ""
  if (nightly) {
    unprocessed <- function(statuses) {
      if (!length(statuses)) return(character(0))
      done <- vapply(statuses, function(s) isTRUE(s$processed), logical(1))
      vapply(statuses[!done], function(s) s$fname, character(1))
    }
    new_ll <- unprocessed(app_fns$get_ll_file_statuses(
      file.path(repo_root, "data", "raw", "dhis2_linelist_raw"),
      file.path(repo_root, "data", "processed", "dhis2_linelist_processed")))
    new_ct <- unprocessed(app_fns$get_ct_file_statuses(
      file.path(repo_root, "data", "raw", "contact_tracing_raw"),
      file.path(repo_root, "data", "processed", "contact_tracing_processed")))
    if (length(new_ll) || length(new_ct)) {
      job  <- "full"
      note <- paste0("new export(s): ", paste(c(new_ll, new_ct), collapse = ", "))
    } else {
      job  <- "ledger"
      note <- "no new exports"
    }
  }
  if (!job %in% names(jobs))
    stop("unknown job '", job, "'; one of: ",
         paste(c(names(jobs), "nightly"), collapse = ", "), call. = FALSE)

  # ---- record + lock ---------------------------------------------------------
  started  <- Sys.time()
  run_id   <- paste0(format(started, "%Y-%m-%d_%H%M%S"), "_", job)
  runs_dir <- app_fns$pipeline_runs_dir(repo_root)
  dir.create(runs_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(runs_dir, paste0(run_id, ".log"))

  rec <- list(run_id = run_id, job = job, label = jobs[[job]]$label,
              trigger = trigger, by = by, note = note,
              started = format(started, "%Y-%m-%d %H:%M:%S %Z"),
              started_epoch = as.numeric(started),
              finished = "", finished_epoch = "", status = "running",
              last_step = "", error = "", log = basename(log_path),
              pid = Sys.getpid())
  record_to <- c(app_fns$pipeline_last_path(repo_root, job),
                 if (nightly) app_fns$pipeline_last_path(repo_root, "nightly"))

  if (!app_fns$pipeline_lock_acquire(repo_root, rec)) {
    cur <- app_fns$pipeline_current_run(repo_root)
    msg <- paste0("not started: another run is going (", cur$label,
                  ", started ", cur$started, ", ", cur$trigger, ")")
    message(stamp(), "  ", msg)
    if (nightly) {
      rec$status <- "skipped"; rec$error <- msg; rec$log <- ""
      rec$finished <- rec$started; rec$finished_epoch <- rec$started_epoch
      app_fns$pipeline_write_dcf(rec, app_fns$pipeline_last_path(repo_root, "nightly"))
      quit(save = "no", status = 0L)
    }
    quit(save = "no", status = 3L)
  }
  for (p in record_to) app_fns$pipeline_write_dcf(rec, p)

  # ---- run -------------------------------------------------------------------
  con <- file(log_path, open = "wt")
  sink(con); sink(con, type = "message")
  message(stamp(), "  ", rec$label, " started (", trigger,
          if (nzchar(by)) paste0(", ", by), ")")
  if (nzchar(note)) message("  ", note)

  job_args <- switch(job,
    full = list(epi_stems = epi_stems, ct_stems = ct_stems),
    epi  = list(force_stems = epi_stems),
    ct   = list(force_stems = ct_stems),
    list())

  last_step <- ""
  ok <- tryCatch({
    withCallingHandlers(
      do.call(app_fns[[jobs[[job]]$fn]], c(list(repo_root = repo_root), job_args)),
      message = function(m) {
        t <- trimws(conditionMessage(m))
        if (grepl("^\\[[0-9]+[a-z]?/[0-9]+\\]", t)) last_step <<- t
      })
    TRUE
  }, error = function(e) {
    rec$error <<- conditionMessage(e)
    message("ERROR: ", conditionMessage(e))
    FALSE
  })

  finished <- Sys.time()
  mins <- round(as.numeric(difftime(finished, started, units = "mins")), 1)
  message(stamp(), "  ", if (ok) "finished" else "FAILED", " after ", mins, " min")
  sink(type = "message"); sink(); close(con)

  setwd(repo_root)   # the steps may have moved it
  rec$finished <- format(finished, "%Y-%m-%d %H:%M:%S %Z")
  rec$finished_epoch <- as.numeric(finished)
  rec$status    <- if (ok) "succeeded" else "failed"
  rec$last_step <- last_step
  for (p in record_to) app_fns$pipeline_write_dcf(rec, p)
  app_fns$pipeline_lock_release(repo_root)

  # The end of the log on standard output: for the nightly run that is the
  # instance's journal (journalctl -u bdbv-nightly).
  cat("log: logs/pipeline_runs/", basename(log_path), "\n", sep = "")
  cat(utils::tail(readLines(log_path, warn = FALSE), 40), sep = "\n")
  quit(save = "no", status = if (ok) 0L else 1L)
})
