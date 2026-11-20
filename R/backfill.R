# Idempotent day-by-day Statcast backfill and per-season compaction.
# A day file exists only if it was fetched, checked against the schedule, and fully
# written, so a rerun resumes exactly where an interrupted run stopped.
#
# Day statuses:
#   written          every finished game in the schedule is present
#   written_partial  an old day (before the strict window) is missing a game even after
#                    a retry; written anyway, because Savant will not fill it later
#   skipped          the day file already exists
#   empty_error / fetch_error / incomplete_error
#                    nothing written; the run's caller exits non-zero (backfill_failed)

statcast_day_key <- function(level, date) {
  date <- as.Date(date)
  sprintf("raw/statcast/%s/%s/%s.parquet", level, format(date, "%Y"), format(date, "%Y-%m-%d"))
}

statcast_season_key <- function(level, season) sprintf("raw/statcast/%s/%s.parquet", level, season)

# Finished games on a date, from the schedule.
expected_game_pks <- function(date, level) {
  s <- fetch_schedule(as.integer(format(as.Date(date), "%Y")), level)
  s$game_pk[s$game_date == as.Date(date)]
}

backfill_failed <- function(log) any(grepl("_error$", log$status))

backfill_statcast <- function(level, seasons, fetch = fetch_statcast_day, days = game_days,
                              expected = expected_game_pks, today = Sys.Date(),
                              strict_days = 7, retry_wait = 60,
                              log_dir = file.path(cfg()$data_dir, "logs")) {
  all_days <- do.call(c, lapply(seasons, function(s) days(s, level)))
  rows <- vector("list", length(all_days))

  attempt <- function(date) {
    d <- tryCatch(fetch(date, level), error = function(e) e)
    if (inherits(d, "error")) return(list(d = NULL, status = "fetch_error", note = conditionMessage(d)))
    if (nrow(d) == 0) return(list(d = d, status = "empty_error", note = ""))
    if (!is.null(expected)) {
      missing <- setdiff(expected(date, level), d$game_pk)
      if (length(missing) > 0) {
        return(list(d = d, status = "incomplete_error",
                    note = paste("missing game_pk", paste(missing, collapse = ","))))
      }
    }
    list(d = d, status = "written", note = "")
  }

  for (i in seq_along(all_days)) {
    date <- all_days[i]
    key <- statcast_day_key(level, date)
    if (store_exists(key)) {
      rows[[i]] <- tibble::tibble(date = date, rows = NA_integer_, status = "skipped")
      next
    }
    res <- attempt(date)
    if (res$status != "written") {
      # Retry once: Savant sometimes returns a busy page or posts games late.
      if (retry_wait > 0) Sys.sleep(retry_wait)
      res <- attempt(date)
    }
    if (res$status == "incomplete_error" && as.Date(date) < as.Date(today) - strict_days) {
      res$status <- "written_partial"
    }
    if (res$status %in% c("written", "written_partial")) store_write(res$d, key)
    rows[[i]] <- tibble::tibble(date = date,
                                rows = if (is.null(res$d)) NA_integer_ else nrow(res$d),
                                status = res$status)
    if (res$status != "written") message(level, " ", date, ": ", res$status, " ", res$note)
  }

  log <- dplyr::bind_rows(rows)
  if (is.null(log) || nrow(log) == 0) {
    log <- tibble::tibble(date = as.Date(character()), rows = integer(), status = character())
  }
  if (!is.null(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(log, file.path(log_dir, sprintf("backfill_%s_%s.csv", level,
                                                     format(Sys.time(), "%Y%m%d_%H%M%S"))))
  }
  log
}

# Streams every day file of a season through duckdb into one parquet file, keeping
# the latest game_date for each pitch_uid (resumed games). Never loads the season
# into R, so it is safe on an 8 GB machine.
compact_season <- function(level, season) {
  dir <- store_path(sprintf("raw/statcast/%s/%s", level, season))
  key <- statcast_season_key(level, season)
  out <- store_path(key)
  tmp <- paste0(out, ".tmp-", Sys.getpid())
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '2GB'")
  sql <- sprintf(
    "COPY (
       SELECT * EXCLUDE (rn) FROM (
         SELECT *, row_number() OVER (PARTITION BY pitch_uid ORDER BY game_date DESC) AS rn
         FROM read_parquet('%s/*.parquet', union_by_name = true)
       ) WHERE rn = 1 ORDER BY game_date, pitch_uid
     ) TO '%s' (FORMAT parquet)",
    gsub("'", "''", dir), gsub("'", "''", tmp))
  DBI::dbExecute(con, sql)
  if (!file.rename(tmp, out)) stop("Could not move compacted file into place: ", out)
  key
}
