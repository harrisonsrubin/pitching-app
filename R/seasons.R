# The latest season the pipeline covers (README "Season rollover", P2 design 7): the
# latest year whose MLB regular season has started, so 2027 joins every season list
# once its first game has been played. Never below 2026. Evidence: a stored schedule
# with a game on or before today, or an ingested Statcast day file (the nightly job
# always has these; current-season schedules are not cached).
# Memoized per process for each (data dir, date), since cfg() calls it often.

.pv_seasons <- new.env(parent = emptyenv())

last_season <- function(today = getOption("pv.today", Sys.Date())) {
  today <- as.Date(today)
  data_dir <- Sys.getenv("PV_DATA_DIR", unset = "")
  if (!nzchar(data_dir)) data_dir <- file.path(repo_root(), "data")
  memo <- paste(data_dir, today)
  if (!is.null(.pv_seasons[[memo]])) return(.pv_seasons[[memo]])
  files <- list.files(file.path(data_dir, "raw", "statsapi", "schedule", "MLB"),
                      pattern = "^\\d{4}\\.parquet$", full.names = TRUE)
  started <- vapply(files, function(f) {
    d <- arrow::read_parquet(f, col_select = "game_date")$game_date
    length(d) > 0 && any(as.Date(d) <= today)
  }, logical(1))
  seasons <- as.integer(sub("\\.parquet$", "", basename(files)))[started]
  days <- list.files(file.path(data_dir, "raw", "statcast", "MLB"), pattern = "^\\d{4}-\\d{2}-\\d{2}\\.parquet$",
                     recursive = TRUE)
  day_dates <- as.Date(sub("\\.parquet$", "", basename(days)))
  seasons <- c(seasons, as.integer(format(day_dates[day_dates <= today], "%Y")))
  out <- max(c(2026L, seasons))
  .pv_seasons[[memo]] <- out
  out
}
