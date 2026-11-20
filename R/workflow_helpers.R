# Helpers the GitHub Actions workflows call (P2 design 6-7).

# Seasons whose corrections file is among the changed paths of a push.
changed_seasons <- function(paths) {
  m <- regmatches(paths, regexec("^corrections/pitch_corrections_(\\d{4})\\.csv$", paths))
  s <- vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, "")
  sort(unique(as.integer(stats::na.omit(s))))
}

# Levels that have data for a season (Triple-A Statcast starts in 2023).
levels_for <- function(season) if (season >= min(cfg()$seasons_aaa)) c("MLB", "AAA") else "MLB"
