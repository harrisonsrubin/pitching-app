# Data-quality checks on a backfilled Statcast season. Each check returns
# tibble(check, value, pass); validate_statcast_season() stacks them.

check_unique_uid <- function(d) {
  dup <- sum(duplicated(d$pitch_uid))
  tibble::tibble(check = "unique_pitch_uid", value = dup, pass = dup == 0)
}

check_game_coverage <- function(statcast, schedule, bar = 0.99) {
  share <- mean(schedule$game_pk %in% statcast$game_pk)
  tibble::tibble(check = "share_of_final_games_with_pitches", value = share, pass = share >= bar)
}

check_non_missing <- function(d, cols, bar = 0.97) {
  share <- vapply(cols, function(cl) mean(!is.na(d[[cl]])), numeric(1))
  tibble::tibble(check = paste0("non_missing_", cols), value = unname(share), pass = unname(share >= bar))
}

row_bar <- function(level, season) {
  if (level == "MLB" && season == 2020) return(200000)
  if (level == "MLB") return(650000)
  300000
}

KEY_PHYSICAL_COLS <- c("release_speed", "pfx_x", "pfx_z", "plate_x", "plate_z", "release_extension")

validate_statcast_season <- function(level, season) {
  d <- store_read(statcast_season_key(level, season))
  sched <- fetch_schedule(season, level)
  arm <- mean(!is.na(d$arm_angle))
  dplyr::bind_rows(
    tibble::tibble(check = "rows", value = nrow(d), pass = nrow(d) >= row_bar(level, season)),
    check_unique_uid(d),
    check_game_coverage(d, sched),
    check_non_missing(d, KEY_PHYSICAL_COLS),
    tibble::tibble(check = "non_missing_arm_angle (reported only)", value = arm, pass = NA)
  ) %>%
    dplyr::mutate(level = level, season = season, .before = 1)
}
