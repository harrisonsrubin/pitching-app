# The app's pitcher list (P2 design 4.1): name, hand, team(s) and season line for every
# pitcher with graded pitches. A pitcher's team in a game comes from Statcast (top of
# the inning = the home team is pitching), so a traded pitcher is listed under every
# team he pitched for; his current team is the one from his latest game.

outputs_key <- function(name) file.path("outputs", name)

pitcher_index <- function(pitches, stats, people, level, season) {
  games <- pitches %>%
    dplyr::mutate(team = ifelse(inning_topbot == "Top", home_team, away_team)) %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(
      pitches = dplyr::n(),
      p_throws = dplyr::first(p_throws),
      team_abbrev = team[order(game_date, game_pk, decreasing = TRUE)][1],
      teams = paste(unique(team[order(game_date, game_pk)]), collapse = ","),
      .groups = "drop")
  games %>%
    dplyr::left_join(people, by = c("pitcher" = "player_id")) %>%
    dplyr::left_join(dplyr::select(stats, player_id, g, gs, ip_outs, age), by = c("pitcher" = "player_id")) %>%
    dplyr::mutate(throws = dplyr::coalesce(throws, p_throws), level = level, season = as.integer(season),
                  pitches = as.integer(pitches)) %>%
    dplyr::select(level, season, pitcher, name, throws, team_abbrev, teams, pitches, g, gs, ip_outs, age) %>%
    dplyr::arrange(dplyr::desc(pitches))
}

pitchers_key <- function(level, season) outputs_key(sprintf("pitchers_%s_%s.parquet", level, season))

build_pitcher_index <- function(level, season) {
  p <- arrow::open_dataset(store_path(features_key(level, season))) %>%
    dplyr::select(pitcher, game_pk, game_date, home_team, away_team, inning_topbot, p_throws) %>%
    dplyr::collect()
  stats_key <- sprintf("raw/statsapi/pitching/%s/%s.parquet", level, season)
  stats <- if (store_exists(stats_key)) store_read(stats_key) else
    tibble::tibble(player_id = integer(), g = integer(), gs = integer(), ip_outs = integer(), age = integer())
  idx <- pitcher_index(p, stats, fetch_people(unique(p$pitcher)), level, season)
  store_write(idx, pitchers_key(level, season))
  idx
}
