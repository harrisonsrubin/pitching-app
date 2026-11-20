# Baseball Savant pitch-level Statcast, one day at a time, for MLB and Triple-A.
#
# Endpoints (verified 2026-11-19 against 2025-06-01):
#   MLB: https://baseballsavant.mlb.com/statcast_search/csv
#   AAA: https://baseballsavant.mlb.com/statcast-search-minors/csv
#        plus minors=true and hfLevel=AAA|. Without minors=true this endpoint
#        silently returns MLB pitches.
#   Shared params: all=true, type=details, player_type=pitcher, hfGT=R|,
#                  game_date_gt=<date>, game_date_lt=<date> (same date = one day).
#   Response: CSV with a UTF-8 byte-order mark, 119 columns, identical for both levels
#   (~4,200 MLB and ~4,600 AAA pitches on a 15-game day). Header-only when no games.
#
# Column coverage at AAA (2025-06-01): arm_angle, release_extension, spin_axis,
#   9-parameter trajectory, sz_top/sz_bot, delta_run_exp all present. bat_speed,
#   swing_length, attack_angle are empty at AAA (bat tracking is MLB-only).

SAVANT_URLS <- c(
  MLB = "https://baseballsavant.mlb.com/statcast_search/csv",
  AAA = "https://baseballsavant.mlb.com/statcast-search-minors/csv"
)

statcast_numeric_cols <- function() {
  c("release_speed", "release_pos_x", "release_pos_y", "release_pos_z", "release_extension",
    "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot",
    "vx0", "vy0", "vz0", "ax", "ay", "az",
    "release_spin_rate", "spin_axis", "arm_angle", "effective_speed",
    "launch_speed", "launch_angle", "hit_distance_sc",
    "estimated_ba_using_speedangle", "estimated_woba_using_speedangle",
    "woba_value", "woba_denom", "babip_value", "iso_value",
    "delta_run_exp", "delta_home_win_exp", "delta_pitcher_run_exp",
    "bat_speed", "swing_length", "attack_angle", "attack_direction", "swing_path_tilt",
    "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in",
    "balls", "strikes", "outs_when_up", "inning", "at_bat_number", "pitch_number",
    "game_pk", "batter", "pitcher", "on_1b", "on_2b", "on_3b", "zone",
    "home_score", "away_score", "bat_score", "fld_score", "post_bat_score", "post_fld_score",
    "n_thruorder_pitcher", "n_priorpa_thisgame_player_at_bat",
    "age_pit", "age_bat", "pitcher_days_since_prev_game", "batter_days_since_prev_game",
    "hc_x", "hc_y", "spin_dir")
}

savant_request <- function(date, level) {
  if (!(length(level) == 1 && level %in% names(SAVANT_URLS))) {
    stop("level must be one of: ", paste(names(SAVANT_URLS), collapse = ", "))
  }
  d <- format(as.Date(date), "%Y-%m-%d")
  query <- list(all = "true", type = "details", player_type = "pitcher", hfGT = "R|",
                game_date_gt = d, game_date_lt = d)
  if (level == "AAA") query <- c(query, list(minors = "true", hfLevel = "AAA|"))
  list(url = unname(SAVANT_URLS[level]), query = query)
}

pv_bad_response <- function(msg) {
  rlang::abort(msg, class = "pv_bad_response")
}

parse_statcast_csv <- function(text, level) {
  text <- sub("^﻿", "", text)
  if (grepl("^\\s*<", text)) pv_bad_response("Savant returned HTML instead of CSV")

  d <- readr::read_csv(I(text), col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE, show_col_types = FALSE)
  names(d) <- sub("^﻿", "", names(d))
  if (!all(c("game_pk", "at_bat_number", "pitch_number", "game_date") %in% names(d))) {
    pv_bad_response("Savant CSV is missing its key columns")
  }

  num <- intersect(statcast_numeric_cols(), names(d))
  d %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num), readr::parse_double)) %>%
    dplyr::filter(game_type == "R") %>%
    dplyr::mutate(
      game_date = as.Date(game_date),
      season    = as.integer(format(game_date, "%Y")),
      level     = level,
      pitch_uid = paste(as.integer(game_pk), as.integer(at_bat_number),
                        as.integer(pitch_number), sep = "-")
    )
}

fetch_statcast_day <- function(date, level) {
  r <- savant_request(date, level)
  resp <- http_get(r$url, r$query)
  parse_statcast_csv(httr2::resp_body_string(resp, encoding = "UTF-8"), level)
}
