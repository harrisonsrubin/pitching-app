# Load one level-season of pitches for modeling: only the columns P1 needs, with
# pitch-type corrections applied before anything else (README 5.4), non-competitive
# pitches removed, outcome flags added, and each game's venue and air density joined.

PITCH_COLS <- c(
  "pitch_uid", "game_pk", "game_date", "season", "level", "home_team", "away_team",
  "pitcher", "batter", "stand", "p_throws", "pitch_type",
  "balls", "strikes", "outs_when_up", "inning", "inning_topbot", "at_bat_number", "pitch_number",
  "n_thruorder_pitcher", "description", "events", "bb_type",
  "release_speed", "release_spin_rate", "spin_axis", "release_pos_x", "release_pos_z",
  "release_extension", "arm_angle", "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot",
  "vx0", "vy0", "vz0", "ax", "ay", "az", "delta_run_exp", "estimated_woba_using_speedangle",
  "bat_score", "post_bat_score", "post_home_score", "post_away_score"
)

SWING_DESC   <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play",
                  "foul_bunt", "missed_bunt", "bunt_foul_tip")
WHIFF_DESC   <- c("swinging_strike", "swinging_strike_blocked", "missed_bunt")
TAKE_DESC    <- c("ball", "blocked_ball", "called_strike", "hit_by_pitch")
IN_PLAY_DESC <- "hit_into_play"
NON_COMPETITIVE_DESC <- c("pitchout", "intent_ball", "automatic_ball", "automatic_strike")

# Relabel corrected pitches; a "REMOVED" correction drops the pitch.
apply_corrections <- function(d, corr) {
  if (is.null(corr) || nrow(corr) == 0) return(d)
  # Corrections should use app labels; a Savant code (e.g. "SC") is mapped to its label.
  labels <- c(unique(pitch_type_table()$label), "REMOVED")
  fixed <- ifelse(corr$new_pitch_type %in% labels, corr$new_pitch_type, map_pitch_type(corr$new_pitch_type))
  bad <- unique(corr$new_pitch_type[is.na(fixed)])
  if (length(bad)) stop("Unknown pitch type in corrections: ", paste(bad, collapse = ", "))
  corr$new_pitch_type <- fixed
  new_type <- corr$new_pitch_type[match(d$pitch_uid, corr$pitch_uid)]
  d$pitch_type <- ifelse(is.na(new_type), d$pitch_type, new_type)
  d[d$pitch_type != "REMOVED" | is.na(d$pitch_type), ]
}

outcome_flags <- function(description) {
  tibble::tibble(
    swing         = as.integer(description %in% SWING_DESC),
    whiff         = as.integer(description %in% WHIFF_DESC),
    take          = as.integer(description %in% TAKE_DESC),
    called_strike = as.integer(description %in% "called_strike"),
    in_play       = as.integer(description %in% IN_PLAY_DESC)
  )
}

exclude_non_competitive <- function(d) {
  d[!(d$description %in% NON_COMPETITIVE_DESC) & !is.na(d$pitch_type), ]
}

read_corrections <- function(season) {
  f <- file.path(repo_root(), "corrections", sprintf("pitch_corrections_%s.csv", season))
  if (!file.exists(f)) return(NULL)
  readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
}

read_parks <- function() {
  readr::read_csv(file.path(repo_root(), "reference", "parks.csv"), show_col_types = FALSE)
}

load_pitches <- function(level, season) {
  d <- arrow::open_dataset(store_path(statcast_season_key(level, season))) %>%
    dplyr::select(dplyr::all_of(PITCH_COLS)) %>%
    dplyr::collect()
  d$pitch_type <- map_pitch_type(d$pitch_type)
  d <- apply_corrections(d, read_corrections(season))
  d <- exclude_non_competitive(d)
  d$pitch_group <- pitch_group(d$pitch_type)
  d <- dplyr::bind_cols(d, outcome_flags(d$description))

  sched <- store_read(sprintf("raw/statsapi/schedule/%s/%s.parquet", level, season))
  wx_key <- sprintf("raw/weather/game_weather_%s_%s.parquet", level, season)
  wx <- if (store_exists(wx_key)) store_read(wx_key) else
    tibble::tibble(game_pk = integer(), temp_c = numeric(), rh_pct = numeric(), pressure_hpa = numeric())
  air <- game_air(sched, wx, read_parks())
  d <- d %>%
    dplyr::left_join(dplyr::select(sched, game_pk, venue_id), by = "game_pk") %>%
    dplyr::left_join(dplyr::select(air, game_pk, rho, indoor), by = "game_pk")
  # A game missing from the schedule (rare) gets standard air rather than NA.
  d$rho <- dplyr::coalesce(d$rho, RHO_REF)
  d$indoor <- dplyr::coalesce(d$indoor, FALSE)
  d
}
