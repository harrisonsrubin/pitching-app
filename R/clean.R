# Pitch cleaning: corrections, zone, physics, air density, park offsets, labels

# Pitch types

# Savant codes to app labels and groups; unlisted codes map to NA and get dropped

pitch_type_table <- function() {
  readr::read_csv(file.path(repo_root(), "reference", "pitch_type_map.csv"),
                  col_types = "ccc", progress = FALSE)
}

# Savant code to app label
map_pitch_type <- function(code) {
  tbl <- pitch_type_table()
  tbl$label[match(code, tbl$savant_code)]
}

# App label to group (FB / BR / OS)
pitch_group <- function(label) {
  tbl <- dplyr::distinct(pitch_type_table(), label, group)
  tbl$group[match(label, tbl$label)]
}

# Corrections

# Migrate old play_id-keyed corrections to pitch_uid (game_pk-at_bat-pitch)

CORRECTION_COLS <- c("pitch_uid", "play_id", "new_pitch_type", "original_pitch_type",
                     "corrected_at", "corrected_by")

# Read an old corrections file as character columns
read_old_corrections <- function(path) {
  d <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE, show_col_types = FALSE)
  d$corrected_at <- as.POSIXct(substr(d$corrected_at, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  d
}

# One row per pitch: the latest correction wins
dedupe_corrections <- function(df, key = "play_id") {
  df %>%
    dplyr::arrange(dplyr::desc(corrected_at)) %>%
    dplyr::distinct(.data[[key]], .keep_all = TRUE) %>%
    dplyr::arrange(corrected_at)
}

# Map play_id to pitch_uid from an old processed CSV via duckdb (atBatIndex is 0-based)
play_id_index <- function(processed_csv) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sql <- sprintf(
    "SELECT DISTINCT play_id,
            CAST(CAST(game_pk AS BIGINT) AS VARCHAR) || '-' ||
            CAST(CAST(atBatIndex AS BIGINT) + 1 AS VARCHAR) || '-' ||
            CAST(CAST(pitchNumber AS BIGINT) AS VARCHAR) AS pitch_uid
     FROM read_csv('%s', header = true, all_varchar = true)
     WHERE play_id IS NOT NULL AND play_id <> '' AND play_id <> 'NA'",
    gsub("'", "''", processed_csv))
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

# Map old corrections to pitch_uid, keeping unmapped rows for review
migrate_corrections <- function(old, index) {
  old <- dedupe_corrections(old, key = "play_id")
  joined <- dplyr::left_join(old, index, by = "play_id")
  mapped <- joined %>%
    dplyr::filter(!is.na(pitch_uid)) %>%
    dplyr::mutate(corrected_by = "admin") %>%
    dplyr::select(dplyr::all_of(CORRECTION_COLS)) %>%
    dedupe_corrections(key = "pitch_uid")
  unmapped <- dplyr::filter(joined, is.na(pitch_uid)) %>% dplyr::select(-pitch_uid)
  list(mapped = mapped, unmapped = unmapped)
}

# Check that original labels in mapped corrections agree with Savant
verify_against_statcast <- function(mapped, statcast) {
  j <- dplyr::inner_join(dplyr::filter(mapped, !is.na(original_pitch_type)),
                         dplyr::select(statcast, pitch_uid, pitch_type), by = "pitch_uid") %>%
    dplyr::mutate(savant_label = map_pitch_type(pitch_type))
  same <- !is.na(j$savant_label) & j$savant_label == j$original_pitch_type
  list(n_found = nrow(j),
       match_rate = if (nrow(j) == 0) NA_real_ else mean(same),
       mismatches = j[!same, ])
}

# Zone

# Put plate location and strike zone on one definition across pre-ABS and ABS eras

ABS_ERA <- list(MLB = 2026L, AAA = 2023L)  # first ABS-era season per level

is_abs_era <- function(level, season) season >= ABS_ERA[[level]]

# Time for a pitch to reach plane y, from the 9-parameter fit
plane_time <- function(vy0, ay, y) {
  a <- ay / 2; c <- 50 - y
  (-vy0 - sqrt(vy0^2 - 4 * a * c)) / (2 * a)
}

# Shift middle-of-plate locations to the front along each pitch's trajectory
to_front_plane <- function(d) {
  tf <- plane_time(d$vy0, d$ay, 17 / 12)
  tm <- plane_time(d$vy0, d$ay, 17 / 24)
  d$plate_x <- d$plate_x + d$vx0 * (tf - tm) + d$ax * (tf^2 - tm^2) / 2
  d$plate_z <- d$plate_z + d$vz0 * (tf - tm) + d$az * (tf^2 - tm^2) / 2
  d
}

# Median zone top and bottom per batter
batter_medians <- function(d) {
  d %>% dplyr::group_by(batter) %>%
    dplyr::summarise(top = stats::median(sz_top, na.rm = TRUE), bot = stats::median(sz_bot, na.rm = TRUE),
                     .groups = "drop")
}

# Linear map from ABS zone to posture zone, fit on batters seen in both eras
fit_zone_map <- function(posture, abs) {
  j <- dplyr::inner_join(batter_medians(posture), batter_medians(abs), by = "batter", suffix = c("_pos", "_abs"))
  ft <- stats::lm(top_pos ~ top_abs, data = j); fb <- stats::lm(bot_pos ~ bot_abs, data = j)
  tibble::tibble(edge = c("top", "bot"),
                 intercept = c(stats::coef(ft)[[1]], stats::coef(fb)[[1]]),
                 slope = c(stats::coef(ft)[[2]], stats::coef(fb)[[2]]),
                 r2 = c(summary(ft)$r.squared, summary(fb)$r.squared),
                 n_batters = nrow(j))
}

# Give each batter one zone, mapped to the posture scale in the ABS era
harmonize_zone <- function(d, zone_map, abs_era) {
  m <- batter_medians(d)
  i <- match(d$batter, m$batter)
  top <- m$top[i]; bot <- m$bot[i]
  if (abs_era) {
    zt <- zone_map[zone_map$edge == "top", ]; zb <- zone_map[zone_map$edge == "bot", ]
    top <- zt$intercept + zt$slope * top
    bot <- zb$intercept + zb$slope * bot
  }
  d$sz_top <- top; d$sz_bot <- bot
  d
}

# Physics

# Statcast coordinates: x to catcher's right, y toward mound, z up; feet and seconds

GRAVITY_FT <- -32.174
BALL_MASS_KG <- 0.1455
BALL_RADIUS_M <- 0.0366
FT_TO_M <- 0.3048

# Mirror lefties onto the right-handed side so one model serves both hands
mirror_x <- function(x, p_throws) x * ifelse(p_throws == "L", -1, 1)
mirror_axis <- function(axis, p_throws) ifelse(p_throws == "L", (360 - axis) %% 360, axis)

# Rescale movement to standard air (Magnus force scales with density)
neutral_movement <- function(pfx, rho) pfx * RHO_REF / rho

# Vertical and horizontal approach angles (degrees) at the front of the plate
approach_angles <- function(vx0, vy0, vz0, ax, ay, az) {
  y_plate <- 17 / 12
  vy_f <- -sqrt(vy0^2 - 2 * ay * (50 - y_plate))
  t <- (vy_f - vy0) / ay
  vz_f <- vz0 + az * t
  vx_f <- vx0 + ax * t
  tibble::tibble(vaa = -atan(vz_f / vy_f) * 180 / pi,
                 haa = -atan(vx_f / vy_f) * 180 / pi)
}

# Share of spin that is transverse, from inverting the lift coefficient
spin_efficiency <- function(vx0, vy0, vz0, ax, ay, az, spin_rate, rho) {
  # Mid-flight velocity stands in for the whole trip
  vm <- mid_flight_velocity(vx0, vy0, vz0, ax, ay, az)
  bx <- ax; by <- ay; bz <- az - GRAVITY_FT
  v <- sqrt(vm$vx^2 + vm$vy^2 + vm$vz^2)
  along <- (bx * vm$vx + by * vm$vy + bz * vm$vz) / v
  mx <- bx - along * vm$vx / v
  my <- by - along * vm$vy / v
  mz <- bz - along * vm$vz / v
  a_m <- sqrt(mx^2 + my^2 + mz^2) * FT_TO_M
  v_m <- v * FT_TO_M
  area <- pi * BALL_RADIUS_M^2
  cl <- pmin(pmax(2 * BALL_MASS_KG * a_m / (rho * area * v_m^2), 0), 0.42)
  s <- 0.4 * cl / (1 - 2.32 * cl)
  rpm_t <- s * v_m / BALL_RADIUS_M * 60 / (2 * pi)
  pmin(1, rpm_t / spin_rate)
}

# Velocity halfway (in time) between y = 50 ft and the front of the plate
mid_flight_velocity <- function(vx0, vy0, vz0, ax, ay, az) {
  vy_f <- -sqrt(vy0^2 - 2 * ay * (50 - 17 / 12))
  tm <- (vy_f - vy0) / ay / 2
  list(vx = vx0 + ax * tm, vy = vy0 + ay * tm, vz = vz0 + az * tm)
}

# Spin axis implied by movement direction (Savant convention, 180 = pure backspin)
movement_axis <- function(pfx_x, pfx_z) (180 + atan2(-pfx_x, pfx_z) * 180 / pi) %% 360

# Signed angular difference a - b in (-180, 180]
circ_diff <- function(a, b) {
  d <- (a - b) %% 360
  ifelse(d > 180, d - 360, d)
}

# Air

# Air density per game; movement is rescaled to sea level, 70°F, 50% humidity

STD_TEMP_C <- 21.111  # 70°F
STD_RH_PCT <- 50

# Moist air density (kg/m^3) from temp (°C), humidity (%) and pressure (hPa)
air_density <- function(temp_c, rh_pct, pressure_hpa) {
  es <- 6.1078 * 10^(7.5 * temp_c / (temp_c + 237.3))
  pv <- rh_pct / 100 * es
  tk <- temp_c + 273.15
  100 * (pressure_hpa - pv) / (287.058 * tk) + 100 * pv / (461.495 * tk)
}

# Standard-atmosphere pressure (hPa) at an elevation in feet
pressure_from_elevation <- function(elev_ft) {
  1013.25 * (1 - 2.25577e-5 * elev_ft * 0.3048)^5.25588
}

# Reference density for a standard day
RHO_REF <- air_density(STD_TEMP_C, STD_RH_PCT, 1013.25)

INDOOR_CONDITIONS <- c("Dome", "Roof Closed")

# One row per game; indoor or missing weather uses a standard day at park elevation
game_air <- function(schedule, weather, parks) {
  d <- schedule %>%
    dplyr::select(game_pk, venue_id, weather_condition) %>%
    dplyr::left_join(dplyr::select(weather, game_pk, temp_c, rh_pct, pressure_hpa), by = "game_pk") %>%
    dplyr::left_join(dplyr::select(parks, venue_id, elevation_ft, roof_type), by = "venue_id")
  elev_p <- pressure_from_elevation(dplyr::coalesce(d$elevation_ft, 0))
  indoor <- (d$weather_condition %in% INDOOR_CONDITIONS) | (d$roof_type %in% "Dome")
  have_wx <- !is.na(d$temp_c) & !is.na(d$rh_pct) & !is.na(d$pressure_hpa)
  p_indoor <- dplyr::coalesce(d$pressure_hpa, elev_p)
  rho <- dplyr::case_when(
    indoor  ~ air_density(STD_TEMP_C, STD_RH_PCT, p_indoor),
    have_wx ~ air_density(d$temp_c, d$rh_pct, d$pressure_hpa),
    TRUE    ~ air_density(STD_TEMP_C, STD_RH_PCT, elev_p)
  )
  tibble::tibble(
    game_pk = d$game_pk,
    rho = rho,
    indoor = indoor,
    air_source = ifelse(indoor, "indoor", ifelse(have_wx, "weather", "elevation_only"))
  )
}

# Park offsets

# Per-park tracking offsets, fit per metric from pitchers who throw in several parks

PARK_METRICS <- c("velo", "spin_rate", "rel_x", "rel_z", "ext", "hb", "ivb")
# Horizontal metrics are unmirrored to fit, so both hands get the same park correction
HORIZONTAL_METRICS <- c("rel_x", "hb")

# -1 for lefties on horizontal metrics, otherwise 1
hand_sign <- function(d, m) {
  if (m %in% HORIZONTAL_METRICS && "p_throws" %in% names(d)) ifelse(d$p_throws == "L", -1, 1) else 1
}

# Alternating least squares: pitcher x pitch-type means plus shrunk venue offsets
fit_park_offsets <- function(d, metrics = PARK_METRICS, k = 2000, iters = 5) {
  grp_all <- paste(d$pitcher, d$pitch_type)
  out <- lapply(metrics, function(m) {
    raw <- d[[m]] * hand_sign(d, m)
    ok <- !is.na(raw) & !is.na(d$venue_id)
    x <- raw[ok]; v <- d$venue_id[ok]; g <- grp_all[ok]
    # Only groups seen in at least two venues carry information
    n_venues <- tapply(v, g, function(z) length(unique(z)))
    keep <- n_venues[g] >= 2
    x <- x[keep]; v <- v[keep]; g <- g[keep]
    venues <- sort(unique(v))
    vi <- match(v, venues); gi <- match(g, unique(g))
    n_v <- tabulate(vi, length(venues))
    off <- numeric(length(venues))
    for (i in seq_len(iters)) {
      m_g <- rowsum(x - off[vi], gi)[, 1] / tabulate(gi)
      off <- rowsum(x - m_g[gi], vi)[, 1] / (n_v + k)
      off <- off - sum(off * n_v) / sum(n_v)
    }
    tibble::tibble(venue_id = venues, metric = m, offset = unname(off), n = n_v)
  })
  dplyr::bind_rows(out)
}

# Subtract each venue's offset from the metric
apply_park_offsets <- function(d, offsets) {
  for (m in unique(offsets$metric)) {
    o <- offsets[offsets$metric == m, ]
    adj <- o$offset[match(d$venue_id, o$venue_id)]
    d[[m]] <- d[[m]] - dplyr::coalesce(adj, 0) * hand_sign(d, m)
  }
  d
}

# Pitches

# Load one level-season for modeling: corrections, outcome flags, venue and air density

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

# Relabel corrected pitches; a "REMOVED" correction drops the pitch
apply_corrections <- function(d, corr) {
  if (is.null(corr) || nrow(corr) == 0) return(d)
  # Accept Savant codes (e.g. "SC") by mapping them to app labels
  labels <- c(unique(pitch_type_table()$label), "REMOVED")
  fixed <- ifelse(corr$new_pitch_type %in% labels, corr$new_pitch_type, map_pitch_type(corr$new_pitch_type))
  bad <- unique(corr$new_pitch_type[is.na(fixed)])
  if (length(bad)) stop("Unknown pitch type in corrections: ", paste(bad, collapse = ", "))
  corr$new_pitch_type <- fixed
  new_type <- corr$new_pitch_type[match(d$pitch_uid, corr$pitch_uid)]
  d$pitch_type <- ifelse(is.na(new_type), d$pitch_type, new_type)
  d[d$pitch_type != "REMOVED" | is.na(d$pitch_type), ]
}

# 0/1 swing, whiff, take, called strike and in-play flags
outcome_flags <- function(description) {
  tibble::tibble(
    swing         = as.integer(description %in% SWING_DESC),
    whiff         = as.integer(description %in% WHIFF_DESC),
    take          = as.integer(description %in% TAKE_DESC),
    called_strike = as.integer(description %in% "called_strike"),
    in_play       = as.integer(description %in% IN_PLAY_DESC)
  )
}

# Drop pitchouts, intentional and automatic calls, and unlabeled pitches
exclude_non_competitive <- function(d) {
  d[!(d$description %in% NON_COMPETITIVE_DESC) & !is.na(d$pitch_type), ]
}

# Season corrections file, or NULL if there is none
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
  # A game missing from the schedule gets standard air rather than NA
  d$rho <- dplyr::coalesce(d$rho, RHO_REF)
  d$indoor <- dplyr::coalesce(d$indoor, FALSE)
  d
}

# Prepare

# Prepared pitches: mirrored, air-neutral, physics added, park offsets removed

prepared_key <- function(level, season) sprintf("processed/prepared/%s/%s.parquet", level, season)
park_offsets_key <- function(level, season) sprintf("processed/park_offsets/%s/%s.parquet", level, season)
features_key <- function(level, season) sprintf("processed/features/%s/%s.parquet", level, season)

prepare_pitches <- function(level, season) {
  d <- load_pitches(level, season)
  # Keep the reported plate location before any shift
  d$plate_x_reported <- d$plate_x; d$plate_z_reported <- d$plate_z
  if (is_abs_era(level, season)) d <- to_front_plane(d)  # move ABS-era locations to the front plane
  d <- prepare_mirror(d)
  aa <- approach_angles(d$vx0, d$vy0, d$vz0, d$ax, d$ay, d$az)
  d$vaa <- aa$vaa
  d$haa <- mirror_x(aa$haa, d$p_throws)
  d$spin_eff <- spin_efficiency(d$vx0, d$vy0, d$vz0, d$ax, d$ay, d$az, d$release_spin_rate, d$rho)
  d$axis_gap <- circ_diff(d$spin_axis_m, mirror_axis(movement_axis(d$pfx_x, d$pfx_z), d$p_throws))
  d <- dplyr::rename(d, velo = release_speed, spin_rate = release_spin_rate,
                     rel_z = release_pos_z, ext = release_extension)
  offsets <- fit_park_offsets(d)
  d <- apply_park_offsets(d, offsets)
  list(pitches = d, offsets = dplyr::mutate(offsets, level = level, season = season))
}
