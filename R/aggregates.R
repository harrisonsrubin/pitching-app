# Public per-pitcher aggregates for the P2b pages (Stuff Map, Command & Tunneling).
# Descriptive numbers only: none of these is a new grade.

# Statcast-style attack zones relative to each batter's own zone: distance from the
# zone's center as a share of its half-width (0.83 ft, with the ball) or half-height.
attack_zone <- function(plate_x, plate_z, sz_top, sz_bot) {
  half_h <- (sz_top - sz_bot) / 2
  ok <- !is.na(plate_x) & !is.na(plate_z) & !is.na(half_h) & half_h > 0
  m <- pmax(abs(plate_x) / 0.83, abs(plate_z - (sz_top + sz_bot) / 2) / half_h)
  out <- ifelse(m < 0.67, "Heart", ifelse(m < 1.33, "Shadow", ifelse(m < 2, "Chase", "Waste")))
  out[!ok] <- NA_character_
  out
}

# Where each pitch is when the hitter must decide: the plane 23.8 ft in front of the
# plate, found by running the pitch's own trajectory back from its plate location.
TUNNEL_FT <- 23.8
tunnel_position <- function(d, y_from_front = TUNNEL_FT) {
  y_front <- 17 / 12
  tp <- plane_time(d$vy0, d$ay, y_front)
  tt <- plane_time(d$vy0, d$ay, y_front + y_from_front)
  dt <- tp - tt; dt2 <- tp^2 - tt^2
  tibble::tibble(x_tun = d$plate_x - (d$vx0 * dt + 0.5 * d$ax * dt2),
                 z_tun = d$plate_z - (d$vz0 * dt + 0.5 * d$az * dt2))
}

# Pitch types thrown fewer than this many times are not published: a row that small is
# effectively one pitch's measurements, and pitch-level data stays private.
MIN_PUBLISHED_N <- 5L

pitch_profiles <- function(display) {
  display %>%
    dplyr::group_by(pitcher, p_throws, pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), velo = mean(velo, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
                     hb = mean(hb, na.rm = TRUE), spin = mean(spin_rate, na.rm = TRUE), ext = mean(ext, na.rm = TRUE),
                     arm_angle = mean(arm_angle, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(n >= MIN_PUBLISHED_N)
}

command_rates <- function(display) {
  z <- display %>%
    dplyr::mutate(az = attack_zone(plate_x, plate_z, sz_top, sz_bot),
                  in_zone = abs(plate_x) <= 0.83 & abs(plate_z - (sz_top + sz_bot) / 2) <= (sz_top - sz_bot) / 2) %>%
    dplyr::filter(!is.na(az))
  rates <- function(g) g %>% dplyr::summarise(
    n = dplyr::n(), zone = mean(in_zone), heart = mean(az == "Heart"), shadow = mean(az == "Shadow"),
    chase = mean(az == "Chase"), waste = mean(az == "Waste"), .groups = "drop")
  dplyr::bind_rows(rates(dplyr::group_by(z, pitcher, pitch_type)),
                   rates(dplyr::group_by(dplyr::mutate(z, pitch_type = "ALL"), pitcher, pitch_type))) %>%
    dplyr::filter(n >= MIN_PUBLISHED_N)
}

# Per batter side: two pitches thrown mostly to different sides would otherwise look far
# apart at the plate because of who they were thrown to, not deception.
tunnel_centroids <- function(display, min_n = 20) {
  display %>%
    dplyr::filter(!is.na(x_tun), !is.na(z_tun), !is.na(plate_x), !is.na(plate_z), !is.na(stand)) %>%
    dplyr::group_by(pitcher, stand, pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), x_tun = mean(x_tun), z_tun = mean(z_tun),
                     x_plate = mean(plate_x), z_plate = mean(plate_z), .groups = "drop") %>%
    dplyr::filter(n >= min_n)
}

profiles_key <- function(level, season) outputs_key(sprintf("profiles_%s_%s.parquet", level, season))
command_key <- function(level, season) outputs_key(sprintf("command_%s_%s.parquet", level, season))
tunnel_key <- function(level, season) outputs_key(sprintf("tunnel_%s_%s.parquet", level, season))
ANCHORS_OUTPUT <- "outputs/grade_anchors.csv"

# Publish the anchors these grades were built on, so the app's rolling grades use the same scale.
write_anchors_output <- function(anchors) {
  path <- store_path(ANCHORS_OUTPUT)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(anchors, path, na = "")
  invisible(path)
}
