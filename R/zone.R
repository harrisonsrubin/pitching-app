# Plate location and strike zone made consistent across eras.
# Statcast changed both definitions with the automated ball-strike (ABS) system,
# verified from the data (release height implied by each pitch's trajectory):
#   MLB 2020–2025:           plate_x/plate_z at the FRONT of the plate (y = 17 in);
#                            sz_top/sz_bot = posture zone, varies pitch to pitch.
#   MLB 2026, AAA 2023–2026: plate_x/plate_z at the MIDDLE of the plate (y = 8.5 in);
#                            sz_top/sz_bot = ABS zone, fixed per batter from height.
# Everything is converted to the front plane and to a per-batter zone on the
# 2023–2025 posture scale, so the models see one definition.

ABS_ERA <- list(MLB = 2026L, AAA = 2023L)   # first ABS-era season; every later one is too

is_abs_era <- function(level, season) season >= ABS_ERA[[level]]

plane_time <- function(vy0, ay, y) {
  a <- ay / 2; c <- 50 - y
  (-vy0 - sqrt(vy0^2 - 4 * a * c)) / (2 * a)
}

# Shift locations reported at the middle of the plate to the front, along each
# pitch's own trajectory.
to_front_plane <- function(d) {
  tf <- plane_time(d$vy0, d$ay, 17 / 12)
  tm <- plane_time(d$vy0, d$ay, 17 / 24)
  d$plate_x <- d$plate_x + d$vx0 * (tf - tm) + d$ax * (tf^2 - tm^2) / 2
  d$plate_z <- d$plate_z + d$vz0 * (tf - tm) + d$az * (tf^2 - tm^2) / 2
  d
}

batter_medians <- function(d) {
  d %>% dplyr::group_by(batter) %>%
    dplyr::summarise(top = stats::median(sz_top, na.rm = TRUE), bot = stats::median(sz_bot, na.rm = TRUE),
                     .groups = "drop")
}

# Linear map from the ABS zone to the posture zone, fit on batters seen in both a
# posture-era season and an ABS-era season.
fit_zone_map <- function(posture, abs) {
  j <- dplyr::inner_join(batter_medians(posture), batter_medians(abs), by = "batter", suffix = c("_pos", "_abs"))
  ft <- stats::lm(top_pos ~ top_abs, data = j); fb <- stats::lm(bot_pos ~ bot_abs, data = j)
  tibble::tibble(edge = c("top", "bot"),
                 intercept = c(stats::coef(ft)[[1]], stats::coef(fb)[[1]]),
                 slope = c(stats::coef(ft)[[2]], stats::coef(fb)[[2]]),
                 r2 = c(summary(ft)$r.squared, summary(fb)$r.squared),
                 n_batters = nrow(j))
}

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
