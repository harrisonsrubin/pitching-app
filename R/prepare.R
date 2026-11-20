# Stage 1 of P1: one level-season of pitches, mirrored, air-neutral, with physics
# columns and per-park tracking offsets removed. Raw pfx_x / pfx_z are kept for the
# app's raw-vs-neutral display (README 6).

prepared_key <- function(level, season) sprintf("processed/prepared/%s/%s.parquet", level, season)
park_offsets_key <- function(level, season) sprintf("processed/park_offsets/%s/%s.parquet", level, season)
features_key <- function(level, season) sprintf("processed/features/%s/%s.parquet", level, season)

prepare_pitches <- function(level, season) {
  d <- load_pitches(level, season)
  d$plate_x_reported <- d$plate_x; d$plate_z_reported <- d$plate_z
  if (is_abs_era(level, season)) d <- to_front_plane(d)   # see R/zone.R
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
