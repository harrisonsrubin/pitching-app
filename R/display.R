# Pitch-level display data for the app (P2 design 4.2). Private: written to the private
# store only, one file per pitcher bucket so a pitcher's whole season (trades included)
# is one small download. Plus two aggregate references that are public outputs: league
# movement ellipses by hand × type × arm-angle decile, and per-type xRV percentiles.

N_BUCKETS <- 32L

DISPLAY_COLS <- c("pitch_uid", "game_pk", "game_date", "pitcher", "batter", "stand", "p_throws",
                  "pitch_type", "pitch_type_savant", "balls", "strikes", "description", "events",
                  "velo", "spin_rate", "ivb", "hb", "ext", "rel_x", "rel_z", "arm_angle", "vaa",
                  "plate_x", "plate_z", "sz_top", "sz_bot", "stuff_xrv", "location_xrv", "pitching_xrv")

pitcher_bucket <- function(pitcher) as.integer(pitcher %% N_BUCKETS)

display_key <- function(level, season, k) sprintf("display/%s/%s/bucket_%d.parquet", level, season, as.integer(k))
movement_ref_key <- function(level, season) outputs_key(sprintf("movement_ref_%s_%s.parquet", level, season))
pitch_pctl_key <- function(level, season) outputs_key(sprintf("pitch_pctl_%s_%s.parquet", level, season))

# features: one level-season of feature rows (mirrored frame); xrv: pitch_uid + facet
# xRVs; savant: pitch_uid + pitch_type_savant (Savant's label before corrections).
display_rows <- function(features, xrv, savant) {
  features %>%
    dplyr::inner_join(dplyr::select(xrv, pitch_uid, stuff_xrv, location_xrv, pitching_xrv), by = "pitch_uid") %>%
    dplyr::left_join(savant, by = "pitch_uid") %>%
    dplyr::mutate(hb = mirror_x(hb, p_throws), rel_x = mirror_x(rel_x, p_throws)) %>%
    dplyr::select(dplyr::all_of(DISPLAY_COLS))
}

split_buckets <- function(d) {
  k <- pitcher_bucket(d$pitcher)
  lapply(0:(N_BUCKETS - 1L), function(i) d[k == i, ])
}

write_display <- function(level, season, d) {
  b <- split_buckets(d)
  for (i in seq_along(b)) store_write(b[[i]], display_key(level, season, i - 1L))
  invisible(vapply(b, nrow, 1L))
}

movement_ref <- function(d, min_n = 200) {
  d %>%
    dplyr::filter(!is.na(arm_angle), !is.na(hb), !is.na(ivb)) %>%
    dplyr::group_by(p_throws) %>%
    dplyr::mutate(arm_decile = dplyr::ntile(arm_angle, 10)) %>%
    dplyr::group_by(p_throws, arm_decile) %>%
    dplyr::mutate(arm_lo = min(arm_angle), arm_hi = max(arm_angle)) %>%
    dplyr::group_by(p_throws, pitch_type, arm_decile, arm_lo, arm_hi) %>%
    dplyr::summarise(n = dplyr::n(), mean_hb = mean(hb), mean_ivb = mean(ivb),
                     sd_hb = stats::sd(hb), sd_ivb = stats::sd(ivb),
                     rho = if (dplyr::n() > 2) suppressWarnings(stats::cor(hb, ivb)) else NA_real_,
                     .groups = "drop") %>%
    dplyr::filter(n >= min_n) %>%
    dplyr::mutate(rho = dplyr::coalesce(rho, 0))
}

PCTL_FACETS <- c("stuff", "location", "pitching")

pitch_percentiles <- function(d) {
  dplyr::bind_rows(lapply(PCTL_FACETS, function(f) {
    d %>% dplyr::filter(!is.na(.data[[paste0(f, "_xrv")]])) %>%
      dplyr::group_by(pitch_type) %>%
      dplyr::summarise(q = list(stats::quantile(.data[[paste0(f, "_xrv")]], seq(0, 1, 0.01), names = FALSE)),
                       .groups = "drop") %>%
      dplyr::mutate(facet = f, .after = pitch_type)
  }))
}

# Percentile of each xRV among that type's pitches; lower xRV is better, so higher pctl.
pctl_of <- function(xrv, q) {
  p <- stats::approx(q, seq(0, 100, 1), xout = xrv, rule = 2, ties = "ordered")$y
  round(100 - p)
}
