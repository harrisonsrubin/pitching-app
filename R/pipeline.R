# One level-season through the pipeline (P2 design 7): features from prepared pitches,
# then everything the app reads. Used by scripts/update_season.R (nightly, rescore) and
# scripts/build_outputs.R (filling past seasons from stored features + xRV).

build_features_season <- function(level, season, refs) {
  d <- store_read(prepared_key(level, season))
  d <- add_stuff_features(d, refs)
  d <- harmonize_zone(d, refs$zone_map, is_abs_era(level, season))   # see R/zone.R
  d <- add_location_features(d)
  d <- add_sequence_features(d)
  d$target <- denoised_target(d, refs$bip)
  dplyr::select(d, -dplyr::any_of(c("vx0", "vy0", "vz0", "ax", "ay", "az")))
}

# Savant's own label for every pitch, before corrections.
savant_types <- function(level, season) {
  arrow::open_dataset(store_path(statcast_season_key(level, season))) %>%
    dplyr::select(pitch_uid, pitch_type) %>% dplyr::collect() %>%
    dplyr::transmute(pitch_uid, pitch_type_savant = map_pitch_type(pitch_type)) %>%
    dplyr::distinct(pitch_uid, .keep_all = TRUE)
}

grades_output_key <- function(level, season) outputs_key(sprintf("grades_%s_%s.parquet", level, season))
components_output_key <- function(level, season) outputs_key(sprintf("components_%s_%s.parquet", level, season))

# Grades (frozen anchors) -> components -> pitcher index -> display + references.
build_outputs <- function(level, season, anchors = read_anchors()) {
  x <- store_read(xrv_key(level, season))
  g <- grade_season(x, level, season, anchors)
  store_write(g$grades, grades_key(level, season)); store_write(g$grades, grades_output_key(level, season))
  store_write(g$components, components_key(level, season)); store_write(g$components, components_output_key(level, season))
  message(level, " ", season, ": ", sum(g$grades$half == "full" & g$grades$pitch_type == "ALL") / 3, " pitchers graded")
  rm(g); gc()
  idx <- build_pitcher_index(level, season)
  message(level, " ", season, ": index ", nrow(idx), " pitchers, names ", round(100 * mean(!is.na(idx$name)), 1), "%")
  f <- store_read(features_key(level, season))
  d <- display_rows(f, x, savant_types(level, season))
  rm(f, x); gc()
  # P2b aggregates: tunnel positions from each pitch's trajectory (prepared data)
  tr <- arrow::open_dataset(store_path(prepared_key(level, season))) %>%
    dplyr::select(pitch_uid, vy0, ay, vx0, ax, vz0, az) %>% dplyr::collect()
  tr <- dplyr::inner_join(dplyr::select(d, pitch_uid, plate_x, plate_z), tr, by = "pitch_uid")
  tun <- dplyr::bind_cols(dplyr::select(tr, pitch_uid), tunnel_position(tr))
  store_write(pitch_profiles(d), profiles_key(level, season))
  store_write(command_rates(d), command_key(level, season))
  store_write(tunnel_centroids(dplyr::inner_join(
    dplyr::select(d, pitch_uid, pitcher, stand, pitch_type, plate_x, plate_z), tun, by = "pitch_uid")),
    tunnel_key(level, season))
  write_anchors_output(anchors)
  rm(tr, tun); gc()
  write_projection_aggregates(level, season, x_key = xrv_key(level, season), anchors)
  sizes <- write_display(level, season, d)
  store_write(movement_ref(d), movement_ref_key(level, season))
  store_write(pitch_percentiles(d), pitch_pctl_key(level, season))
  message(level, " ", season, ": display ", nrow(d), " pitches in ", length(sizes), " buckets (max ", max(sizes), ")")
  invisible(TRUE)
}

# The season's schedule for prepare: the current season is always fetched live (its
# schedule is never cached), past seasons come from the store.
ensure_schedule <- function(level, season, repo, current) {
  key <- schedule_key(season, level)
  if (current) {
    store_write(fetch_schedule(season, level), key)
  } else if (!store_exists(key)) {
    release_download_file(key, repo)
  }
  invisible(key)
}

# The calendar year's season is "current": its Statcast lives as daily files in the
# store (no compacted season file is uploaded), and its schedule is fetched live.
is_current_season <- function(season, today = Sys.Date()) as.integer(season) == as.integer(format(as.Date(today), "%Y"))

has_day_files <- function(level, season) length(store_list(sprintf("raw/statcast/%s/%s", level, season))) > 0
