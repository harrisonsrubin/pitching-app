# Pitch-type corrections: migration from the old app's play_id-keyed files to
# pitch_uid ("<game_pk>-<at_bat_number>-<pitch_number>", Savant numbering).

CORRECTION_COLS <- c("pitch_uid", "play_id", "new_pitch_type", "original_pitch_type",
                     "corrected_at", "corrected_by")

read_old_corrections <- function(path) {
  d <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE, show_col_types = FALSE)
  d$corrected_at <- as.POSIXct(substr(d$corrected_at, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  d
}

# One row per pitch: the latest correction wins.
dedupe_corrections <- function(df, key = "play_id") {
  df %>%
    dplyr::arrange(dplyr::desc(corrected_at)) %>%
    dplyr::distinct(.data[[key]], .keep_all = TRUE) %>%
    dplyr::arrange(corrected_at)
}

# play_id -> pitch_uid from an old processed CSV, reading only four columns through
# duckdb so the 2 GB file never lands in R. The old data's atBatIndex is 0-based;
# Savant's at_bat_number is 1-based. pitchNumber is already 1-based within the PA.
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

verify_against_statcast <- function(mapped, statcast) {
  j <- dplyr::inner_join(dplyr::filter(mapped, !is.na(original_pitch_type)),
                         dplyr::select(statcast, pitch_uid, pitch_type), by = "pitch_uid") %>%
    dplyr::mutate(savant_label = map_pitch_type(pitch_type))
  same <- !is.na(j$savant_label) & j$savant_label == j$original_pitch_type
  list(n_found = nrow(j),
       match_rate = if (nrow(j) == 0) NA_real_ else mean(same),
       mismatches = j[!same, ])
}
