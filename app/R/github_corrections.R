# Saving pitch-type corrections (P2 design 6, README 5.4): read the season's CSV from
# the repo through the GitHub contents API, upsert by pitch_uid (latest wins), and write
# it back with the sha it was read at. If it changed in between, re-read and retry once;
# then report an error rather than overwrite. Only corrections files can be written.

CORRECTION_COLS <- c("pitch_uid", "play_id", "new_pitch_type", "original_pitch_type", "corrected_at", "corrected_by")
CORRECTIONS_PATH_RE <- "^corrections/pitch_corrections_\\d{4}\\.csv$"

corrections_path <- function(season) sprintf("corrections/pitch_corrections_%d.csv", as.integer(season))

empty_corrections <- function() {
  stats::setNames(as.data.frame(replicate(length(CORRECTION_COLS), character(), simplify = FALSE)), CORRECTION_COLS) %>%
    tibble::as_tibble()
}

upsert_corrections <- function(existing, new, now = Sys.time()) {
  stamp <- format(as.POSIXct(now, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  add <- tibble::tibble(pitch_uid = new$pitch_uid, play_id = NA_character_, new_pitch_type = new$new_pitch_type,
                        original_pitch_type = new$original_pitch_type, corrected_at = stamp, corrected_by = "admin")
  existing <- dplyr::mutate(existing, dplyr::across(dplyr::everything(), as.character))
  dplyr::bind_rows(existing[!existing$pitch_uid %in% add$pitch_uid, ], add) %>%
    dplyr::select(dplyr::all_of(CORRECTION_COLS)) %>%
    dplyr::arrange(corrected_at, pitch_uid)
}

validate_corrections <- function(new, display) {
  msgs <- character()
  bad <- setdiff(unique(new$new_pitch_type), c(PITCH_LABELS, "REMOVED"))
  if (length(bad)) msgs <- c(msgs, paste("Unknown pitch type:", paste(bad, collapse = ", ")))
  missing <- setdiff(new$pitch_uid, display$pitch_uid)
  if (length(missing)) msgs <- c(msgs, paste("Unknown pitch:", paste(utils::head(missing, 5), collapse = ", ")))
  msgs
}

contents_url <- function(path, cfg) sprintf("%s/repos/%s/contents/%s", GITHUB_API, cfg$app_repo, path)

# list(rows, sha); a missing file is an empty table with no sha.
github_get_corrections <- function(path, cfg) {
  meta <- httr2::req_perform(gh_request(contents_url(path, cfg), cfg$corrections_token))
  if (httr2::resp_status(meta) == 404) return(list(rows = empty_corrections(), sha = NULL))
  if (httr2::resp_status(meta) != 200) stop("Could not read ", path, " (HTTP ", httr2::resp_status(meta), ")")
  sha <- httr2::resp_body_json(meta)$sha
  raw <- httr2::req_perform(gh_request(contents_url(path, cfg), cfg$corrections_token) %>%
                              httr2::req_headers(Accept = "application/vnd.github.raw+json"))
  if (httr2::resp_status(raw) != 200) stop("Could not read ", path, " (HTTP ", httr2::resp_status(raw), ")")
  rows <- readr::read_csv(I(httr2::resp_body_string(raw)), col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  list(rows = dplyr::bind_rows(empty_corrections(), rows), sha = sha)
}

# Returns the HTTP status of the write.
github_put_file <- function(path, csv_text, sha, message, cfg) {
  if (!grepl(CORRECTIONS_PATH_RE, path)) stop("Only corrections files can be written, not ", path)
  body <- list(message = message, content = jsonlite::base64_enc(charToRaw(csv_text)))
  if (!is.null(sha)) body$sha <- sha
  resp <- httr2::req_perform(gh_request(contents_url(path, cfg), cfg$corrections_token) %>%
                               httr2::req_method("PUT") %>% httr2::req_body_json(body))
  httr2::resp_status(resp)
}

corrections_csv <- function(rows) {
  paste0(paste(utils::capture.output(readr::write_csv(rows, stdout(), na = "")), collapse = "\n"), "\n")
}

save_corrections <- function(new, season, cfg) {
  path <- corrections_path(season)
  msg <- sprintf("Pitch-type corrections %d (%d pitches)", as.integer(season), nrow(new))
  for (attempt in 1:2) {
    cur <- tryCatch(github_get_corrections(path, cfg), error = function(e) e)
    if (inherits(cur, "error")) return(list(ok = FALSE, message = conditionMessage(cur)))
    status <- github_put_file(path, corrections_csv(upsert_corrections(cur$rows, new)), cur$sha, msg, cfg)
    if (status %in% c(200L, 201L)) return(list(ok = TRUE, message = sprintf("Saved %d pitch(es).", nrow(new))))
    if (!status %in% c(409L, 422L)) return(list(ok = FALSE, message = sprintf("Save failed (HTTP %d).", status)))
  }
  list(ok = FALSE, message = "The corrections file changed while saving. Nothing was overwritten; reload and try again.")
}

apply_selection <- function(display, uids, new_type) {
  if (identical(new_type, "REMOVED")) return(display[!display$pitch_uid %in% uids, ])
  display$pitch_type[display$pitch_uid %in% uids] <- new_type
  display
}

apply_selection_edits <- function(display, edits) {
  if (is.null(edits) || nrow(edits) == 0) return(display)
  for (ty in unique(edits$new_pitch_type)) display <- apply_selection(display, edits$pitch_uid[edits$new_pitch_type == ty], ty)
  display
}

# REMOVED pitches are gone from the display data, so they are found in the season's
# corrections file: those in a plate appearance (game_pk-at_bat) this pitcher threw in.
removed_for_pitcher <- function(corr, display) {
  if (is.null(corr) || nrow(corr) == 0 || is.null(display) || nrow(display) == 0) return(corr[0, ])
  pa <- function(uid) sub("-\\d+$", "", uid)
  corr[corr$new_pitch_type == "REMOVED" & pa(corr$pitch_uid) %in% pa(display$pitch_uid), ]
}

# The season's corrections as the app reads them: the repo file (local mode) or GitHub.
read_season_corrections <- function(season, cfg) {
  if (isTRUE(cfg$local)) {
    f <- file.path(cfg$data_dir, "..", corrections_path(season))
    if (!file.exists(f)) return(empty_corrections())
    return(dplyr::bind_rows(empty_corrections(), readr::read_csv(f, col_types = readr::cols(.default = "c"), show_col_types = FALSE)))
  }
  tryCatch(github_get_corrections(corrections_path(season), cfg)$rows, error = function(e) empty_corrections())
}
