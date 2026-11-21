# Data: configuration, the private store, polite HTTP, and the fetchers for Statcast, the MLB Stats API,
# weather and FanGraphs, with the nightly ingest, backfill and data-quality checks.

# ==== config ==============================================================================

# Project-wide settings. Every other file reads seasons, levels, and paths from here.

repo_root <- function() {
  if (exists(".pv_root", envir = globalenv())) return(get(".pv_root", envir = globalenv()))
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "R", "load.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Could not find the repo root (R/load.R).")
    root <- parent
  }
  root
}

cfg <- function() {
  data_dir <- Sys.getenv("PV_DATA_DIR", unset = "")
  if (!nzchar(data_dir)) data_dir <- file.path(repo_root(), "data")
  last <- last_season()   # R/data.R: rolls to 2027 once its first game is played
  list(
    seasons_mlb   = 2020:last,
    seasons_aaa   = 2023:last,
    seasons_stats = 2010:last,
    sport_ids     = c(MLB = 1L, AAA = 11L),
    data_dir      = data_dir,
    user_agent    = "pitching-app-ingest/0.1",
    # P1 pitch models: training seasons (README 5.2), grade anchor seasons (README 7.6)
    seasons_train = 2023:last,
    seasons_ref   = 2023:2025,
    recency_decay = 0.8,
    n_folds       = 5L,
    lgb_params    = list(learning_rate = 0.05, num_leaves = 63L, min_data_in_leaf = 500L,
                         feature_fraction = 0.8, bagging_fraction = 0.8, bagging_freq = 1L,
                         max_bin = 63L, num_threads = 6L, seed = 2026L, verbose = -1L),
    lgb_max_rounds = 3000L,
    lgb_early_stop = 100L
  )
}

`%>%` <- magrittr::`%>%`

# ==== seasons =============================================================================

# The latest season the pipeline covers (README "Season rollover", P2 design 7): the
# latest year whose MLB regular season has started, so 2027 joins every season list
# once its first game has been played. Never below 2026. Evidence: a stored schedule
# with a game on or before today, or an ingested Statcast day file (the nightly job
# always has these; current-season schedules are not cached).
# Memoized per process for each (data dir, date), since cfg() calls it often.

.pv_seasons <- new.env(parent = emptyenv())

last_season <- function(today = getOption("pv.today", Sys.Date())) {
  today <- as.Date(today)
  data_dir <- Sys.getenv("PV_DATA_DIR", unset = "")
  if (!nzchar(data_dir)) data_dir <- file.path(repo_root(), "data")
  memo <- paste(data_dir, today)
  if (!is.null(.pv_seasons[[memo]])) return(.pv_seasons[[memo]])
  files <- list.files(file.path(data_dir, "raw", "statsapi", "schedule", "MLB"),
                      pattern = "^\\d{4}\\.parquet$", full.names = TRUE)
  started <- vapply(files, function(f) {
    d <- arrow::read_parquet(f, col_select = "game_date")$game_date
    length(d) > 0 && any(as.Date(d) <= today)
  }, logical(1))
  seasons <- as.integer(sub("\\.parquet$", "", basename(files)))[started]
  days <- list.files(file.path(data_dir, "raw", "statcast", "MLB"), pattern = "^\\d{4}-\\d{2}-\\d{2}\\.parquet$",
                     recursive = TRUE)
  day_dates <- as.Date(sub("\\.parquet$", "", basename(days)))
  seasons <- c(seasons, as.integer(format(day_dates[day_dates <= today], "%Y")))
  out <- max(c(2026L, seasons))
  .pv_seasons[[memo]] <- out
  out
}

# ==== store ===============================================================================

# Data store: parquet files under cfg()$data_dir, addressed by keys like
# "raw/statcast/MLB/2025/2025-04-01.parquet". The same keys map to GitHub release
# assets (slashes flattened to "__") so Actions can sync the store to a private repo.

store_path <- function(key) file.path(cfg()$data_dir, key)

store_write <- function(df, key) {
  path <- store_path(key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  arrow::write_parquet(df, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not move ", tmp, " into place at ", path)
  }
  invisible(path)
}

store_read <- function(key) tibble::as_tibble(arrow::read_parquet(store_path(key)))

store_exists <- function(key) file.exists(store_path(key))

store_list <- function(prefix) {
  base <- store_path(prefix)
  if (!dir.exists(base)) return(character())
  files <- list.files(base, pattern = "\\.(parquet|txt)$", recursive = TRUE)   # data + LightGBM boosters
  sort(file.path(prefix, files))
}

# One release per store directory (e.g. "raw--statcast--MLB--2026"), so no release
# comes near GitHub's per-release asset limit. Assets keep their file names.
release_tag <- function(key) gsub("/", "--", dirname(key), fixed = TRUE)

# The one place that shells out to the GitHub CLI, so tests can mock it. `token`
# overrides GH_TOKEN for one call (publishing to the app repo uses its own token).
# system2() goes through the shell, so every argument is quoted (e.g. "Data store"),
# and the pager is off: `gh release view` would otherwise wait for a keypress.
gh_run <- function(args, token = NULL) {
  env <- c("GH_PAGER=cat", "PAGER=cat", if (!is.null(token)) paste0("GH_TOKEN=", shQuote(token)))
  system2("gh", shQuote(args), env = env)
}

# Text output of a gh call (mockable like gh_run).
gh_output <- function(args) suppressWarnings(system2("gh", shQuote(args), stdout = TRUE, stderr = FALSE,
                                                    env = c("GH_PAGER=cat", "PAGER=cat")))

ensure_release <- function(repo, tag) {
  if (identical(as.integer(gh_run(c("release", "view", tag, "--repo", repo))), 0L)) return(invisible(tag))
  status <- gh_run(c("release", "create", tag, "--repo", repo, "--title", tag, "--notes", "Data store"))
  if (!identical(as.integer(status), 0L)) stop("gh release create failed for ", tag)
  invisible(tag)
}

release_upload <- function(key, repo, tag = release_tag(key)) {
  ensure_release(repo, tag)
  status <- gh_run(c("release", "upload", tag, store_path(key), "--repo", repo, "--clobber"))
  if (!identical(as.integer(status), 0L)) stop("gh release upload failed for ", key)
  invisible(key)
}

# Restores every asset of the release that holds `prefix` (a store directory).
release_download_dir <- function(prefix, repo) {
  dest <- store_path(prefix)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  tag <- gsub("/", "--", prefix, fixed = TRUE)
  status <- gh_run(c("release", "download", tag, "--repo", repo, "--dir", dest, "--clobber"))
  if (!identical(as.integer(status), 0L)) stop("gh release download failed for ", tag)
  invisible(store_list(prefix))
}

# Restores one file from its directory's release.
release_download_file <- function(key, repo) {
  dest <- dirname(store_path(key))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  status <- gh_run(c("release", "download", release_tag(key), "--repo", repo,
                     "--pattern", basename(key), "--dir", dest, "--clobber"))
  if (!identical(as.integer(status), 0L)) stop("gh release download failed for ", key)
  invisible(key)
}

# Uploads every file under a store directory, one gh call per release per `batch` files,
# skipping files the release already holds at the same size (unless `force`). GitHub
# allows 5,000 API calls an hour, so one call per file does not scale to a full store.
# GitHub also caps uploads per hour separately from API calls, so each batch is followed
# by a pause of `seconds_per_file` per file sent.
upload_pause <- function(seconds) Sys.sleep(seconds)

release_upload_dir <- function(prefix, repo, batch = 50, force = FALSE, seconds_per_file = 1) {
  keys <- store_list(prefix)
  for (tag in unique(release_tag(keys))) {
    k <- keys[release_tag(keys) == tag]
    ensure_release(repo, tag)
    if (!force) {
      have <- gh_output(c("release", "view", tag, "--repo", repo, "--json", "assets",
                          "--jq", ".assets[] | \"\\(.name)\\t\\(.size)\""))
      have <- strsplit(have[nzchar(have)], "\t", fixed = TRUE)
      sizes <- stats::setNames(as.numeric(vapply(have, `[`, "", 2)), vapply(have, `[`, "", 1))
      k <- k[!(basename(k) %in% names(sizes) & sizes[basename(k)] == file.size(store_path(k)))]
    }
    for (chunk in split(k, ceiling(seq_along(k) / batch))) {
      status <- gh_run(c("release", "upload", tag, store_path(chunk), "--repo", repo, "--clobber"))
      if (!identical(as.integer(status), 0L)) stop("gh release upload failed for ", tag)
      upload_pause(length(chunk) * seconds_per_file)
    }
    if (length(k)) message(tag, ": uploaded ", length(k), " file(s)")
  }
  invisible(keys)
}

# ==== http ================================================================================

# Polite HTTP GET shared by every fetcher: generic User-Agent, retries with
# exponential backoff on 429 / 5xx / timeouts, and at least 1 second between calls.

.pv_http <- new.env(parent = emptyenv())
.pv_http$last_call <- 0

http_get <- function(url, query = list(), tries = 4, min_gap = 1, timeout = 120) {
  wait <- min_gap - (as.numeric(Sys.time()) - .pv_http$last_call)
  if (wait > 0) Sys.sleep(wait)
  on.exit(.pv_http$last_call <- as.numeric(Sys.time()), add = TRUE)

  req <- httr2::request(url) %>%
    httr2::req_url_query(!!!query) %>%
    httr2::req_user_agent(cfg()$user_agent) %>%
    httr2::req_timeout(timeout) %>%
    httr2::req_retry(
      max_tries = tries,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      retry_on_failure = TRUE,
      backoff = function(attempt) 2^attempt
    )
  httr2::req_perform(req)
}

# ==== savant ==============================================================================

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

# ==== statsapi ============================================================================

# MLB Stats API: schedules (with venue and game weather), venues, season pitching
# stats, and transactions. Each fetcher is a thin wrapper around a pure parse_*()
# function so the parsers can be tested on synthetic JSON.

STATSAPI <- "https://statsapi.mlb.com/api/v1"

statsapi_json <- function(path, query = list()) {
  resp <- http_get(paste0(STATSAPI, path), query)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

`%|||%` <- function(x, y) if (is.null(x)) y else x

# ── Schedules ─────────────────────────────────────────────────────────────────
# Finished games only: codedGameState "F" (final, including completed early) or
# "O" (game over). Postponed ("D") and suspended games are dropped; a suspended game
# reappears as final on the date it is resumed, and the later entry is kept.
FINISHED_STATES <- c("F", "O")

parse_schedule <- function(json, level) {
  games <- unlist(lapply(json$dates, function(d) d$games), recursive = FALSE)
  if (length(games) == 0) {
    return(tibble::tibble(game_pk = integer(), game_date = as.Date(character()),
                          first_pitch_utc = as.POSIXct(character(), tz = "UTC"),
                          venue_id = integer(), level = character(), status = character(),
                          double_header = character(), weather_condition = character(),
                          weather_temp_f = numeric(), weather_wind = character()))
  }
  rows <- lapply(games, function(g) {
    w <- g$weather %|||% list()
    tibble::tibble(
      game_pk           = as.integer(g$gamePk),
      game_date         = as.Date(g$officialDate),
      first_pitch_utc   = as.POSIXct(g$gameDate, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      venue_id          = as.integer(g$venue$id %|||% NA),
      level             = level,
      status            = g$status$codedGameState %|||% NA_character_,
      double_header     = g$doubleHeader %|||% NA_character_,
      weather_condition = w$condition %|||% NA_character_,
      weather_temp_f    = suppressWarnings(as.numeric(w$temp %|||% NA)),
      weather_wind      = w$wind %|||% NA_character_
    )
  })
  dplyr::bind_rows(rows) %>%
    dplyr::filter(status %in% FINISHED_STATES) %>%
    dplyr::arrange(dplyr::desc(game_date)) %>%
    dplyr::distinct(game_pk, .keep_all = TRUE) %>%
    dplyr::arrange(game_date, game_pk)
}

schedule_key <- function(season, level) sprintf("raw/statsapi/schedule/%s/%s.parquet", level, season)

fetch_schedule <- function(season, level) {
  key <- schedule_key(season, level)
  if (store_exists(key)) return(store_read(key))
  json <- statsapi_json("/schedule", list(sportId = cfg()$sport_ids[[level]], season = season,
                                          gameType = "R", hydrate = "weather,venue"))
  s <- parse_schedule(json, level)
  # Only cache seasons that are over; the current season keeps changing.
  if (season < as.integer(format(Sys.Date(), "%Y")) || format(Sys.Date(), "%m") %in% c("11", "12")) {
    store_write(s, key)
  }
  s
}

game_days <- function(season, level) sort(unique(fetch_schedule(season, level)$game_date))

# ── Venues and the park table ─────────────────────────────────────────────────
parse_venues <- function(json) {
  dplyr::bind_rows(lapply(json$venues, function(v) {
    loc <- v$location %|||% list()
    co  <- loc$defaultCoordinates %|||% list()
    tibble::tibble(
      venue_id     = as.integer(v$id),
      venue_name   = v$name %|||% NA_character_,
      lat          = as.numeric(co$latitude %|||% NA),
      lon          = as.numeric(co$longitude %|||% NA),
      elevation_ft = as.numeric(loc$elevation %|||% NA),
      roof_type    = (v$fieldInfo %|||% list())$roofType %|||% NA_character_,
      azimuth_deg  = as.numeric(loc$azimuthAngle %|||% NA)
    )
  }))
}

fetch_venues <- function(ids) {
  chunks <- split(ids, ceiling(seq_along(ids) / 50))
  dplyr::bind_rows(lapply(chunks, function(ch) {
    parse_venues(statsapi_json("/venues", list(venueIds = paste(ch, collapse = ","),
                                               hydrate = "location,fieldInfo")))
  }))
}


# ── People ────────────────────────────────────────────────────────────────────
parse_people <- function(json) {
  p <- json$people %|||% list()
  tibble::tibble(
    player_id = vapply(p, function(x) as.integer(x$id), integer(1)),
    name      = vapply(p, function(x) x$fullName %|||% NA_character_, character(1)),
    throws    = vapply(p, function(x) x$pitchHand$code %|||% NA_character_, character(1)))
}

fetch_people <- function(ids, chunk = 150) {
  ids <- unique(stats::na.omit(as.integer(ids)))
  if (length(ids) == 0) return(parse_people(list()))
  groups <- split(ids, ceiling(seq_along(ids) / chunk))
  dplyr::bind_rows(lapply(groups, function(g)
    parse_people(statsapi_json("/people", list(personIds = paste(g, collapse = ","))))))
}

# ==== statsapi_stats ======================================================================

# ── Season pitching stats ─────────────────────────────────────────────────────
ip_to_outs <- function(ip) {
  ip <- as.character(ip)
  whole <- suppressWarnings(as.integer(sub("\\..*$", "", ip)))
  frac  <- suppressWarnings(as.integer(ifelse(grepl("\\.", ip), sub("^.*\\.", "", ip), "0")))
  as.integer(whole * 3L + frac)
}

parse_pitching_stats <- function(json, season, level) {
  splits <- json$stats[[1]]$splits
  dplyr::bind_rows(lapply(splits, function(sp) {
    st <- sp$stat
    num <- function(x) as.numeric(x %|||% NA)
    tibble::tibble(
      player_id = as.integer(sp$player$id),
      season    = as.integer(season),
      level     = level,
      team_id   = as.integer(sp$team$id %|||% NA),
      age       = num(st$age),
      g = num(st$gamesPlayed), gs = num(st$gamesStarted),
      ip_outs   = if (!is.null(st$outs)) as.integer(st$outs) else ip_to_outs(st$inningsPitched),
      tbf = num(st$battersFaced), so = num(st$strikeOuts), bb = num(st$baseOnBalls),
      hbp = num(st$hitByPitch), hr = num(st$homeRuns), h = num(st$hits), r = num(st$runs),
      er = num(st$earnedRuns), sv = num(st$saves), w = num(st$wins), l = num(st$losses)
    )
  }))
}

fetch_pitching_stats <- function(season, level, limit = 5000) {
  out <- list(); offset <- 0
  repeat {
    json <- statsapi_json("/stats", list(stats = "season", group = "pitching",
                                         sportId = cfg()$sport_ids[[level]], season = season,
                                         playerPool = "ALL", limit = limit, offset = offset))
    page <- parse_pitching_stats(json, season, level)
    out[[length(out) + 1]] <- page
    if (nrow(page) < limit) break
    offset <- offset + limit
  }
  dplyr::bind_rows(out)
}

# ── Transactions ──────────────────────────────────────────────────────────────
classify_il <- function(description) {
  d <- tolower(description)
  il <- grepl("injured list", d, fixed = TRUE)
  dplyr::case_when(
    il & grepl("transferred", d, fixed = TRUE) ~ "transferred",
    il & grepl("activated", d, fixed = TRUE)   ~ "activated",
    il & grepl("placed", d, fixed = TRUE)      ~ "placed",
    TRUE                                       ~ NA_character_
  )
}

empty_transactions <- function() {
  tibble::tibble(txn_id = integer(), date = as.Date(character()), player_id = integer(),
                 team_id = integer(), type_code = character(), description = character())
}

parse_transactions <- function(json) {
  if (length(json$transactions) == 0) return(dplyr::mutate(empty_transactions(), il_event = character()))
  t <- dplyr::bind_rows(lapply(json$transactions, function(x) {
    tibble::tibble(
      txn_id      = as.integer(x$id),
      date        = as.Date(x$date %|||% NA),
      player_id   = as.integer((x$person %|||% list())$id %|||% NA),
      team_id     = as.integer((x$toTeam %|||% list())$id %|||% NA),
      type_code   = x$typeCode %|||% NA_character_,
      description = x$description %|||% NA_character_
    )
  }))
  dplyr::mutate(t, il_event = classify_il(description))
}

month_chunks <- function(start_date, end_date) {
  starts <- seq(as.Date(format(start_date, "%Y-%m-01")), end_date, by = "month")
  starts[1] <- start_date
  ends <- c(seq(as.Date(format(starts[1], "%Y-%m-01")), by = "month", length.out = length(starts) + 1)[-1] - 1)
  ends[length(ends)] <- end_date
  tibble::tibble(start = starts, end = pmin(ends, end_date))
}

fetch_transactions <- function(start_date, end_date) {
  ch <- month_chunks(as.Date(start_date), as.Date(end_date))
  dplyr::bind_rows(lapply(seq_len(nrow(ch)), function(i) {
    parse_transactions(statsapi_json("/transactions", list(
      startDate = format(ch$start[i]), endDate = format(ch$end[i]))))
  }))
}

# ==== weather =============================================================================

# Historical weather at first pitch from Open-Meteo's archive API (free, no key).
# Requested in GMT and joined to games on the UTC hour containing first pitch.
# Indoor (roof-closed) handling happens later, when air density is computed (P1).

OPEN_METEO_ARCHIVE <- "https://archive-api.open-meteo.com/v1/archive"

open_meteo_request <- function(lat, lon, start_date, end_date) {
  list(url = OPEN_METEO_ARCHIVE,
       query = list(latitude = lat, longitude = lon,
                    start_date = format(as.Date(start_date)), end_date = format(as.Date(end_date)),
                    hourly = "temperature_2m,relative_humidity_2m,surface_pressure",
                    timezone = "GMT"))
}

num_or_na <- function(x) vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))

parse_open_meteo <- function(json) {
  h <- json$hourly
  tibble::tibble(
    time_utc     = as.POSIXct(unlist(h$time), format = "%Y-%m-%dT%H:%M", tz = "UTC"),
    temp_c       = num_or_na(h$temperature_2m),
    rh_pct       = num_or_na(h$relative_humidity_2m),
    pressure_hpa = num_or_na(h$surface_pressure)
  )
}

fetch_hourly_weather <- function(lat, lon, start_date, end_date) {
  r <- open_meteo_request(lat, lon, start_date, end_date)
  parse_open_meteo(httr2::resp_body_json(http_get(r$url, r$query)))
}

join_game_weather <- function(games, hourly) {
  games %>%
    dplyr::mutate(time_utc = lubridate::floor_date(first_pitch_utc, "hour")) %>%
    dplyr::left_join(hourly, by = c("venue_id", "time_utc")) %>%
    dplyr::select(-time_utc)
}

game_weather_key <- function(level, season) sprintf("raw/weather/game_weather_%s_%s.parquet", level, season)

# One request per venue per season, covering that venue's first to last game.
build_game_weather <- function(schedule, parks) {
  venues <- schedule %>%
    dplyr::group_by(venue_id) %>%
    dplyr::summarise(start = min(game_date) - 1, end = max(game_date) + 1, .groups = "drop") %>%
    dplyr::inner_join(dplyr::select(parks, venue_id, lat, lon), by = "venue_id") %>%
    dplyr::filter(!is.na(lat), !is.na(lon))
  hourly <- dplyr::bind_rows(lapply(seq_len(nrow(venues)), function(i) {
    v <- venues[i, ]
    fetch_hourly_weather(v$lat, v$lon, v$start, v$end) %>% dplyr::mutate(venue_id = v$venue_id)
  }))
  join_game_weather(dplyr::select(schedule, game_pk, game_date, venue_id, first_pitch_utc), hourly)
}

# The season's game weather: the backfilled season file plus every nightly daily file
# of that season; one row per game, daily rows win (they are newer).
assemble_season_weather <- function(level, season) {
  base <- if (store_exists(game_weather_key(level, season))) store_read(game_weather_key(level, season)) else NULL
  days <- store_list(sprintf("raw/weather/daily/%s", level))
  days <- days[startsWith(basename(days), paste0(season, "-"))]
  daily <- dplyr::bind_rows(lapply(days, store_read))
  dplyr::bind_rows(daily, base) %>%
    dplyr::distinct(game_pk, .keep_all = TRUE) %>%
    dplyr::arrange(game_pk)
}

# ==== fangraphs ===========================================================================

# FanGraphs benchmarks (README 9): ERA, FIP, xFIP, SIERA, K-BB%, and full-season
# Stuff+ / Location+ / Pitching+, from the free public leaderboard JSON (no
# membership). Rows are kept in the private data store only; the public repo
# publishes only the resulting correlations and errors.

FANGRAPHS_URL <- "https://www.fangraphs.com/api/leaders/major-league/data"

fangraphs_query <- function(season, type = 8) {
  list(age = "", pos = "all", stats = "pit", lg = "all", qual = 0, season = season, season1 = season,
       month = 0, ind = 0, team = 0, pageitems = 2000000, pagenum = 1, type = type)
}

fangraphs_request <- function(season, type = 8) {
  httr2::request(FANGRAPHS_URL) %>%
    httr2::req_url_query(!!!fangraphs_query(season, type)) %>%
    httr2::req_user_agent(cfg()$user_agent)
}

parse_fangraphs <- function(json) {
  num <- function(r, k) if (is.null(r[[k]])) NA_real_ else as.numeric(r[[k]])
  rows <- lapply(json$data, function(r) tibble::tibble(
    mlbam_id = as.integer(num(r, "xMLBAMID")),
    season = as.integer(num(r, "Season")),
    ip_outs = ip_to_outs(sprintf("%.1f", num(r, "IP"))),
    tbf = num(r, "TBF"), era = num(r, "ERA"), fip = num(r, "FIP"), xfip = num(r, "xFIP"),
    siera = num(r, "SIERA"), k_minus_bb = num(r, "K-BB%"),
    stuff_plus = num(r, "sp_stuff"), location_plus = num(r, "sp_location"),
    pitching_plus = num(r, "sp_pitching")))
  dplyr::bind_rows(rows)
}

fangraphs_key <- function(season) sprintf("raw/fangraphs/pitching/%s.parquet", season)

fetch_fangraphs <- function(season) {
  resp <- http_get(FANGRAPHS_URL, fangraphs_query(season))
  f <- parse_fangraphs(httr2::resp_body_json(resp, simplifyVector = FALSE))
  f[!is.na(f$mlbam_id), ]
}

# ==== parks ===============================================================================

# Park reference table: one row per venue used by any MLB or AAA regular-season game
# in the backfill seasons. Venue facts come from the Stats API; hand fixes in
# reference/parks_overrides.csv are applied last.

build_parks <- function(schedules, venues, overrides = NULL) {
  used <- schedules %>%
    dplyr::filter(!is.na(venue_id)) %>%
    dplyr::group_by(venue_id) %>%
    dplyr::summarise(
      level = if (dplyr::n_distinct(level) > 1) "both" else dplyr::first(level),
      first_season = as.integer(min(format(game_date, "%Y"))),
      last_season  = as.integer(max(format(game_date, "%Y"))),
      .groups = "drop"
    )
  parks <- dplyr::left_join(used, venues, by = "venue_id")

  if (!is.null(overrides) && nrow(overrides) > 0) {
    for (col in setdiff(names(overrides), "venue_id")) {
      idx <- match(parks$venue_id, overrides$venue_id)
      has <- !is.na(idx) & !is.na(overrides[[col]][idx])
      parks[[col]][has] <- overrides[[col]][idx[has]]
    }
  }
  dplyr::arrange(parks, venue_id)
}

read_park_overrides <- function() {
  p <- file.path(repo_root(), "reference", "parks_overrides.csv")
  if (!file.exists(p)) return(NULL)
  readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
}

# ── Elevation fill ────────────────────────────────────────────────────────────
# The Stats API has no elevation for many AAA parks. Open-Meteo's free elevation
# API (90 m terrain model) fills the gaps; it agrees with the Stats API on Coors Field.
open_meteo_elevation_request <- function(lat, lon) {
  list(url = "https://api.open-meteo.com/v1/elevation",
       query = list(latitude = paste(lat, collapse = ","), longitude = paste(lon, collapse = ",")))
}

open_meteo_elevation <- function(lat, lon) {
  r <- open_meteo_elevation_request(lat, lon)
  unlist(httr2::resp_body_json(http_get(r$url, r$query))$elevation)
}

fill_missing_elevation <- function(parks, lookup = open_meteo_elevation) {
  default_src <- ifelse(is.na(parks$elevation_ft), NA_character_, "statsapi")
  parks$elevation_source <- if ("elevation_source" %in% names(parks)) dplyr::coalesce(parks$elevation_source, default_src) else default_src
  need <- which(is.na(parks$elevation_ft) & !is.na(parks$lat) & !is.na(parks$lon))
  if (length(need) > 0) {
    meters <- lookup(parks$lat[need], parks$lon[need])
    parks$elevation_ft[need] <- meters * 3.28084
    parks$elevation_source[need] <- "open-meteo"
  }
  parks
}

# ==== backfill ============================================================================

# Idempotent day-by-day Statcast backfill and per-season compaction.
# A day file exists only if it was fetched, checked against the schedule, and fully
# written, so a rerun resumes exactly where an interrupted run stopped.
#
# Day statuses:
#   written          every finished game in the schedule is present
#   written_partial  an old day (before the strict window) is missing a game even after
#                    a retry; written anyway, because Savant will not fill it later
#   skipped          the day file already exists
#   empty_error / fetch_error / incomplete_error
#                    nothing written; the run's caller exits non-zero (backfill_failed)

statcast_day_key <- function(level, date) {
  date <- as.Date(date)
  sprintf("raw/statcast/%s/%s/%s.parquet", level, format(date, "%Y"), format(date, "%Y-%m-%d"))
}

statcast_season_key <- function(level, season) sprintf("raw/statcast/%s/%s.parquet", level, season)

# Finished games on a date, from the schedule.
expected_game_pks <- function(date, level) {
  s <- fetch_schedule(as.integer(format(as.Date(date), "%Y")), level)
  s$game_pk[s$game_date == as.Date(date)]
}

backfill_failed <- function(log) any(grepl("_error$", log$status))

backfill_statcast <- function(level, seasons, fetch = fetch_statcast_day, days = game_days,
                              expected = expected_game_pks, today = Sys.Date(),
                              strict_days = 7, retry_wait = 60,
                              log_dir = file.path(cfg()$data_dir, "logs")) {
  all_days <- do.call(c, lapply(seasons, function(s) days(s, level)))
  rows <- vector("list", length(all_days))

  attempt <- function(date) {
    d <- tryCatch(fetch(date, level), error = function(e) e)
    if (inherits(d, "error")) return(list(d = NULL, status = "fetch_error", note = conditionMessage(d)))
    if (nrow(d) == 0) return(list(d = d, status = "empty_error", note = ""))
    if (!is.null(expected)) {
      missing <- setdiff(expected(date, level), d$game_pk)
      if (length(missing) > 0) {
        return(list(d = d, status = "incomplete_error",
                    note = paste("missing game_pk", paste(missing, collapse = ","))))
      }
    }
    list(d = d, status = "written", note = "")
  }

  for (i in seq_along(all_days)) {
    date <- all_days[i]
    key <- statcast_day_key(level, date)
    if (store_exists(key)) {
      rows[[i]] <- tibble::tibble(date = date, rows = NA_integer_, status = "skipped")
      next
    }
    res <- attempt(date)
    if (res$status != "written") {
      # Retry once: Savant sometimes returns a busy page or posts games late.
      if (retry_wait > 0) Sys.sleep(retry_wait)
      res <- attempt(date)
    }
    if (res$status == "incomplete_error" && as.Date(date) < as.Date(today) - strict_days) {
      res$status <- "written_partial"
    }
    if (res$status %in% c("written", "written_partial")) store_write(res$d, key)
    rows[[i]] <- tibble::tibble(date = date,
                                rows = if (is.null(res$d)) NA_integer_ else nrow(res$d),
                                status = res$status)
    if (res$status != "written") message(level, " ", date, ": ", res$status, " ", res$note)
  }

  log <- dplyr::bind_rows(rows)
  if (is.null(log) || nrow(log) == 0) {
    log <- tibble::tibble(date = as.Date(character()), rows = integer(), status = character())
  }
  if (!is.null(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(log, file.path(log_dir, sprintf("backfill_%s_%s.csv", level,
                                                     format(Sys.time(), "%Y%m%d_%H%M%S"))))
  }
  log
}

# Streams every day file of a season through duckdb into one parquet file, keeping
# the latest game_date for each pitch_uid (resumed games). Never loads the season
# into R, so it is safe on an 8 GB machine.
compact_season <- function(level, season) {
  dir <- store_path(sprintf("raw/statcast/%s/%s", level, season))
  key <- statcast_season_key(level, season)
  out <- store_path(key)
  tmp <- paste0(out, ".tmp-", Sys.getpid())
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '2GB'")
  sql <- sprintf(
    "COPY (
       SELECT * EXCLUDE (rn) FROM (
         SELECT *, row_number() OVER (PARTITION BY pitch_uid ORDER BY game_date DESC) AS rn
         FROM read_parquet('%s/*.parquet', union_by_name = true)
       ) WHERE rn = 1 ORDER BY game_date, pitch_uid
     ) TO '%s' (FORMAT parquet)",
    gsub("'", "''", dir), gsub("'", "''", tmp))
  DBI::dbExecute(con, sql)
  if (!file.rename(tmp, out)) stop("Could not move compacted file into place: ", out)
  key
}

# ==== ingest ==============================================================================

# Helpers for the nightly ingest (run.R ingest_daily).

# Re-check the last `window` days, not just yesterday, so a day Savant posted late
# (or a failed night) is picked up on a later run. Existing day files are skipped.
ingest_dates <- function(today, window = 5) seq(as.Date(today) - window, as.Date(today) - 1, by = "day")

# Open-Meteo's archive is built from reanalysis data and runs several days behind,
# so weather is ingested for a day `lag` days back rather than yesterday.
weather_date <- function(today, lag = 7) as.Date(today) - lag

weather_usable <- function(w, min_share = 0.9) nrow(w) > 0 && mean(!is.na(w$temp_c)) >= min_share

# Stats API files: a past season never changes once cached, but the current season
# (and the current year's transactions) must be refetched every time.
needs_fetch <- function(key, season, current) !store_exists(key) || season >= current

# ==== validate ============================================================================

# Data-quality checks on a backfilled Statcast season. Each check returns
# tibble(check, value, pass); validate_statcast_season() stacks them.

check_unique_uid <- function(d) {
  dup <- sum(duplicated(d$pitch_uid))
  tibble::tibble(check = "unique_pitch_uid", value = dup, pass = dup == 0)
}

check_game_coverage <- function(statcast, schedule, bar = 0.99) {
  share <- mean(schedule$game_pk %in% statcast$game_pk)
  tibble::tibble(check = "share_of_final_games_with_pitches", value = share, pass = share >= bar)
}

check_non_missing <- function(d, cols, bar = 0.97) {
  share <- vapply(cols, function(cl) mean(!is.na(d[[cl]])), numeric(1))
  tibble::tibble(check = paste0("non_missing_", cols), value = unname(share), pass = unname(share >= bar))
}

row_bar <- function(level, season) {
  if (level == "MLB" && season == 2020) return(200000)
  if (level == "MLB") return(650000)
  300000
}

KEY_PHYSICAL_COLS <- c("release_speed", "pfx_x", "pfx_z", "plate_x", "plate_z", "release_extension")

validate_statcast_season <- function(level, season) {
  d <- store_read(statcast_season_key(level, season))
  sched <- fetch_schedule(season, level)
  arm <- mean(!is.na(d$arm_angle))
  dplyr::bind_rows(
    tibble::tibble(check = "rows", value = nrow(d), pass = nrow(d) >= row_bar(level, season)),
    check_unique_uid(d),
    check_game_coverage(d, sched),
    check_non_missing(d, KEY_PHYSICAL_COLS),
    tibble::tibble(check = "non_missing_arm_angle (reported only)", value = arm, pass = NA)
  ) %>%
    dplyr::mutate(level = level, season = season, .before = 1)
}
