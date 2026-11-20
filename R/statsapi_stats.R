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
