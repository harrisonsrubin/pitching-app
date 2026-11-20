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
