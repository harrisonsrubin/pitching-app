# P3a pipeline aggregates (design: builder-check rulings). Per pitcher and level-season:
# component counts and grades after 100/250/500 pitches and season to date (so
# rest-of-season projections train on the same input they get), and park factors for
# runs, home runs and hits on balls in play. Both are aggregates, safe to publish.

WINDOW_NS <- c(100L, 250L, 500L)
BB_EVENTS <- c("walk", "intent_walk")
HIT_BIP_EVENTS <- c("single", "double", "triple")

windows_key <- function(level, season) outputs_key(sprintf("windows_%s_%s.parquet", level, season))
parks_key <- function(level, season) outputs_key(sprintf("parks_%s_%s.parquet", level, season))
park_sums_key <- function(level, season) sprintf("processed/park_sums/%s/%s.parquet", level, season)

# `pa` from pa_table() plus `key` (order_key); `cutoffs` (pitcher, cutoff) keeps PAs at
# or before each pitcher's cutoff. Only completed PAs (an event) count as batters faced.
window_counts <- function(pa, cutoffs = NULL) {
  if (!is.null(cutoffs)) {
    pa <- dplyr::inner_join(pa, cutoffs, by = "pitcher")
    pa <- pa[pa$key <= pa$cutoff, ]
  }
  if (!"bb_type" %in% names(pa)) pa$bb_type <- NA_character_
  pa %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(tbf = sum(!is.na(events)), k = sum(events %in% FIP_EVENTS$k),
                     bbhbp = sum(events %in% c(BB_EVENTS, FIP_EVENTS$hbp)), hr = sum(events %in% FIP_EVENTS$hr),
                     hbip = sum(events %in% HIT_BIP_EVENTS), fb = sum(bb_type %in% "fly_ball" & !is.na(events)),
                     outs = sum(outs), runs = sum(runs), .groups = "drop") %>%
    dplyr::mutate(bip = pmax(tbf - k - bbhbp - hr, 0L)) %>%
    dplyr::select(pitcher, tbf, k, bbhbp, hr, bip, hbip, fb, outs, runs)
}

# Window counts and window grades (re-fit on the pitches before each cutoff, as the P1
# scorecard does) for 100/250/500 pitches and the season to date.
window_stats <- function(run_pitches, pa, x, anchors, ns = WINDOW_NS) {
  pa$key <- order_key(pa$game_date, pa$game_pk, pa$at_bat_number)
  ab <- as.integer(vapply(strsplit(x$pitch_uid, "-", fixed = TRUE), `[`, "", 2))
  x$key <- order_key(x$game_date, x$game_pk, ab)
  grade_of <- function(xs) {
    est <- lapply(c("stuff", "location", "pitching"), function(f) {
      g <- tryCatch(fit_grades(xs, paste0(f, "_xrv"))$by_pitcher, error = function(e) NULL)
      if (is.null(g)) return(tibble::tibble(pitcher = integer()))
      a <- anchors[anchors$facet == f & anchors$scope == "pitcher", ]
      tibble::tibble(pitcher = g$pitcher, !!f := to_grade(g$est, a$mean, a$unit))
    })
    Reduce(function(a, b) dplyr::full_join(a, b, by = "pitcher"), est)
  }
  out <- lapply(ns, function(n) {
    cut <- window_cutoffs(run_pitches, n)
    if (nrow(cut) == 0) return(NULL)
    xc <- dplyr::inner_join(x, cut, by = "pitcher"); xc <- xc[xc$key <= xc$cutoff, ]
    dplyr::left_join(window_counts(pa, cut), grade_of(xc), by = "pitcher") %>%
      dplyr::mutate(window = as.character(n))
  })
  full <- dplyr::left_join(window_counts(pa), grade_of(x), by = "pitcher") %>% dplyr::mutate(window = "to_date")
  dplyr::bind_rows(c(out, list(full))) %>% dplyr::relocate(pitcher, window)
}

# Per-season sums for park factors: each team's home totals at its venue and its road
# totals. Stored per season so a 3-season window never has to reload old pitch files.
park_sums <- function(pa, scores) {
  p <- pa %>% dplyr::left_join(dplyr::select(scores, game_pk, venue_id, home_team, away_team), by = "game_pk")
  per_game <- p %>%
    dplyr::group_by(game_pk, season, venue_id, home_team, away_team) %>%
    dplyr::summarise(pa = sum(!is.na(events)), hr = sum(events %in% FIP_EVENTS$hr),
                     bip = sum(!is.na(events) & !events %in% c(FIP_EVENTS$k, BB_EVENTS, FIP_EVENTS$hbp, FIP_EVENTS$hr)),
                     hbip = sum(events %in% HIT_BIP_EVENTS), runs = sum(runs), .groups = "drop")
  home <- per_game %>% dplyr::group_by(season, venue_id, team = home_team) %>%
    dplyr::summarise(dplyr::across(c(pa, hr, bip, hbip, runs), sum), games = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(side = "home")
  # A team's road games: every game it played away (both teams' events, like the home side).
  road <- per_game %>% dplyr::group_by(season, team = away_team) %>%
    dplyr::summarise(dplyr::across(c(pa, hr, bip, hbip, runs), sum), games = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(side = "road", venue_id = NA_integer_)
  dplyr::bind_rows(home, road) %>% dplyr::select(season, venue_id, team, side, pa, hr, bip, hbip, runs, games)
}

# Park factors per venue-season from the stored sums of seasons s-2..s (whatever is
# available): home rate over the home team's road rate, regressed halfway to 1.
component_park_factors <- function(sums) {
  home <- sums[sums$side == "home", ]
  vs <- dplyr::distinct(home, venue_id, season)
  out <- lapply(seq_len(nrow(vs)), function(i) {
    v <- vs$venue_id[i]; s <- vs$season[i]
    h <- home[home$venue_id == v & home$season %in% (s - 2):s, ]
    team <- names(sort(tapply(h$games, h$team, sum), decreasing = TRUE))[1]
    h <- h[h$team == team, ]
    r <- sums[sums$side == "road" & sums$team == team & sums$season %in% (s - 2):s, ]
    ratio <- function(num, den) {
      x <- (sum(h[[num]]) / sum(h[[den]])) / (sum(r[[num]]) / sum(r[[den]]))
      if (!is.finite(x)) 1 else 1 + 0.5 * (x - 1)
    }
    tibble::tibble(venue_id = v, season = s, pf_runs = ratio("runs", "games"),
                   pf_hr = ratio("hr", "pa"), pf_hbip = ratio("hbip", "bip"))
  })
  dplyr::bind_rows(out)
}

# Each pitcher's park factors: plate-appearance-weighted over the parks he pitched in;
# a park without a factor counts as neutral (1).
pitcher_parks <- function(pa, scores, cpf) {
  pa %>%
    dplyr::filter(!is.na(events)) %>%
    dplyr::left_join(dplyr::select(scores, game_pk, venue_id, season), by = c("game_pk", "season")) %>%
    dplyr::left_join(cpf, by = c("venue_id", "season")) %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(ppf_runs = mean(dplyr::coalesce(pf_runs, 1)), ppf_hr = mean(dplyr::coalesce(pf_hr, 1)),
                     ppf_hbip = mean(dplyr::coalesce(pf_hbip, 1)), .groups = "drop")
}

# Windows and park files for one level-season (called from build_outputs()). Loads the
# raw run pitches once and frees them before returning (8 GB).
write_projection_aggregates <- function(level, season, x_key, anchors) {
  rp <- load_run_pitches(level, season)
  pa <- pa_table(rp)
  scores <- game_scores(rp)
  store_write(park_sums(pa, scores), park_sums_key(level, season))
  sums <- dplyr::bind_rows(lapply(intersect((season - 2):season, stored_seasons_with(park_sums_key, level)),
                                  function(s) store_read(park_sums_key(level, s))))
  cpf <- component_park_factors(sums)
  store_write(pitcher_parks(pa, scores, cpf), parks_key(level, season))
  x <- store_read(x_key)
  store_write(window_stats(rp, pa, x, anchors), windows_key(level, season))
  rm(rp, pa, x); gc()
  invisible(TRUE)
}

stored_seasons_with <- function(key_fn, level) {
  s <- 2000:2100
  s[vapply(s, function(y) store_exists(key_fn(level, y)), TRUE)]
}
