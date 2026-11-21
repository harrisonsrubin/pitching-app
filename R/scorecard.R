# Scorecard: run accounting, park run factors, Marcel, and the P1 and P3 scorecard helpers.

# ==== runs ================================================================================

# Run accounting for the scorecard (README 9): outs and runs per plate appearance
# from Statcast, park run factors, park-adjusted RA9 from official Stats API runs,
# and a Marcel-style RA9 benchmark.

OUTS_BY_EVENT <- c(strikeout = 1L, field_out = 1L, force_out = 1L, sac_fly = 1L, sac_bunt = 1L,
                   fielders_choice_out = 1L, other_out = 1L, double_play = 2L,
                   grounded_into_double_play = 2L, strikeout_double_play = 2L,
                   sac_fly_double_play = 2L, sac_bunt_double_play = 2L, triple_play = 3L)

# One row per plate appearance. Runs = runs scored while the PA was in progress
# (runs on the pitcher's watch, not charged runs). Outs = change in outs before the
# next PA of the half-inning; the last PA of a half-inning gets 3 - outs, except in
# the game's final half-inning (walk-offs), where the event decides.
pa_table <- function(pitches) {
  p <- pitches %>% dplyr::arrange(game_pk, at_bat_number, pitch_number)
  pa <- p %>%
    dplyr::group_by(game_pk, at_bat_number) %>%
    dplyr::summarise(pitcher = dplyr::last(pitcher), season = dplyr::first(season),
                     game_date = dplyr::first(game_date), inning = dplyr::first(inning),
                     inning_topbot = dplyr::first(inning_topbot),
                     outs_before = dplyr::first(outs_when_up),
                     runs = as.integer(sum(post_bat_score - bat_score, na.rm = TRUE)),
                     events = dplyr::last(events),
                     bb_type = if ("bb_type" %in% names(p)) dplyr::last(bb_type) else NA_character_,
                     .groups = "drop") %>%
    dplyr::arrange(game_pk, at_bat_number)
  half_order <- pa$inning * 2 + (pa$inning_topbot == "Bot")
  pa %>%
    dplyr::mutate(half = half_order) %>%
    dplyr::group_by(game_pk) %>%
    dplyr::mutate(final_half = half == max(half)) %>%
    dplyr::group_by(game_pk, half) %>%
    dplyr::mutate(next_outs = dplyr::lead(outs_before),
                  event_outs = dplyr::coalesce(unname(OUTS_BY_EVENT[events]), 0L),
                  outs = as.integer(dplyr::case_when(
                    !is.na(next_outs) ~ next_outs - outs_before,
                    final_half ~ event_outs,
                    TRUE ~ 3L - outs_before))) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_pk, at_bat_number, pitcher, season, game_date, inning, inning_topbot, outs, runs, events, bb_type)
}

game_scores <- function(pitches) {
  pitches %>%
    dplyr::group_by(game_pk) %>%
    dplyr::summarise(season = dplyr::first(season), venue_id = dplyr::first(venue_id),
                     home_team = dplyr::first(home_team), away_team = dplyr::first(away_team),
                     home_runs = max(as.numeric(post_home_score), na.rm = TRUE),
                     away_runs = max(as.numeric(post_away_score), na.rm = TRUE), .groups = "drop")
}

# Runs per game at the venue ÷ runs per game in its home team's road games, pooled
# over the season and the two before it, regressed halfway to 1.
park_run_factors <- function(scores) {
  scores$total <- scores$home_runs + scores$away_runs
  vs <- dplyr::distinct(scores, venue_id, season)
  out <- lapply(seq_len(nrow(vs)), function(i) {
    v <- vs$venue_id[i]; s <- vs$season[i]
    w <- scores[scores$season %in% (s - 2):s, ]
    at_v <- w[w$venue_id == v, ]
    team <- names(sort(table(at_v$home_team), decreasing = TRUE))[1]
    road <- w[w$away_team == team, ]
    raw <- mean(at_v$total) / mean(road$total)
    tibble::tibble(venue_id = v, season = s, pf = 1 + 0.5 * (raw - 1))
  })
  dplyr::bind_rows(out)
}

pitcher_park_factor <- function(pa, scores, pf) {
  pa %>%
    dplyr::left_join(dplyr::select(scores, game_pk, venue_id), by = "game_pk") %>%
    dplyr::left_join(pf, by = c("venue_id", "season")) %>%
    dplyr::group_by(pitcher, season) %>%
    dplyr::summarise(ppf = mean(dplyr::coalesce(pf, 1)), .groups = "drop")
}

ra9_official <- function(stats, ppf) {
  stats %>%
    dplyr::filter(level == "MLB") %>%
    dplyr::group_by(pitcher = player_id, season) %>%
    dplyr::summarise(r = sum(r, na.rm = TRUE), outs = sum(ip_outs, na.rm = TRUE),
                     age = dplyr::first(age), .groups = "drop") %>%
    dplyr::filter(outs > 0) %>%
    dplyr::left_join(ppf, by = c("pitcher", "season")) %>%
    dplyr::mutate(ip = outs / 3, ra9 = 27 * r / outs, ra9_adj = ra9 / dplyr::coalesce(ppf, 1)) %>%
    dplyr::select(pitcher, season, ip, ra9, ra9_adj, age)
}

# Marcel-style RA9 for `season`: prior three seasons weighted 3/2/1 by innings, plus
# 100 IP of league RA9, times an age factor for the age in `season`.
marcel_ra9 <- function(hist, season, lg_ra9) {
  proj <- season
  h <- hist[hist$season %in% (proj - 3):(proj - 1) & !is.na(hist$ra9), ]
  h$wt <- c(3, 2, 1)[proj - h$season]
  h %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(num = sum(wt * ip * ra9) + 100 * lg_ra9, den = sum(wt * ip) + 100,
                     age = age[which.max(season)] + (proj - max(season)), .groups = "drop") %>%
    dplyr::mutate(raw = num / den,
                  factor = ifelse(age < 29, 1 - 0.006 * (29 - age), 1 + 0.003 * (age - 29)),
                  marcel = raw * dplyr::coalesce(factor, 1)) %>%
    dplyr::select(pitcher, marcel)
}

# Run accounting reads raw Statcast, not the model-filtered pitches: a plate
# appearance made only of automatic balls/strikes or untyped pitches still records
# outs and runs (~480 such PAs per MLB season).
RUN_COLS <- c("game_pk", "pitcher", "season", "game_date", "inning", "inning_topbot", "at_bat_number",
              "pitch_number", "outs_when_up", "bat_score", "post_bat_score", "events", "home_team",
              "away_team", "post_home_score", "post_away_score", "balls", "strikes", "description", "bb_type")

load_run_pitches <- function(level, season) {
  p <- arrow::open_dataset(store_path(statcast_season_key(level, season))) %>%
    dplyr::select(dplyr::all_of(RUN_COLS)) %>% dplyr::collect()
  sched <- store_read(sprintf("raw/statsapi/schedule/%s/%s.parquet", level, season))
  dplyr::left_join(p, dplyr::select(sched, game_pk, venue_id), by = "game_pk")
}

# ==== scorecard ===========================================================================

# Scorecard (README 9): is a metric a skill (stickiness), does it predict runs
# (rest-of-season and next-season park-adjusted RA9), and does it add anything to what
# we already knew (incremental value)? Rules: same pitchers and thresholds in both
# seasons, weight = the smaller of the two seasons' innings, bands by innings,
# Pearson and Spearman, pitchers who disappear counted separately.

wcor <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[ok]; y <- y[ok]; w <- w[ok] / sum(w[ok])
  mx <- sum(w * x); my <- sum(w * y)
  sum(w * (x - mx) * (y - my)) / sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
}

wspearman <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  wcor(rank(x[ok]), rank(y[ok]), w[ok])
}

ip_band <- function(ip) {
  dplyr::case_when(ip >= 150 ~ "150+", ip >= 100 ~ "100-150", ip >= 50 ~ "50-100", TRUE ~ NA_character_)
}

# m: pitcher, season, value. ip: pitcher, season, ip.
pair_seasons <- function(m, ip, min_ip = 50) {
  a <- dplyr::inner_join(m, ip, by = c("pitcher", "season"))
  nxt <- dplyr::mutate(a, season = season - 1L)
  dplyr::inner_join(a, nxt, by = c("pitcher", "season"), suffix = c("_y", "_y1")) %>%
    dplyr::filter(ip_y >= min_ip, ip_y1 >= min_ip, !is.na(value_y), !is.na(value_y1)) %>%
    dplyr::transmute(pitcher, season, x = value_y, y = value_y1, w = pmin(ip_y, ip_y1), band = ip_band(w))
}

# Qualified in season y but under the threshold (or absent) in y + 1, for seasons
# where y + 1 exists in the data.
disappeared_count <- function(m, ip, min_ip = 50) {
  a <- dplyr::inner_join(m, ip, by = c("pitcher", "season"))
  q <- a[a$ip >= min_ip & a$season < max(ip$season), ]
  nxt <- ip[ip$ip >= min_ip, ] %>% dplyr::mutate(season = season - 1L)
  tibble::tibble(n = sum(!paste(q$pitcher, q$season) %in% paste(nxt$pitcher, nxt$season)))
}

# Weighted linear fit on the other folds, predicted on the held-out fold.
cv_rmse <- function(df, formula, w, folds = fold_of(df$pitcher)) {
  force(folds)   # evaluate on the unfiltered rows, before df is subset below
  df$.w <- w
  vars <- all.vars(formula)
  ok <- stats::complete.cases(df[vars]) & !is.na(df$.w)
  df <- df[ok, ]; folds <- folds[ok]
  pred <- numeric(nrow(df))
  for (k in unique(folds)) {
    tr <- folds != k
    fit <- stats::lm(formula, data = df[tr, ], weights = .w)
    pred[!tr] <- stats::predict(fit, newdata = df[!tr, ])
  }
  y <- df[[vars[1]]]
  list(rmse = sqrt(stats::weighted.mean((y - pred)^2, df$.w)),
       pearson = wcor(pred, y, df$.w), spearman = wspearman(pred, y, df$.w), n = nrow(df))
}

# ---- rest-of-season windows ---------------------------------------------------------

order_key <- function(game_date, game_pk, at_bat_number) {
  as.numeric(game_date) * 1e9 + (as.numeric(game_pk) %% 1e6) * 1e3 + as.numeric(at_bat_number)
}

# Per pitcher-season: the PA key of his N-th pitch (raw Statcast count). Pitchers with
# fewer than N pitches get no cutoff.
window_cutoffs <- function(run_pitches, n_pitches) {
  run_pitches %>%
    dplyr::mutate(key = order_key(game_date, game_pk, at_bat_number)) %>%
    dplyr::arrange(pitcher, key, pitch_number) %>%
    dplyr::group_by(pitcher) %>%
    dplyr::filter(dplyr::n() >= n_pitches) %>%
    dplyr::summarise(cutoff = key[n_pitches], .groups = "drop")
}

FIP_EVENTS <- list(hr = "home_run", bb = "walk", hbp = "hit_by_pitch",
                   k = c("strikeout", "strikeout_double_play"))

# Runs/outs/FIP pieces from a PA table subset, per pitcher.
pa_summary <- function(pa) {
  pa %>% dplyr::group_by(pitcher) %>%
    dplyr::summarise(outs = sum(outs), runs = sum(runs), n_pa = dplyr::n(),
                     hr = sum(events %in% FIP_EVENTS$hr), bb = sum(events %in% FIP_EVENTS$bb),
                     hbp = sum(events %in% FIP_EVENTS$hbp), k = sum(events %in% FIP_EVENTS$k),
                     .groups = "drop") %>%
    dplyr::mutate(ip = outs / 3, ra9 = 27 * runs / pmax(outs, 1),
                  fip_raw = (13 * hr + 3 * (bb + hbp) - 2 * k) / pmax(ip, 1 / 3),
                  k_minus_bb = (k - bb) / n_pa)
}

fip_constant <- function(pa_season) {
  s <- pa_summary(dplyr::mutate(pa_season, pitcher = 0L))
  s$ra9 - s$fip_raw
}

# ==== scorecard_p3 ========================================================================

# P3 scorecard helpers (README 9): range coverage, CRPS from simulation draws, and
# calibration of the Step forward / Step back odds.

coverage <- function(lo, hi, actual) {
  ok <- !is.na(lo) & !is.na(hi) & !is.na(actual)
  if (!any(ok)) return(NA_real_)
  mean(actual[ok] >= lo[ok] & actual[ok] <= hi[ok])
}

# Sample CRPS: E|X - y| - E|X - X'| / 2 over the draws X.
crps_draws <- function(draws, actual) {
  draws <- draws[!is.na(draws)]
  mean(abs(draws - actual)) - 0.5 * mean(abs(outer(draws, draws, "-")))
}

step_calibration <- function(pred, realized, breaks = c(0, 0.05, 0.1, 0.2, 0.3, 1)) {
  bin <- cut(pred, breaks, include.lowest = TRUE)
  tibble::tibble(bin = bin, pred = pred, real = as.numeric(realized)) %>%
    dplyr::filter(!is.na(bin), !is.na(real)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(predicted = mean(pred), observed = mean(real), n = dplyr::n(), .groups = "drop")
}
