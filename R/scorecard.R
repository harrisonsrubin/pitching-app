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
