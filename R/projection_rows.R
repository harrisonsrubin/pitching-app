# P3a training and projection rows (no leakage: every predictor comes from seasons up to
# the base season; rest-of-season rows see only the partial window of the current season).
#
# Past results are pooled over the base season and the two before it (weights 5/4/3 per
# trial) across MLB and Triple-A, each relative to its own league, as a logit padded with
# league-rate trials (PAST_PRIOR_W) so it stays finite and bounded. Home runs and hits on balls in play are
# divided by the pitcher's park factor first. Grades are capped 20-80 and scaled to
# (grade - 50) / 10; a pitcher without grades gets zeros and has_grades = 0.

RECENCY_W <- c(5, 4, 3)
# League-rate padding for past results, in weighted trials (= 20 batters at weight 5). It
# keeps rare events (a reliever with no home runs) from producing extreme logits; the
# past_rel x sample-size interaction still lets large samples dominate.
PAST_PRIOR_W <- 100
SIGNAL_GRADES <- c("stuff", "location", "pitching")
SIGNAL_X <- c("x_whiff", "x_chase", "x_cstrike", "x_woba_contact", "x_gb")
TRAIN_WINDOWS <- c("100", "250", "500")

park_col <- function(component) switch(component, hr = "ppf_hr", hbip = "ppf_hbip", NA_character_)

# Lines with `ev` (park-adjusted events), `tr` (trials) and `lg` (own-league rate).
component_lines <- function(lines, parks, league, component) {
  tcol <- component_trials(component); pcol <- park_col(component)
  l <- lines %>%
    dplyr::left_join(dplyr::select(league, season, level, lg = !!paste0("lg_", component)), by = c("season", "level"))
  if (!is.na(pcol) && !is.null(parks) && nrow(parks)) {
    l <- dplyr::left_join(l, dplyr::select(parks, pitcher, level, season, pf = !!pcol), by = c("pitcher", "level", "season"))
  } else l$pf <- NA_real_
  l %>% dplyr::mutate(ev = .data[[component]] / dplyr::coalesce(pf, 1), tr = .data[[tcol]]) %>%
    dplyr::select(pitcher, season, level, age, gs_share, tbf, ev, tr, lg)
}

# History features for (pitcher, base_season) pairs. `offsets` are the seasons back from
# the base that count (0 = the base season itself); `extra` adds partial-season counts
# (pitcher, base_season, ev, tr, lg, level) at weight 5.
history_features <- function(cl, bases, offsets = 0:2, extra = NULL) {
  h <- dplyr::bind_rows(lapply(offsets, function(o)
    cl %>% dplyr::mutate(base_season = season + o, w = RECENCY_W[o + 1]) %>%
      dplyr::semi_join(bases, by = c("pitcher", "base_season"))))
  if (!is.null(extra)) h <- dplyr::bind_rows(h, dplyr::mutate(extra, w = RECENCY_W[1]))
  past <- h %>% dplyr::group_by(pitcher, base_season) %>%
    dplyr::summarise(wev = sum(w * ev), wtr = sum(w * tr), wlg = sum(w * tr * lg),
                     past_trials = sum(tr), aaa_tr = sum(tr[level == "AAA"]), .groups = "drop") %>%
    dplyr::mutate(lgbar = ifelse(wtr > 0, wlg / wtr, NA_real_),
                  past_rel = ifelse(wtr > 0, stats::qlogis((wev + PAST_PRIOR_W * lgbar) / (wtr + PAST_PRIOR_W)) - stats::qlogis(lgbar), 0),
                  aaa_share = ifelse(past_trials > 0, aaa_tr / past_trials, 0)) %>%
    dplyr::select(pitcher, base_season, past_rel, past_trials, aaa_share)
  base_info <- cl %>% dplyr::semi_join(dplyr::rename(bases, season = base_season), by = c("pitcher", "season")) %>%
    dplyr::group_by(pitcher, base_season = season) %>%
    dplyr::summarise(age = max(age), gs_share = stats::weighted.mean(gs_share, pmax(tbf, 1)), .groups = "drop")
  bases %>% dplyr::left_join(past, by = c("pitcher", "base_season")) %>%
    dplyr::left_join(base_info, by = c("pitcher", "base_season")) %>%
    dplyr::mutate(past_rel = dplyr::coalesce(past_rel, 0), past_trials = dplyr::coalesce(past_trials, 0),
                  aaa_share = dplyr::coalesce(aaa_share, 0), gs_share = dplyr::coalesce(gs_share, 0))
}

# Signals at the base season (MLB row preferred), scaled; zeros when absent.
signal_features <- function(signals, bases, grade_override = NULL) {
  s <- signals %>% dplyr::mutate(pref = ifelse(level == "MLB", 1L, 2L)) %>%
    dplyr::arrange(pitcher, season, pref) %>% dplyr::distinct(pitcher, season, .keep_all = TRUE) %>%
    dplyr::select(pitcher, base_season = season, dplyr::all_of(c(SIGNAL_GRADES, SIGNAL_X)))
  out <- dplyr::left_join(bases, s, by = c("pitcher", "base_season"))
  if (!is.null(grade_override)) {
    out <- dplyr::left_join(dplyr::select(out, -dplyr::all_of(SIGNAL_GRADES)), grade_override, by = c("pitcher", "base_season"))
  }
  out %>% dplyr::mutate(
    has_grades = as.numeric(!is.na(stuff)),
    dplyr::across(dplyr::all_of(SIGNAL_GRADES), ~ ifelse(is.na(.x), 0, (pmin(pmax(.x, 20), 80) - 50) / 10)),
    dplyr::across(dplyr::all_of(SIGNAL_X), ~ ifelse(is.na(.x) | has_grades == 0, 0, .x)))
}

lg_logit_of <- function(league, component, seasons) {
  lg <- league[league$level == "MLB", ]
  stats::qlogis(lg[[paste0("lg_", component)]][match(seasons, lg$season)])
}

training_rows <- function(lines, signals, windows, parks, league, horizon, component) {
  cl <- component_lines(lines, parks, league, component)
  if (horizon >= 1) {
    target <- cl %>% dplyr::filter(level == "MLB", tr >= 1) %>%
      dplyr::transmute(pitcher, base_season = season - horizon, target_season = season, events = ev, trials = tr)
    bases <- dplyr::distinct(cl, pitcher, base_season = season) %>% dplyr::semi_join(target, by = c("pitcher", "base_season"))
    h <- history_features(cl, bases)
    rows <- h %>% dplyr::inner_join(target, by = c("pitcher", "base_season")) %>%
      dplyr::left_join(signal_features(signals, bases), by = c("pitcher", "base_season")) %>%
      dplyr::mutate(window = "full", age_target = age + horizon)
  } else {
    rows <- window_training_rows(cl, signals, windows, parks, league, component)
  }
  rows %>% dplyr::mutate(horizon = as.integer(horizon), lg_logit = lg_logit_of(league, component, target_season)) %>%
    dplyr::filter(!is.na(lg_logit)) %>% finish_rows()
}

# Rest-of-season rows: history = the two prior seasons + the partial window (weight 5);
# target = season to date minus the window; grades are the window's own.
window_training_rows <- function(cl, signals, windows, parks, league, component) {
  if (is.null(windows) || !nrow(windows)) return(tibble::tibble())
  w <- windows[windows$level == "MLB", ]
  wl <- component_lines(
    dplyr::transmute(w, pitcher, season, level, window, age = NA_real_, gs_share = NA_real_, tbf,
                     k, bbhbp, hr, bip, hbip),
    parks, league, component) %>%
    dplyr::mutate(window = w$window)
  full <- wl[wl$window == "to_date", ]
  part <- wl[wl$window %in% TRAIN_WINDOWS, ]
  out <- lapply(TRAIN_WINDOWS, function(win) {
    p <- part[part$window == win, ]
    if (!nrow(p)) return(NULL)
    tgt <- dplyr::inner_join(dplyr::select(p, pitcher, season, ev, tr), dplyr::select(full, pitcher, season, ev_all = ev, tr_all = tr),
                             by = c("pitcher", "season")) %>%
      dplyr::transmute(pitcher, base_season = season, target_season = season, events = ev_all - ev, trials = tr_all - tr) %>%
      dplyr::filter(trials >= 1)
    bases <- dplyr::distinct(tgt, pitcher, base_season)
    extra <- dplyr::transmute(p, pitcher, base_season = season, level, ev, tr, lg)
    h <- history_features(cl, bases, offsets = 1:2, extra = extra)
    base_line <- cl %>% dplyr::filter(level == "MLB") %>% dplyr::select(pitcher, base_season = season, age_line = age, gs_line = gs_share)
    go <- w[w$window == win, ] %>% dplyr::select(pitcher, base_season = season, dplyr::all_of(SIGNAL_GRADES))
    prior <- signals %>% dplyr::mutate(season = season + 1L)
    h %>% dplyr::inner_join(tgt, by = c("pitcher", "base_season")) %>%
      dplyr::left_join(base_line, by = c("pitcher", "base_season")) %>%
      dplyr::mutate(age = dplyr::coalesce(age, age_line), gs_share = dplyr::coalesce(gs_line, gs_share)) %>%
      dplyr::select(-age_line, -gs_line) %>%
      dplyr::left_join(signal_features(prior, bases, grade_override = go), by = c("pitcher", "base_season")) %>%
      dplyr::mutate(window = win, age_target = age)
  })
  dplyr::bind_rows(out)
}

finish_rows <- function(r) {
  if (!nrow(r)) return(r)
  r %>% dplyr::filter(!is.na(age_target)) %>%
    dplyr::select(pitcher, base_season, target_season, window, horizon, dplyr::any_of(c("events", "trials")),
                  dplyr::any_of("lg_logit"), past_rel, past_trials, has_grades, dplyr::all_of(c(SIGNAL_GRADES, SIGNAL_X)),
                  age_target, gs_share, aaa_share)
}

# Rows to project from the base season (no target). Every pitcher with a line that season.
newdata_rows <- function(lines, signals, parks, league, base_season, horizon, component) {
  cl <- component_lines(lines, parks, league, component)
  bases <- dplyr::distinct(cl[cl$season == base_season, ], pitcher, base_season = season)
  history_features(cl, bases) %>%
    dplyr::left_join(signal_features(signals, bases), by = c("pitcher", "base_season")) %>%
    dplyr::mutate(window = "full", horizon = as.integer(horizon), target_season = base_season + horizon,
                  age_target = age + horizon) %>%
    finish_rows()
}
