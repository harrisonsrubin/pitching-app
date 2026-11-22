# True-talent projection: inputs, park factors, models, simulation, aging and luck

# Projection inputs

PROJ_COMPONENTS <- c("k", "bbhbp", "hr", "hbip")

# Trials per component: batters faced, or balls in play for hits on BIP
component_trials <- function(component) if (identical(component, "hbip")) "bip" else "tbf"

# Season totals per pitcher, level and season; missing ages get the league median
season_lines <- function(stats) {
  l <- stats %>%
    dplyr::group_by(pitcher = as.integer(player_id), season = as.integer(season), level) %>%
    dplyr::summarise(age = suppressWarnings(max(age, na.rm = TRUE)),
                     g = sum(g, na.rm = TRUE), gs = sum(gs, na.rm = TRUE),
                     tbf = sum(tbf, na.rm = TRUE), k = sum(so, na.rm = TRUE),
                     bb = sum(bb, na.rm = TRUE), hbp = sum(hbp, na.rm = TRUE), hr = sum(hr, na.rm = TRUE),
                     h = sum(h, na.rm = TRUE), outs = sum(ip_outs, na.rm = TRUE),
                     r = sum(r, na.rm = TRUE), er = sum(er, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(age = ifelse(is.finite(age), age, NA_real_),
                  bbhbp = bb + hbp,
                  bip = pmax(tbf - k - bb - hbp - hr, 0),
                  hbip = pmax(h - hr, 0),
                  gs_share = ifelse(g > 0, gs / g, 0))
  l %>%
    dplyr::group_by(season, level) %>%
    dplyr::mutate(age_missing = is.na(age), age = ifelse(is.na(age), stats::median(age, na.rm = TRUE), age)) %>%
    dplyr::ungroup() %>%
    dplyr::select(pitcher, season, level, age, age_missing, tbf, k, bbhbp, hr, bip, hbip, outs, r, er, h, bb, hbp, gs_share)
}

# League rates, ER/R ratio, FIP constant and RA9 per season and level
league_table <- function(lines) {
  lines %>%
    dplyr::group_by(season, level) %>%
    dplyr::summarise(tbf = sum(tbf), k = sum(k), bbhbp = sum(bbhbp), hr = sum(hr), bip = sum(bip), hbip = sum(hbip),
                     outs = sum(outs), r = sum(r), er = sum(er), .groups = "drop") %>%
    dplyr::mutate(lg_k = k / tbf, lg_bbhbp = bbhbp / tbf, lg_hr = hr / tbf, lg_hbip = hbip / bip,
                  er_r = er / r, ip = outs / 3,
                  fip_const = 9 * er / ip - (13 * hr + 3 * bbhbp - 2 * k) / ip,
                  lg_ra9 = 27 * r / outs) %>%
    dplyr::select(season, level, lg_k, lg_bbhbp, lg_hr, lg_hbip, er_r, fip_const, lg_ra9)
}

# Season grades plus usage-weighted expected whiff, chase, strike, contact wOBA and GB
pitch_signals <- function(grades, components) {
  g <- grades %>%
    dplyr::filter(pitch_type == "ALL", half == "full", facet %in% c("stuff", "location", "pitching")) %>%
    dplyr::select(level, season, pitcher, facet, grade_global) %>%
    tidyr::pivot_wider(names_from = facet, values_from = grade_global)
  x <- components %>%
    dplyr::filter(pitch_type != "ALL", !is.na(n), n > 0) %>%
    dplyr::group_by(level, season, pitcher) %>%
    dplyr::summarise(x_whiff = stats::weighted.mean(x_whiff_per_swing, n, na.rm = TRUE),
                     x_chase = stats::weighted.mean(x_chase, n, na.rm = TRUE),
                     x_cstrike = stats::weighted.mean(x_cstrike_per_take, n, na.rm = TRUE),
                     x_woba_contact = stats::weighted.mean(x_woba_contact, n, na.rm = TRUE),
                     x_gb = stats::weighted.mean(x_gb, n, na.rm = TRUE), .groups = "drop")
  dplyr::full_join(g, x, by = c("level", "season", "pitcher")) %>%
    dplyr::select(level, season, pitcher, dplyr::any_of(c("stuff", "location", "pitching")),
                  x_whiff, x_chase, x_cstrike, x_woba_contact, x_gb)
}

# In-season windows and park factors

WINDOW_NS <- c(100L, 250L, 500L)
BB_EVENTS <- c("walk", "intent_walk")
HIT_BIP_EVENTS <- c("single", "double", "triple")

# Store keys for window, park and park-sum files
windows_key <- function(level, season) outputs_key(sprintf("windows_%s_%s.parquet", level, season))
parks_key <- function(level, season) outputs_key(sprintf("parks_%s_%s.parquet", level, season))
park_sums_key <- function(level, season) sprintf("processed/park_sums/%s/%s.parquet", level, season)

# Component counts per pitcher, optionally only PAs at or before each pitcher's cutoff
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

# Counts and re-fit grades after 100/250/500 pitches and for the season to date
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

# Each team's home and road totals per season, stored so old pitch files never reload
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
  # Road side: every game the team played away, both teams' events
  road <- per_game %>% dplyr::group_by(season, team = away_team) %>%
    dplyr::summarise(dplyr::across(c(pa, hr, bip, hbip, runs), sum), games = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(side = "road", venue_id = NA_integer_)
  dplyr::bind_rows(home, road) %>% dplyr::select(season, venue_id, team, side, pa, hr, bip, hbip, runs, games)
}

# Venue factors, seasons s-2..s: home rate over the team's road rate, regressed halfway
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

# PA-weighted park factors over the parks each pitcher threw in; missing parks count as 1
pitcher_parks <- function(pa, scores, cpf) {
  pa %>%
    dplyr::filter(!is.na(events)) %>%
    dplyr::left_join(dplyr::select(scores, game_pk, venue_id, season), by = c("game_pk", "season")) %>%
    dplyr::left_join(cpf, by = c("venue_id", "season")) %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(ppf_runs = mean(dplyr::coalesce(pf_runs, 1)), ppf_hr = mean(dplyr::coalesce(pf_hr, 1)),
                     ppf_hbip = mean(dplyr::coalesce(pf_hbip, 1)), .groups = "drop")
}

# Write park and window files for one level-season, then free the raw pitches
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

# Seasons that already have a stored file for this key
stored_seasons_with <- function(key_fn, level) {
  s <- 2000:2100
  s[vapply(s, function(y) store_exists(key_fn(level, y)), TRUE)]
}

# Aging

AGE_RANGE <- 20:42

# Mean year-to-year logit change by age, weighted by trials
aging_deltas <- function(lines, component, min_tbf = 100, correct = TRUE) {
  tcol <- component_trials(component)
  l <- lines[lines$level == "MLB" & lines[[tcol]] >= min_tbf, ]
  l$trials <- l[[tcol]]
  l$lg <- stats::qlogis((l[[component]] + 0.5) / (l$trials + 1))
  l <- l[, c("pitcher", "season", "age", "trials", "lg")]
  nxt <- dplyr::mutate(l, season = season - 1L) %>% dplyr::rename(trials2 = trials, lg2 = lg, age2 = age)
  pairs <- dplyr::inner_join(l, nxt, by = c("pitcher", "season")) %>%
    dplyr::mutate(delta = lg2 - lg, w = 2 / (1 / trials + 1 / trials2), age = floor(age))
  by_age <- pairs %>% dplyr::group_by(age) %>%
    dplyr::summarise(m = stats::weighted.mean(delta, w), s = stats::sd(delta), .groups = "drop")
  if (correct) {
  # Pitchers who vanish get their age group's mean change minus half an SD
    last_season <- max(lines$season)
    drops <- dplyr::anti_join(l, nxt, by = c("pitcher", "season")) %>%
      dplyr::filter(season < last_season) %>%
      dplyr::mutate(age = floor(age)) %>%
      dplyr::inner_join(by_age, by = "age") %>%
      dplyr::transmute(age, delta = m - 0.5 * dplyr::coalesce(s, 0), w = trials / 2)
    pairs <- dplyr::bind_rows(dplyr::select(pairs, age, delta, w), drops)
  }
  pairs %>% dplyr::group_by(age) %>%
    dplyr::summarise(delta = stats::weighted.mean(delta, w), n = sum(w), .groups = "drop") %>%
    dplyr::filter(!is.na(delta))
}

# Smooth the age deltas and sum them into a curve that is 0 at age 27
aging_curve <- function(lines, component, correct = TRUE) {
  d <- aging_deltas(lines, component, correct = correct)
  fit <- stats::lm(delta ~ splines::ns(age, df = 4), data = d, weights = n)
  ages <- AGE_RANGE
  step <- stats::predict(fit, newdata = data.frame(age = pmin(pmax(ages, min(d$age)), max(d$age))))
  names(step) <- ages
  cum <- vapply(ages, function(a) {
    if (a == 27) 0
    else if (a > 27) sum(step[as.character(27:(a - 1))])
    else -sum(step[as.character(a:26)])
  }, 0)
  tibble::tibble(age = ages, cum = unname(cum))
}

# Training and projection rows

# Weights for the base season and the two before it
RECENCY_W <- c(5, 4, 3)
# League-rate padding so rare events (a reliever with no HR) don't give extreme logits
PAST_PRIOR_W <- 100
SIGNAL_GRADES <- c("stuff", "location", "pitching")
SIGNAL_X <- c("x_whiff", "x_chase", "x_cstrike", "x_woba_contact", "x_gb")
TRAIN_WINDOWS <- c("100", "250", "500")

# Park factor column for components that get park-adjusted
park_col <- function(component) switch(component, hr = "ppf_hr", hbip = "ppf_hbip", NA_character_)

# Park-adjusted events, trials and own-league rate per line
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

# Recency-weighted past results vs league, plus age, role and Triple-A share
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

# Base-season signals (MLB row first), grades scaled to (g - 50) / 10, zero when missing
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

# MLB league logit rate for each season
lg_logit_of <- function(league, component, seasons) {
  lg <- league[league$level == "MLB", ]
  stats::qlogis(lg[[paste0("lg_", component)]][match(seasons, lg$season)])
}

# Training rows: target-season results with features from the base season and earlier
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

# Rest-of-season rows: prior seasons plus the partial window, target is the remainder
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

# Drop rows without a target age and keep the model columns
finish_rows <- function(r) {
  if (!nrow(r)) return(r)
  r %>% dplyr::filter(!is.na(age_target)) %>%
    dplyr::select(pitcher, base_season, target_season, window, horizon, dplyr::any_of(c("events", "trials")),
                  dplyr::any_of("lg_logit"), past_rel, past_trials, has_grades, dplyr::all_of(c(SIGNAL_GRADES, SIGNAL_X)),
                  age_target, gs_share, aaa_share)
}

# Rows to project for every pitcher with a line in the base season
newdata_rows <- function(lines, signals, parks, league, base_season, horizon, component) {
  cl <- component_lines(lines, parks, league, component)
  bases <- dplyr::distinct(cl[cl$season == base_season, ], pitcher, base_season = season)
  lost <- unique(cl$pitcher[cl$season == base_season - 1 & cl$level == "MLB"])
  lost <- setdiff(lost, bases$pitcher)  # missed the whole base season
  if (length(lost)) bases <- dplyr::bind_rows(bases, tibble::tibble(pitcher = lost, base_season = as.integer(base_season)))
  last_age <- cl[cl$season < base_season, ] %>% dplyr::group_by(pitcher) %>%
    dplyr::arrange(season, .by_group = FALSE) %>%
    dplyr::summarise(age_fill = max(age) + (base_season - max(season)), gs_fill = dplyr::last(gs_share), .groups = "drop")
  history_features(cl, bases) %>%
    dplyr::left_join(last_age, by = "pitcher") %>%
    dplyr::mutate(age = dplyr::coalesce(age, age_fill), gs_share = ifelse(pitcher %in% lost, gs_fill, gs_share)) %>%
    dplyr::select(-age_fill, -gs_fill) %>%
    dplyr::left_join(signal_features(signals, bases), by = c("pitcher", "base_season")) %>%
    dplyr::mutate(window = "full", horizon = as.integer(horizon), target_season = base_season + horizon,
                  age_target = age + horizon) %>%
    finish_rows()
}

# Component models

# Pitch-model terms in the model
GRADE_TERMS <- c("has_grades", "stuff", "location", "pitching", "x_whiff", "x_chase", "x_cstrike", "x_woba_contact", "x_gb")
# Quality terms on each side of the stuff vs results weight
SIGNAL_WEIGHT_TERMS <- setdiff(GRADE_TERMS, "has_grades")
RESULT_WEIGHT_TERMS <- c("past_rel", "past_rel:log1p(past_trials)", "past_rel:aaa_share")

# GLMM formula, keeping only terms that vary in the data
component_formula <- function(rows) {
  varies <- function(v) v %in% names(rows) && length(unique(rows[[v]])) > 1
  terms <- c("past_rel * log1p(past_trials)",
             GRADE_TERMS[vapply(GRADE_TERMS, varies, TRUE)],
             "splines::ns(age_target, df = 4)", if (varies("gs_share")) "gs_share",
             if (varies("aaa_share")) c("aaa_share", "past_rel:aaa_share"),
             if (varies("window")) "window")
  stats::as.formula(paste("cbind(events, trials - events) ~ offset(lg_off) +", paste(terms, collapse = " + "),
                          "+ (1 | pitcher) + (1 | pitcher_season)"))
}

# Fit one binomial GLMM for a component and horizon
fit_component <- function(rows, component) {
  rows <- rows[rows$trials >= 1 & !is.na(rows$events), ]
  rows$events <- pmin(round(rows$events), rows$trials)  # park-adjusted counts are not integers
  rows$trials <- round(rows$trials)
  # One combined pitcher-season key; a one-level season factor breaks predict()
  rows$pitcher_season <- paste(rows$pitcher, rows$target_season, sep = "_")
  if ("window" %in% names(rows)) rows$window <- factor(rows$window)
  # Center the league offset so glmer converges
  lg_center <- mean(rows$lg_logit)
  rows$lg_off <- rows$lg_logit - lg_center
  f <- component_formula(rows)
  fit <- lme4::glmer(f, data = rows, family = stats::binomial(), nAGQ = 0,
                     control = lme4::glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
  m <- list(fit = fit, component = component, horizon = unique(rows$horizon), formula = f, lg_center = lg_center,
            windows = if ("window" %in% names(rows)) levels(rows$window) else NULL)
  m$unc <- fit_uncertainty(m, rows)
  m$x_means <- colMeans(lme4::getME(fit, "X"))
  m
}

# Talent variance for unseen pitchers by past sample size, fit as a * kk / (kk + n)
fit_uncertainty <- function(m, rows) {
  pr <- stats::predict(m$fit, re.form = NA, type = "link")
  p <- stats::plogis(pr)
  obs <- stats::qlogis((rows$events + 0.5) / (rows$trials + 1))
  excess <- (obs - pr)^2 - 1 / ((rows$trials + 1) * p * (1 - p)) - change_variance(m)
  bins <- dplyr::ntile(rows$past_trials, 10)
  b <- tibble::tibble(n = rows$past_trials, e = excess, bin = bins) %>% dplyr::group_by(bin) %>%
    dplyr::summarise(n = stats::median(n), e = mean(e), w = dplyr::n(), .groups = "drop")
  # Grid search kk, solve a by least squares
  best <- c(a = 0, kk = 100); best_sse <- Inf
  for (kk in 10^seq(1, 4, by = 0.1)) {
    x <- kk / (kk + b$n)
    a <- max(sum(b$w * b$e * x) / sum(b$w * x^2), 0)
    sse <- sum(b$w * (b$e - a * x)^2)
    if (sse < best_sse) { best_sse <- sse; best <- c(a = a, kk = kk) }
  }
  best
}

# Talent variance for a pitcher the fit hasn't seen
talent_uncertainty <- function(m, past_trials) {
  if (is.null(m$unc)) return(rep(0, length(past_trials)))
  m$unc[["a"]] * m$unc[["kk"]] / (m$unc[["kk"]] + past_trials)
}

# Add the keys and offset the model expects to new rows
prep_newdata <- function(m, newdata) {
  newdata$pitcher_season <- paste(newdata$pitcher, newdata$target_season, sep = "_")
  newdata$lg_off <- newdata$lg_logit - m$lg_center
  if (!is.null(m$windows)) newdata$window <- factor(ifelse(newdata$window %in% m$windows, newdata$window,
                                                           m$windows[length(m$windows)]), levels = m$windows)
  newdata
}

# Talent logit: fixed part plus the pitcher's own effect, no season change
predict_component <- function(m, newdata) {
  stats::predict(m$fit, newdata = prep_newdata(m, newdata), re.form = ~ (1 | pitcher),
                 allow.new.levels = TRUE, type = "link")
}

# Fixed-effects design matrix for new rows
fixed_design <- function(m, newdata) {
  nd <- prep_newdata(m, newdata)
  nd$events <- 0; nd$trials <- 1
  tt <- stats::delete.response(stats::terms(m$fit, fixed.only = TRUE))
  X <- stats::model.matrix(tt, stats::model.frame(tt, nd, na.action = stats::na.pass))
  X[, colnames(X) %in% names(lme4::fixef(m$fit)), drop = FALSE]
}

# Share of a pitcher's estimate driven by pitch-model signals rather than results
stuff_weight <- function(m, newdata) {
  X <- fixed_design(m, newdata); b <- lme4::fixef(m$fit)[colnames(X)]
  mu <- if (!is.null(m$x_means)) m$x_means[colnames(X)] else rep(0, ncol(X))
  Xc <- sweep(X, 2, mu)
  part <- function(cols) { k <- colnames(Xc) %in% cols; if (!any(k)) rep(0, nrow(Xc)) else as.vector(Xc[, k, drop = FALSE] %*% b[k]) }
  g <- abs(part(SIGNAL_WEIGHT_TERMS)); r <- abs(part(RESULT_WEIGHT_TERMS))
  if ("has_grades" %in% names(newdata)) g[newdata$has_grades == 0] <- 0  # no signals
  ifelse(g + r > 0, g / (g + r), 0)
}

# Variance of the pitcher-season change effect
change_variance <- function(m) {
  vc <- as.data.frame(lme4::VarCorr(m$fit))
  v <- vc$vcov[vc$grp == "pitcher_season"]
  if (length(v)) v else 0
}

# Variance of the pitcher effect
pitcher_variance <- function(m) {
  vc <- as.data.frame(lme4::VarCorr(m$fit))
  vc$vcov[vc$grp == "pitcher"]
}

# Simulation

# ERA change that counts as a step forward or back
STEP_RUNS <- 0.75
# Forecast targets start in 2020, when park factors begin
PROJ_MIN_TARGET <- 2020L
# Season 2 method: the two-year model, or next season stepped with the aging curve
PROJ_H2_METHOD <- "model"
CHIP_PROBS <- seq(0.05, 0.95, by = 0.1)

# Ten chip values: grade quantiles at 5%, 15%, ..., 95%, capped 20-80
chips <- function(grade_draws) {
  q <- stats::quantile(grade_draws, CHIP_PROBS, names = FALSE, na.rm = TRUE)
  pmin(pmax(q, 20), 80)
}

# RA9 to the 20-80 true talent grade
tt_grade <- function(ra9, anchors) pmin(pmax(50 - 10 * (ra9 - anchors$mean[1]) / anchors$unit[1], 20), 80)

# Share of draws where ERA moves by at least STEP_RUNS either way
step_odds <- function(era_now, era_next) {
  d <- era_next - era_now
  c(forward = mean(d <= -STEP_RUNS), back = mean(d >= STEP_RUNS))
}

# MLB-equivalent basis when most of the base season was in Triple-A
basis_of <- function(lines, base_season) {
  lines[lines$season == base_season, ] %>% dplyr::group_by(pitcher) %>%
    dplyr::summarise(aaa = sum(tbf[level == "AAA"]), all = sum(tbf), .groups = "drop") %>%
    dplyr::transmute(pitcher, basis = ifelse(aaa > 0.5 * all, "MLB-equivalent", "MLB"))
}

# Add any missing signal columns and fix key types
ensure_signal_cols <- function(signals) {
  for (v in c("level", SIGNAL_GRADES, SIGNAL_X)) if (!v %in% names(signals))
    signals[[v]] <- if (v == "level") character(nrow(signals)) else numeric(nrow(signals))
  signals$season <- as.integer(signals$season); signals$pitcher <- as.integer(signals$pitcher)
  signals
}

# Fit every component model per horizon (0 = rest of season when windows exist)
fit_projection_models <- function(lines, signals, windows, parks, league, horizons = 1:2) {
  signals <- ensure_signal_cols(signals)
  hs <- c(if (!is.null(windows) && nrow(windows)) 0L, as.integer(horizons))
  stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) {
    stats::setNames(lapply(hs, function(h) {
      r <- training_rows(lines, signals, windows, parks, league, h, cpt)
      fit_component(r[r$target_season >= PROJ_MIN_TARGET, ], cpt)
    }), paste0("h", hs))
  }), PROJ_COMPONENTS)
}

# Runs map: park-adjusted RA9 on the four rates, MLB seasons with 50+ IP, outs-weighted
fit_runs_map <- function(lines, parks) {
  l <- lines[lines$level == "MLB" & lines$outs >= 150 & lines$bip > 0, ]
  if (!is.null(parks) && nrow(parks)) {
    l <- dplyr::left_join(l, dplyr::select(parks, pitcher, level, season, ppf_runs), by = c("pitcher", "level", "season"))
  } else l$ppf_runs <- NA_real_
  l <- dplyr::mutate(l, ra9_adj = 27 * r / outs / dplyr::coalesce(ppf_runs, 1),
                     k_r = k / tbf, bbhbp_r = bbhbp / tbf, hr_r = hr / tbf, babip = hbip / bip)
  stats::lm(ra9_adj ~ k_r + bbhbp_r + hr_r + babip, data = l, weights = outs)
}

# Season RA9 noise the four rates don't explain, scaled to a season of `outs`
runs_noise_sd <- function(map, outs) stats::sigma(map) / sqrt(pmax(outs, 1))

# RA9 from the runs map, floored at 0.5
ra9_from_rates <- function(map, k, bbhbp, hr, hbip) {
  b <- stats::coef(map)
  pmax(b[["(Intercept)"]] + b[["k_r"]] * k + b[["bbhbp_r"]] * bbhbp + b[["hr_r"]] * hr + b[["babip"]] * hbip, 0.5)
}

# FIP from per-batter rates
fip_from_rates <- function(k, bbhbp, hr, hbip, fip_const) {
  hits <- hr + hbip * pmax(1 - k - bbhbp - hr, 0)
  ip_pt <- pmax(1 - bbhbp - hits, 0.05) / 3
  (13 * hr + 3 * bbhbp - 2 * k) / ip_pt + fip_const
}

# Fixed part under coefficient draws B, plus the centered league offset
fixed_draws <- function(m, nd, B) {
  X <- fixed_design(m, nd)
  X %*% t(B[, colnames(X), drop = FALSE]) + (nd$lg_logit - m$lg_center)
}

# Draws of the fixed-effect coefficients
coef_draws <- function(m, n_draws) {
  b <- lme4::fixef(m$fit); V <- as.matrix(stats::vcov(m$fit))
  B <- MASS::mvrnorm(n_draws, b, V)
  if (n_draws == 1) B <- matrix(B, nrow = 1, dimnames = list(NULL, names(b)))
  B
}

# Pitcher talent draws shared across seasons; unseen pitchers get sample-size uncertainty
pitcher_draws <- function(m, nd, n_draws) {
  re <- lme4::ranef(m$fit, condVar = TRUE)$pitcher
  pv <- attr(re, "postVar")
  modes <- stats::setNames(re[[1]], rownames(re)); vars <- stats::setNames(as.vector(pv), rownames(re))
  key <- as.character(nd$pitcher)
  mu <- ifelse(key %in% names(modes), modes[key], 0)
  v <- ifelse(key %in% names(vars), vars[key], pmax(talent_uncertainty(m, nd$past_trials), pitcher_variance(m)))
  mu + sqrt(v) * matrix(stats::rnorm(nrow(nd) * n_draws), nrow(nd))
}

# Row-wise 5th percentile, 95th percentile and median
q5 <- function(M) apply(M, 1, stats::quantile, 0.05, names = FALSE, na.rm = TRUE)
q95 <- function(M) apply(M, 1, stats::quantile, 0.95, names = FALSE, na.rm = TRUE)
qmed <- function(M) apply(M, 1, stats::median, na.rm = TRUE)

# Simulate each pitcher now and 1-5 seasons out: rates, ERA, FIP, grade, chips, Step odds
project_pitchers <- function(models, lines, signals, parks, league, base_season, map, anchors, curves,
                             n_draws = 1000L, seed = 1L, keep_draws = FALSE, h2_method = PROJ_H2_METHOD,
                             pt = NULL, pt_new = NULL, war_k = NULL, pt_shift = 0L) {
  set.seed(seed)
  kept <- list()
  signals <- ensure_signal_cols(signals)
  lg_mlb <- league[league$level == "MLB" & league$season <= base_season, ]
  lg_last <- lg_mlb[which.max(lg_mlb$season), ]
  nd <- lapply(PROJ_COMPONENTS, function(cpt) {
    a <- newdata_rows(lines, signals, parks, league, base_season, 1L, cpt)
    a$lg_logit <- stats::qlogis(lg_last[[paste0("lg_", cpt)]]); a
  })
  names(nd) <- PROJ_COMPONENTS
  ids <- nd$k$pitcher
  age_now <- nd$k$age_target - 1
  rates <- list(); now <- list()
  for (cpt in PROJ_COMPONENTS) {
    m1 <- models[[cpt]]$h1; m2 <- models[[cpt]]$h2
    d1 <- nd[[cpt]][match(ids, nd[[cpt]]$pitcher), ]
    today <- d1; today$age_target <- today$age_target - 1
    B1 <- coef_draws(m1, n_draws); own <- pitcher_draws(m1, d1, n_draws)
    e_now <- fixed_draws(m1, today, B1) + own
    e1 <- fixed_draws(m1, d1, B1) + own
    d2 <- d1; d2$age_target <- d1$age_target + 1; d2$horizon <- 2L; d2$target_season <- d1$target_season + 1L
    cur <- curves[[cpt]]
    cum_at <- function(a) cur$cum[match(pmin(pmax(round(a), min(cur$age)), max(cur$age)), cur$age)]
    e2 <- if (identical(h2_method, "step")) e1 + (cum_at(age_now + 2) - cum_at(age_now + 1))
          else fixed_draws(m2, d2, coef_draws(m2, n_draws)) + own
    sdc <- sqrt(change_variance(m1))
    ch <- lapply(1:5, function(i) matrix(stats::rnorm(length(e1), 0, sdc), nrow(e1)))
    now[[cpt]] <- e_now
    rates[[cpt]] <- lapply(1:5, function(h) {
      base <- if (h == 1) e1 else e2 + (if (h > 2) cum_at(age_now + h) - cum_at(age_now + 2) else 0)
      stats::plogis(base + Reduce(`+`, ch[1:h]))
    })
    now[[cpt]] <- stats::plogis(e_now)
  }
  # Simulated season outcomes at last season's workload (at least 150 TBF)
  tbf_last <- lines[lines$season == base_season, ] %>% dplyr::group_by(pitcher) %>%
    dplyr::summarise(tbf = sum(tbf), .groups = "drop")
  tbf_out <- pmax(tbf_last$tbf[match(ids, tbf_last$pitcher)], 150, na.rm = TRUE)
  outcome <- function(r) {
    n <- matrix(tbf_out, nrow(r$k), ncol(r$k))
    k <- stats::rbinom(length(n), n, r$k); bb <- stats::rbinom(length(n), n, r$bbhbp); hr <- stats::rbinom(length(n), n, r$hr)
    bip <- pmax(n - k - bb - hr, 1); hb <- stats::rbinom(length(n), bip, r$hbip)
    outs <- pmax(bip - hb + k, 1)
    pmax(ra9_from_rates(map, k / n, bb / n, hr / n, matrix(hb / bip, nrow(n))) +
           stats::rnorm(length(n)) * runs_noise_sd(map, outs), 0)
  }
  era_of <- function(ra9) ra9 * lg_last$er_r
  # Playing-time draws aligned to the projected pitchers (no row means no MLB innings)
  sims <- NULL
  if (!is.null(pt) && !is.null(pt_new) && nrow(pt_new)) {
    rows <- pt_new[match(ids, pt_new$pitcher), ]
    ok <- !is.na(rows$pitcher)
    talent_ra9 <- lapply(1:5, function(h) { r <- lapply(rates, `[[`, h); ra9_from_rates(map, r$k, r$bbhbp, r$hr, r$hbip)[ok, , drop = FALSE] })
    perf <- lapply(c(rep(list(talent_ra9[[1]]), pt_shift), talent_ra9), perf_from_draws, past = rows$perf_ra9[ok])
    s_ok <- simulate_playing_time(pt, rows[ok, ], n_draws, 5L + pt_shift, perf = perf)
    s_ok <- s_ok[(pt_shift + 1L):(pt_shift + 5L)]  # mid-season: start from the last complete season
    sims <- lapply(s_ok, function(x) {
      o <- matrix(0, length(ids), n_draws); sp <- matrix(0, length(ids), n_draws)
      o[ok, ] <- x$outs; sp[ok, ] <- x$starter
      list(outs = o, starter = sp)
    })
  }
  # Season outcomes with simulated innings: batters faced from each draw's outs and rates
  outcome_pt <- function(r, outs) {
    hits <- r$hr + r$hbip * pmax(1 - r$k - r$bbhbp - r$hr, 0)
    n <- pmax(round(outs / pmax(1 - r$bbhbp - hits, 0.3)), 1)
    k <- stats::rbinom(length(n), n, r$k); bb <- stats::rbinom(length(n), n, r$bbhbp); hr <- stats::rbinom(length(n), n, r$hr)
    bip <- pmax(n - k - bb - hr, 1); hb <- stats::rbinom(length(n), bip, r$hbip)
    ra9 <- pmax(ra9_from_rates(map, k / n, bb / n, hr / n, matrix(hb / bip, nrow(outs))) +
                  stats::rnorm(length(n)) * runs_noise_sd(map, pmax(outs, 1)), 0)
    matrix(ra9, nrow(outs))
  }
  r_now <- now
  era_now <- era_of(ra9_from_rates(map, r_now$k, r_now$bbhbp, r_now$hr, r_now$hbip))
  sw <- Reduce(`+`, lapply(PROJ_COMPONENTS, function(cpt) stuff_weight(models[[cpt]]$h1, nd[[cpt]][match(ids, nd[[cpt]]$pitcher), ]))) / length(PROJ_COMPONENTS)
  pp <- if (!is.null(parks) && nrow(parks)) parks[parks$season == base_season, ] %>% dplyr::arrange(level != "MLB") %>%
    dplyr::distinct(pitcher, .keep_all = TRUE) else tibble::tibble(pitcher = integer(), ppf_runs = numeric())
  ppf <- dplyr::coalesce(pp$ppf_runs[match(ids, pp$pitcher)], 1)
  basis <- basis_of(lines, base_season)
  out <- lapply(1:5, function(h) {
    r <- lapply(rates, `[[`, h)
    ra9 <- ra9_from_rates(map, r$k, r$bbhbp, r$hr, r$hbip)
    era <- era_of(ra9); fip <- fip_from_rates(r$k, r$bbhbp, r$hr, r$hbip, lg_last$fip_const)
    g <- tt_grade(ra9, anchors)
    if (!is.null(sims)) {
      outs <- sims[[h]]$outs; sp <- sims[[h]]$starter
      ro <- outcome_pt(r, outs)
      wd <- war_from(ro, outs, sp, lg_last$lg_ra9, war_k %||% 1)
      eo <- era_of(ro); eo[outs == 0] <- NA
    } else eo <- era_of(outcome(r))
    if (keep_draws && h == 1) kept <<- list(era = era, era_out = eo, era_now = era_now)
    t <- tibble::tibble(pitcher = ids, horizon = h, target_season = base_season + h)
    for (cpt in PROJ_COMPONENTS) {
      t[[cpt]] <- qmed(r[[cpt]]); t[[paste0(cpt, "_lo")]] <- q5(r[[cpt]]); t[[paste0(cpt, "_hi")]] <- q95(r[[cpt]])
    }
    t$era <- qmed(era); t$era_lo <- q5(era); t$era_hi <- q95(era)
    t$era_out_lo <- pmin(q5(eo), t$era_lo); t$era_out_hi <- pmax(q95(eo), t$era_hi)
    t$fip <- qmed(fip); t$fip_lo <- q5(fip); t$fip_hi <- q95(fip)
    t$ra9 <- qmed(ra9); t$tt_grade <- qmed(g); t$tt_lo <- q5(g); t$tt_hi <- q95(g)
    ch <- if (h == 1) t(apply(g, 1, chips)) else matrix(NA_real_, length(ids), 10)
    for (i in 1:10) t[[paste0("chip_", i)]] <- ch[, i]
    if (h == 1) {
      so <- t(vapply(seq_along(ids), function(i) step_odds(era_now[i, ], era[i, ]), c(forward = 0, back = 0)))
      t$step_forward <- so[, "forward"]; t$step_back <- so[, "back"]
    } else { t$step_forward <- NA_real_; t$step_back <- NA_real_ }
    t$stuff_weight <- sw; t$era_home <- t$era * ppf
    if (!is.null(sims)) {
      pitched <- outs > 0
      t$p_mlb <- rowMeans(pitched)
      t$p_starter <- ifelse(rowSums(pitched) > 0, rowSums(sp * pitched) / pmax(rowSums(pitched), 1), NA_real_)
      t$ip <- rowMeans(outs) / 3; t$ip_lo <- q5(outs / 3); t$ip_hi <- q95(outs / 3)
      t$war <- rowMeans(wd); t$war_lo <- pmin(q5(wd), t$war); t$war_hi <- pmax(q95(wd), t$war)
    }
    t
  })
  res <- dplyr::bind_rows(out) %>% dplyr::left_join(basis, by = "pitcher") %>%
    dplyr::mutate(basis = dplyr::coalesce(basis, "MLB"))
  today <- tibble::tibble(pitcher = ids, k = qmed(r_now$k), bbhbp = qmed(r_now$bbhbp), hr = qmed(r_now$hr),
                          hbip = qmed(r_now$hbip), era = qmed(era_now))
  attr(res, "today") <- today
  if (keep_draws) attr(res, "draws") <- lapply(kept, function(M) { rownames(M) <- as.character(ids); M })
  res
}

# Talent RA9 point estimate at the base season, used to build grade anchors
talent_point <- function(models, lines, signals, parks, league, base_season, map) {
  signals <- ensure_signal_cols(signals)
  lg <- league[league$level == "MLB" & league$season == base_season, ]
  r <- lapply(PROJ_COMPONENTS, function(cpt) {
    nd <- newdata_rows(lines, signals, parks, league, base_season, 1L, cpt)
    nd$age_target <- nd$age_target - 1; nd$target_season <- base_season
    nd$lg_logit <- stats::qlogis(lg[[paste0("lg_", cpt)]])
    tibble::tibble(pitcher = nd$pitcher, !!cpt := stats::plogis(predict_component(models[[cpt]]$h1, nd)))
  })
  t <- Reduce(function(a, b) dplyr::inner_join(a, b, by = "pitcher"), r)
  t$ra9 <- ra9_from_rates(map, t$k, t$bbhbp, t$hr, t$hbip)
  t
}

# Grade anchors: mean and SD of talent RA9 among MLB pitchers with 50+ IP
projection_anchors <- function(points, lines, seasons) {
  q <- lines[lines$level == "MLB" & lines$season %in% seasons & lines$outs >= 150, c("pitcher", "season")]
  x <- dplyr::inner_join(points, q, by = c("pitcher", "season"))
  tibble::tibble(mean = mean(x$ra9), unit = stats::sd(x$ra9))
}

PROJECTION_ANCHORS_FILE <- "projection_anchors.csv"
# Frozen anchors from models/
read_projection_anchors <- function() {
  readr::read_csv(file.path(repo_root(), "models", PROJECTION_ANCHORS_FILE), show_col_types = FALSE)
}

# Luck gap

LUCK_MIN_OUTS <- 60L  # 20 innings: below that one bad outing is the whole "gap"

# Left-on-base rate
lob_pct <- function(h, bb, hbp, r, hr) {
  den <- h + bb + hbp - 1.4 * hr
  ifelse(den > 0, (h + bb + hbp - r) / den, NA_real_)
}

# Expected strand rate from K and BB+HBP rates
fit_strand <- function(lines) {
  l <- lines[lines$level == "MLB" & lines$outs >= 150, ]
  l$lob <- lob_pct(l$h, l$bb, l$hbp, l$r, l$hr)
  l$k_rate <- l$k / l$tbf; l$bbhbp_rate <- l$bbhbp / l$tbf
  stats::lm(lob ~ k_rate + bbhbp_rate, data = l[is.finite(l$lob), ], weights = outs)
}

# Actual minus expected ERA, split into BABIP, strand, HR/FB and other
luck_gap <- function(line, expected, map, er_r, strand, fb = NULL, parks = NULL) {
  b <- stats::coef(map)
  d <- line %>% dplyr::filter(outs >= LUCK_MIN_OUTS) %>%
    dplyr::inner_join(dplyr::rename_with(expected, ~ paste0("x_", .x), -pitcher), by = "pitcher")
  if (!is.null(fb) && nrow(fb)) d <- dplyr::left_join(d, dplyr::select(fb, pitcher, fb), by = "pitcher") else d$fb <- NA_real_
  if (!is.null(parks) && nrow(parks)) {
    d <- dplyr::left_join(d, dplyr::select(parks, pitcher, ppf_runs, ppf_hr, ppf_hbip), by = "pitcher")
  } else { d$ppf_runs <- NA_real_; d$ppf_hr <- NA_real_; d$ppf_hbip <- NA_real_ }
  d %>% dplyr::mutate(
    ppf_runs = dplyr::coalesce(ppf_runs, 1), x_hr = x_hr * dplyr::coalesce(ppf_hr, 1), x_hbip = x_hbip * dplyr::coalesce(ppf_hbip, 1),
    actual_era = 27 * er / outs,
    expected_era = ra9_from_rates(map, x_k, x_bbhbp, x_hr / dplyr::coalesce(ppf_hr, 1), x_hbip / dplyr::coalesce(ppf_hbip, 1)) * ppf_runs * er_r,
    gap = actual_era - expected_era,
    babip = ifelse(bip > 0, b[["babip"]] * (hbip / bip - x_hbip) * er_r, 0),
    hr_fb = ifelse(is.na(fb), NA_real_, b[["hr_r"]] * (hr / tbf - x_hr) * er_r),
    lob_act = lob_pct(h, bb, hbp, r, hr),
    lob_exp = stats::predict(strand, newdata = data.frame(k_rate = k / tbf, bbhbp_rate = bbhbp / tbf)),
    strand = ifelse(is.na(lob_act), 0, -(lob_act - lob_exp) * (h + bb + hbp - 1.4 * hr) * 27 / outs * er_r),
    other = gap - babip - strand - dplyr::coalesce(hr_fb, 0)) %>%
    dplyr::select(pitcher, season, level, actual_era, expected_era, gap, babip, strand, hr_fb, other)
}

# Projection run

# Store keys for projection and luck outputs
projections_key <- function(season) outputs_key(sprintf("projections_%s.parquet", season))
luck_key <- function(level, season) outputs_key(sprintf("luck_%s_%s.parquet", level, season))
projection_keys <- function(season) c(projections_key(season), luck_key("MLB", season), luck_key("AAA", season))

# Read published MLB/AAA output files through a season, tagged with level and season
read_tagged_store <- function(prefix, through) {
  f <- list.files(store_path("outputs"), pattern = sprintf("^%s_(MLB|AAA)_\\d{4}\\.parquet$", prefix), full.names = TRUE)
  dplyr::bind_rows(lapply(f, function(p) {
    m <- regmatches(basename(p), regexec("_(MLB|AAA)_(\\d{4})", basename(p)))[[1]]
    if (as.integer(m[3]) > through) return(NULL)
    d <- tibble::as_tibble(arrow::read_parquet(p)); d$level <- m[2]; d$season <- as.integer(m[3]); d
  }))
}

# Load season lines, league table, signals, windows, parks and names through a season
projection_inputs <- function(through) {
  keys <- c(sprintf("raw/statsapi/pitching/MLB/%s.parquet", 2010:through),
            sprintf("raw/statsapi/pitching/AAA/%s.parquet", 2023:through))
  keys <- keys[store_exists(keys)]
  lines <- season_lines(dplyr::bind_rows(lapply(keys, store_read)))
  grades <- read_tagged_store("grades", through); comps <- read_tagged_store("components", through)
  signals <- if (nrow(grades)) pitch_signals(grades, comps) else tibble::tibble(level = character(), season = integer(), pitcher = integer())
  tk <- sprintf("raw/statsapi/transactions/%s.parquet", 2012:through); tk <- tk[store_exists(tk)]
  txn <- dplyr::bind_rows(lapply(tk, store_read))
  il <- if (nrow(txn)) il_days(dplyr::mutate(txn, il_event = classify_il(description))) else NULL
  list(lines = lines, league = league_table(lines), signals = signals, il = il, profiles = read_tagged_store("profiles", through),
       windows = read_tagged_store("windows", through), parks = read_tagged_store("parks", through),
       pitchers = read_tagged_store("pitchers", through))
}

# Drop rows after the given season
trim_inputs <- function(inp, season) {
  cut <- function(d) if (is.null(d) || !nrow(d)) d else d[d$season <= season, ]
  lapply(inp, cut)
}

# Fit, simulate and compute luck from loaded inputs
run_projection_from <- function(inp, season, anchors, n_draws = 1000L, seed = 1L) {
  t0 <- Sys.time()
  i <- trim_inputs(inp, season)
  models <- fit_projection_models(i$lines, i$signals, NULL, i$parks, i$league, horizons = 1:2)
  map <- fit_runs_map(i$lines, i$parks)
  curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) aging_curve(i$lines, cpt)), PROJ_COMPONENTS)
  ps <- pt_setup(season)
  il <- i$il %||% tibble::tibble(pitcher = integer(), season = integer(), il_days = numeric())
  pt <- fit_playing_time(pt_rows(i$lines, il, i$profiles, i$league, ps$train[ps$train >= min(i$lines$season) + 2L]))
  pt_new <- pt_rows(i$lines, il, i$profiles, i$league, ps$base, with_target = FALSE)
  war_k <- replacement_scale(realized_war_inputs(i$lines, i$parks), i$league, ps$war[ps$war >= min(i$lines$season)])
  proj <- project_pitchers(models, i$lines, i$signals, i$parks, i$league, season, map, anchors, curves, n_draws, seed,
                           pt = pt, pt_new = pt_new, war_k = war_k, pt_shift = ps$shift)
  today <- attr(proj, "today")
  names_tbl <- if (!is.null(i$pitchers) && nrow(i$pitchers)) i$pitchers %>% dplyr::arrange(dplyr::desc(season), level != "MLB") %>%
    dplyr::distinct(pitcher, .keep_all = TRUE) %>% dplyr::select(pitcher, name) else tibble::tibble(pitcher = integer(), name = character())
  proj <- dplyr::left_join(proj, names_tbl, by = "pitcher") %>% dplyr::mutate(base_season = as.integer(season)) %>%
    dplyr::relocate(pitcher, name, basis, base_season)
  lg <- i$league[i$league$level == "MLB" & i$league$season == season, ]
  strand <- fit_strand(i$lines)
  luck <- lapply(c("MLB", "AAA"), function(lv) {
    if (lv == "AAA") return(NULL)  # AAA results don't compare with MLB-equivalent talent
    line <- i$lines[i$lines$level == lv & i$lines$season == season, ]
    pk <- if (!is.null(i$parks) && nrow(i$parks)) i$parks[i$parks$level == lv & i$parks$season == season, ] else NULL
    fb <- if (!is.null(i$windows) && nrow(i$windows)) i$windows[i$windows$level == lv & i$windows$season == season &
                                                                  i$windows$window == "to_date", c("pitcher", "fb")] else NULL
    if (!nrow(line)) return(NULL)
    luck_gap(line, dplyr::select(today, pitcher, k, bbhbp, hr, hbip), map, lg$er_r, strand, fb, parks = pk)
  })
  names(luck) <- c("MLB", "AAA")
  message(sprintf("Projection %s: %d pitchers in %.1f min", season, length(unique(proj$pitcher)),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  list(projections = proj, luck = luck)
}

# Write projections and luck files, with empty luck tables where missing
write_projection_outputs <- function(res, season) {
  store_write(res$projections, projections_key(season))
  for (lv in c("MLB", "AAA")) {
    l <- res$luck[[lv]]
    if (is.null(l)) l <- tibble::tibble(pitcher = integer(), season = integer(), level = character(), actual_era = numeric(),
                                        expected_era = numeric(), gap = numeric(), babip = numeric(), strand = numeric(),
                                        hr_fb = numeric(), other = numeric())
    store_write(l, luck_key(lv, season))
  }
  invisible(projection_keys(season))
}

# Published outputs the projection reads
PROJECTION_OUTPUT_PREFIXES <- c("grades", "components", "windows", "parks", "pitchers", "profiles")

# CI: download season lines and published outputs; any failed download stops the run
fetch_projection_inputs <- function(data_repo, app_repo, app_token = NULL) {
  for (lv in c("MLB", "AAA")) release_download_dir(sprintf("raw/statsapi/pitching/%s", lv), data_repo)
  release_download_dir("raw/statsapi/transactions", data_repo)  # injured-list history for playing time
  dir.create(store_path("outputs"), recursive = TRUE, showWarnings = FALSE)
  for (pre in PROJECTION_OUTPUT_PREFIXES) {
    st <- gh_run(c("release", "download", OUTPUTS_TAG, "--repo", app_repo, "--pattern", paste0(pre, "_*"),
                   "--dir", store_path("outputs"), "--skip-existing"), token = app_token)
    if (!identical(as.integer(st), 0L)) stop("Outputs download failed for ", pre, "_* from ", app_repo)
  }
  invisible(TRUE)
}

# Stop if any base-season MLB input is missing so we never publish on partial data
check_projection_inputs <- function(season) {
  keys <- outputs_key(sprintf("%s_MLB_%s.parquet", PROJECTION_OUTPUT_PREFIXES, season))
  missing <- keys[!store_exists(keys)]
  if (length(missing)) stop("Projection inputs missing: ", paste(basename(missing), collapse = ", "))
  if (!store_exists(sprintf("raw/statsapi/transactions/%s.parquet", season - 1L))) stop("Projection inputs missing: transactions for ", season - 1L)
  TRUE
}

# Playing time

PT_MAX_OUTS <- 650  # 216.2 innings; one pitcher has topped it since 2021
PT_PERF_PAD <- 150  # outs of league-average RA9 added to the performance measure

# Primary fastball velocity and arm angle per pitcher-season (MLB row preferred)
fastball_trend <- function(profiles) {
  if (is.null(profiles) || !nrow(profiles)) return(tibble::tibble(pitcher = integer(), season = integer(), velo = numeric(), arm = numeric()))
  profiles %>% dplyr::filter(pitch_type %in% c("4S", "SNK")) %>%
    dplyr::arrange(pitcher, season, level != "MLB", dplyr::desc(n)) %>%
    dplyr::distinct(pitcher, season, .keep_all = TRUE) %>%
    dplyr::transmute(pitcher, season, velo, arm = arm_angle)
}

# One row per pitcher with a line in each base season: workload, health, trends, and next-season targets
pt_rows <- function(lines, il, profiles, league, base_seasons, with_target = TRUE) {
  if (with_target) base_seasons <- setdiff(base_seasons, 2019L)  # the 2020 season was 60 games, not a fair target
  mlb <- lines[lines$level == "MLB", ]
  short <- mlb$season == 2020L
  mlb$outs[short] <- round(mlb$outs[short] * 162 / 60); mlb$r[short] <- round(mlb$r[short] * 162 / 60)
  outs_in <- function(p, s) { o <- mlb$outs[match(paste(p, s), paste(mlb$pitcher, mlb$season))]; dplyr::coalesce(o, 0) }
  il_in <- function(p, s) { d <- il$il_days[match(paste(p, s), paste(il$pitcher, il$season))]; dplyr::coalesce(d, 0) }
  lg <- league[league$level == "MLB", ]
  fb <- fastball_trend(profiles)
  out <- lapply(base_seasons, function(y) {
    cur <- lines[lines$season == y, ] %>% dplyr::group_by(pitcher) %>%
      dplyr::summarise(age = max(age), gs_share = stats::weighted.mean(gs_share, pmax(tbf, 1)),
                       aaa_share = sum(tbf[level == "AAA"]) / max(sum(tbf), 1), .groups = "drop")
    lost <- mlb[mlb$season == y - 1 & !mlb$pitcher %in% cur$pitcher, ]  # missed the whole season
    if (nrow(lost)) cur <- dplyr::bind_rows(cur, tibble::tibble(pitcher = lost$pitcher, age = lost$age + 1,
                                                                gs_share = lost$gs_share, aaa_share = 0))
    if (!nrow(cur)) return(NULL)
    p <- cur$pitcher
    w <- c(5, 4, 3); hist <- lapply(0:2, function(k) mlb[mlb$season == y - k, c("pitcher", "outs", "r")])
    num <- den <- numeric(length(p))
    for (k in 1:3) { h <- hist[[k]][match(p, hist[[k]]$pitcher), ]; num <- num + w[k] * dplyr::coalesce(h$r, 0); den <- den + w[k] * dplyr::coalesce(h$outs, 0) }
    lg_ra9 <- lg$lg_ra9[match(y, lg$season)]
    if (is.na(lg_ra9)) lg_ra9 <- stats::median(lg$lg_ra9)
    v0 <- fb[fb$season == y, ]; v1 <- fb[fb$season == y - 1, ]
    vc <- v0$velo[match(p, v0$pitcher)] - v1$velo[match(p, v1$pitcher)]
    ac <- v0$arm[match(p, v0$pitcher)] - v1$arm[match(p, v1$pitcher)]
    r <- tibble::tibble(pitcher = p, base_season = as.integer(y),
                        outs_1 = outs_in(p, y), outs_2 = outs_in(p, y - 1), outs_3 = outs_in(p, y - 2),
                        gs_share = cur$gs_share, aaa_share = cur$aaa_share,
                        il_1 = il_in(p, y), il_3 = il_in(p, y) + il_in(p, y - 1) + il_in(p, y - 2),
                        perf_ra9 = 27 * (num + PT_PERF_PAD * lg_ra9 / 27) / (den + PT_PERF_PAD),
                        has_velo = as.numeric(!is.na(vc)), velo_chg = dplyr::coalesce(vc, 0), arm_chg = dplyr::coalesce(ac, 0),
                        age_next = cur$age + 1)
    r$max_outs3 <- pmax(r$outs_1, r$outs_2, r$outs_3)
    if (with_target) {
      nx <- mlb[mlb$season == y + 1, ]
      i <- match(p, nx$pitcher)
      r$outs_next <- dplyr::coalesce(nx$outs[i], 0)
      r$available <- as.numeric(r$outs_next >= 1)
      r$starter <- ifelse(r$available == 1, as.numeric(nx$gs_share[i] >= 0.5), NA_real_)
    }
    r
  })
  dplyr::bind_rows(out)
}

# Role, availability and innings models on pitcher-season pairs
PT_RECENCY <- 0.75  # each season back counts 25% less (innings per pitcher keep falling)

fit_playing_time <- function(rows) {
  rows <- rows[is.finite(rows$age_next), ]
  rows$w <- PT_RECENCY^(max(rows$base_season) - rows$base_season)
  played <- rows[rows$available == 1 & !is.na(rows$starter), ]
  role <- (stats::glm(starter ~ gs_share + log1p(outs_1) + age_next + perf_ra9 + aaa_share, data = played,
                                      family = stats::quasibinomial(), weights = w))
  avail <- stats::glm(available ~ log1p(outs_1) + log1p(outs_2) + il_1 + il_3 + splines::ns(age_next, 3) + perf_ra9 +
                        aaa_share + velo_chg * has_velo + gs_share, data = rows, family = stats::quasibinomial(), weights = w)
  played$starter <- as.numeric(played$starter)
  ip <- stats::lm(log(outs_next) ~ starter * (log1p(outs_1) + log1p(max_outs3) + il_1 + perf_ra9) +
                    splines::ns(age_next, 3) + aaa_share, data = played, weights = w)
  res <- stats::residuals(ip)
  sigma <- c(rp = stats::sd(res[played$starter == 0]), sp = stats::sd(res[played$starter == 1]))
  mu <- stats::fitted(ip)
  group <- function(k) {
    r <- unname(res[played$starter == k]); m <- mu[played$starter == k]
    if (length(r) < 10 * PT_BANDS) {  # too few pitchers in this role to split: one shared band
      r <- if (length(r) >= 10) r else unname(res)
      return(list(breaks = rep(Inf, PT_BANDS - 1), resid = rep(list(r), PT_BANDS)))
    }
    br <- unname(stats::quantile(m, seq(0, 1, length.out = PT_BANDS + 1))[2:PT_BANDS])
    b <- findInterval(m, br) + 1
    list(breaks = br, resid = lapply(seq_len(PT_BANDS), function(i) r[b == i]))
  }
  g_rp <- group(0); g_sp <- group(1)
  resid <- list(rp = g_rp$resid, sp = g_sp$resid)  # real misses by role and projected innings
  list(role = role, avail = avail, ip = ip, sigma = sigma, resid = resid, breaks = list(rp = g_rp$breaks, sp = g_sp$breaks))
}

PT_BANDS <- 5  # misses grouped into fifths of projected innings

# Resample a real miss from pitchers with a similar projection and the same role
draw_resid <- function(pt, sp, mu) {
  e <- numeric(length(sp))
  for (k in c("rp", "sp")) {
    i <- which((sp == 1) == (k == "sp"))
    if (!length(i)) next
    b <- findInterval(mu[i], pt$breaks[[k]]) + 1
    for (j in seq_len(PT_BANDS)) { ij <- i[b == j]; if (length(ij)) e[ij] <- sample(pt$resid[[k]][[j]], length(ij), TRUE) }
  }
  e
}

# Draws of role and MLB outs for each future season, rolling each season's draws into the next
simulate_playing_time <- function(pt, rows, n_draws, horizons = 5L, perf = NULL) {
  n <- nrow(rows)
  rep_rows <- rows[rep(seq_len(n), times = n_draws), ]   # pitcher-major within each draw column
  out <- vector("list", horizons)
  for (h in seq_len(horizons)) {
    if (!is.null(perf)) rep_rows$perf_ra9 <- as.vector(perf[[min(h, length(perf))]])  # each draw's own talent
    p_sp <- stats::predict(pt$role, rep_rows, type = "response")
    p_av <- stats::predict(pt$avail, rep_rows, type = "response")
    sp <- stats::rbinom(length(p_sp), 1, p_sp); av <- stats::rbinom(length(p_av), 1, p_av)
    nd <- rep_rows; nd$starter <- sp
    mu <- stats::predict(pt$ip, nd)
    e <- if (!is.null(pt$breaks)) draw_resid(pt, sp, mu)
         else stats::rnorm(length(mu)) * ifelse(sp == 1, pt$sigma[["sp"]], pt$sigma[["rp"]])
    outs <- ifelse(av == 1, pmin(pmax(round(exp(mu + e)), 1), PT_MAX_OUTS), 0)
    out[[h]] <- list(starter = matrix(sp, n, n_draws), outs = matrix(outs, n, n_draws))
    rep_rows$outs_3 <- rep_rows$outs_2; rep_rows$outs_2 <- rep_rows$outs_1; rep_rows$outs_1 <- outs
    rep_rows$max_outs3 <- pmax(rep_rows$outs_1, rep_rows$outs_2, rep_rows$outs_3)
    rep_rows$gs_share <- ifelse(outs > 0, sp, rep_rows$gs_share)
    rep_rows$aaa_share <- ifelse(outs > 0, 0, rep_rows$aaa_share)
    rep_rows$il_3 <- rep_rows$il_3 - rep_rows$il_1; rep_rows$il_1 <- 0
    rep_rows$age_next <- rep_rows$age_next + 1
    rep_rows$has_velo <- 0; rep_rows$velo_chg <- 0
  }
  out
}

# WAR

REPL_WIN <- c(sp = 0.38, rp = 0.47)  # replacement winning percentage by role
WAR_TOTAL <- 430  # pitcher WAR per season

runs_per_win <- function(lg_ra9) 1.5 * lg_ra9 + 3

# Wins above replacement; k scales the role offsets so the league total hits WAR_TOTAL
war_from <- function(ra9, outs, starter, lg_ra9, k) {
  rpw <- runs_per_win(lg_ra9)
  repl <- lg_ra9 + k * (0.5 - ifelse(starter == 1, REPL_WIN[["sp"]], REPL_WIN[["rp"]])) * rpw
  ifelse(outs > 0, (repl - ra9) * (outs / 3) / 9 / rpw, 0)
}

# Average scale factor that makes realized WAR total WAR_TOTAL in each calibration season
replacement_scale <- function(act, league, seasons) {
  ks <- vapply(seasons, function(s) {
    a <- act[act$season == s & act$outs > 0, ]
    lg <- league$lg_ra9[league$level == "MLB" & league$season == s]
    if (!nrow(a) || !length(lg)) return(NA_real_)
    rpw <- runs_per_win(lg)
    base <- sum((lg - a$ra9_adj) * (a$outs / 3) / 9 / rpw)
    per_k <- sum((0.5 - ifelse(a$starter == 1, REPL_WIN[["sp"]], REPL_WIN[["rp"]])) * rpw * (a$outs / 3) / 9 / rpw)
    (WAR_TOTAL - base) / per_k
  }, 0)
  mean(ks, na.rm = TRUE)
}

# Realized MLB pitcher seasons for calibrating replacement level (park-adjusted RA9, role)
realized_war_inputs <- function(lines, parks) {
  m <- lines[lines$level == "MLB" & lines$outs > 0, ]
  if (!is.null(parks) && nrow(parks)) {
    m <- dplyr::left_join(m, dplyr::select(parks[parks$level == "MLB", ], pitcher, season, ppf_runs), by = c("pitcher", "season"))
  } else m$ppf_runs <- NA_real_
  dplyr::transmute(m, pitcher, season, outs, starter = as.numeric(gs_share >= 0.5),
                   ra9_adj = 27 * r / outs / dplyr::coalesce(ppf_runs, 1))
}

# A season counts as complete once the regular season is over
season_complete <- function(season, today = getOption("pv.today", Sys.Date())) today >= as.Date(sprintf("%d-10-01", season))

# Which seasons playing time trains on, calibrates WAR on, and projects from
pt_setup <- function(season, today = getOption("pv.today", Sys.Date())) {
  done <- if (season_complete(season, today)) season else season - 1L
  list(base = as.integer(done), shift = as.integer(season - done), train = 2012L:(done - 1L), war = 2021L:done)
}

# Each draw's talent as a shift around the pitcher's own past RA9 (the scale the models learned)
perf_from_draws <- function(draws, past) past + (draws - apply(draws, 1, stats::median))
