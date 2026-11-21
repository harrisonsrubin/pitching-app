# P3a simulation (design: uncertainty, true talent grade, bet, Step odds, basis). From the
# component models: n_draws joint draws of each pitcher's true-talent rates now and 1-5
# seasons out, then RA9 (fitted runs map), park-neutral ERA and FIP, the 20-80 true
# talent grade, the 10-chip bet, and Step forward / Step back odds.
#
# One draw = fixed effects from MVN(fixef, vcov) + the pitcher's own talent (conditional
# mode + noise from its conditional variance and the sample-size uncertainty, shared by
# today and every future season of the draw) + one season-change effect per future
# season (variance of the pitcher:season term). "Today"
# is the one-year model at the current age with no change; next season adds the aging
# step and one change. Seasons 3-5 extend the two-year model with the aging curve.

STEP_RUNS <- 0.75
# Forecasting targets start where park factors do (design: MLB 2010-2019 feed the aging
# curve and past-results history only).
PROJ_MIN_TARGET <- 2020L
# Season 2: "model" = the two-year model; "step" = next season stepped forward with the
# aging curve (as seasons 3-5 are). Chosen by the P3 scorecard's two-seasons-out test.
PROJ_H2_METHOD <- "model"
CHIP_PROBS <- seq(0.05, 0.95, by = 0.1)

chips <- function(grade_draws) {
  q <- stats::quantile(grade_draws, CHIP_PROBS, names = FALSE, na.rm = TRUE)
  pmin(pmax(q, 20), 80)
}

tt_grade <- function(ra9, anchors) pmin(pmax(50 - 10 * (ra9 - anchors$mean[1]) / anchors$unit[1], 20), 80)

step_odds <- function(era_now, era_next) {
  d <- era_next - era_now
  c(forward = mean(d <= -STEP_RUNS), back = mean(d >= STEP_RUNS))
}

basis_of <- function(lines, base_season) {
  lines[lines$season == base_season, ] %>% dplyr::group_by(pitcher) %>%
    dplyr::summarise(aaa = sum(tbf[level == "AAA"]), all = sum(tbf), .groups = "drop") %>%
    dplyr::transmute(pitcher, basis = ifelse(aaa > 0.5 * all, "MLB-equivalent", "MLB"))
}

ensure_signal_cols <- function(signals) {
  for (v in c("level", SIGNAL_GRADES, SIGNAL_X)) if (!v %in% names(signals))
    signals[[v]] <- if (v == "level") character(nrow(signals)) else numeric(nrow(signals))
  signals$season <- as.integer(signals$season); signals$pitcher <- as.integer(signals$pitcher)
  signals
}

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

# Runs map: park-adjusted RA9 on the four rates, MLB pitcher-seasons with 50+ IP,
# weighted by outs.
fit_runs_map <- function(lines, parks) {
  l <- lines[lines$level == "MLB" & lines$outs >= 150 & lines$bip > 0, ]
  if (!is.null(parks) && nrow(parks)) {
    l <- dplyr::left_join(l, dplyr::select(parks, pitcher, level, season, ppf_runs), by = c("pitcher", "level", "season"))
  } else l$ppf_runs <- NA_real_
  l <- dplyr::mutate(l, ra9_adj = 27 * r / outs / dplyr::coalesce(ppf_runs, 1),
                     k_r = k / tbf, bbhbp_r = bbhbp / tbf, hr_r = hr / tbf, babip = hbip / bip)
  stats::lm(ra9_adj ~ k_r + bbhbp_r + hr_r + babip, data = l, weights = outs)
}

# Season-level RA9 error the four rates do not explain (sequencing, defense, timing): the
# runs map's residual SD per unit weight (weights = outs), scaled to a season of `outs`.
runs_noise_sd <- function(map, outs) stats::sigma(map) / sqrt(pmax(outs, 1))

ra9_from_rates <- function(map, k, bbhbp, hr, hbip) {
  b <- stats::coef(map)
  pmax(b[["(Intercept)"]] + b[["k_r"]] * k + b[["bbhbp_r"]] * bbhbp + b[["hr_r"]] * hr + b[["babip"]] * hbip, 0.5)
}

fip_from_rates <- function(k, bbhbp, hr, hbip, fip_const) {
  hits <- hr + hbip * pmax(1 - k - bbhbp - hr, 0)
  ip_pt <- pmax(1 - bbhbp - hits, 0.05) / 3
  (13 * hr + 3 * bbhbp - 2 * k) / ip_pt + fip_const
}

# Fixed part (n x n_draws) under coefficient draws B, plus the centered league offset.
fixed_draws <- function(m, nd, B) {
  X <- fixed_design(m, nd)
  X %*% t(B[, colnames(X), drop = FALSE]) + (nd$lg_logit - m$lg_center)
}

coef_draws <- function(m, n_draws) {
  b <- lme4::fixef(m$fit); V <- as.matrix(stats::vcov(m$fit))
  B <- MASS::mvrnorm(n_draws, b, V)
  if (n_draws == 1) B <- matrix(B, nrow = 1, dimnames = list(NULL, names(b)))
  B
}

# The pitcher's own talent beyond the fixed part, shared by "today" and every future
# season of the same draw. A pitcher the fit has seen: his effect's conditional mode and
# variance. One it has not (e.g. a rookie): 0, with the sample-size-dependent
# uncertainty (talent_uncertainty), never below the pitcher variance.
pitcher_draws <- function(m, nd, n_draws) {
  re <- lme4::ranef(m$fit, condVar = TRUE)$pitcher
  pv <- attr(re, "postVar")
  modes <- stats::setNames(re[[1]], rownames(re)); vars <- stats::setNames(as.vector(pv), rownames(re))
  key <- as.character(nd$pitcher)
  mu <- ifelse(key %in% names(modes), modes[key], 0)
  v <- ifelse(key %in% names(vars), vars[key], pmax(talent_uncertainty(m, nd$past_trials), pitcher_variance(m)))
  mu + sqrt(v) * matrix(stats::rnorm(nrow(nd) * n_draws), nrow(nd))
}

q5 <- function(M) apply(M, 1, stats::quantile, 0.05, names = FALSE, na.rm = TRUE)
q95 <- function(M) apply(M, 1, stats::quantile, 0.95, names = FALSE, na.rm = TRUE)
qmed <- function(M) apply(M, 1, stats::median, na.rm = TRUE)

project_pitchers <- function(models, lines, signals, parks, league, base_season, map, anchors, curves,
                             n_draws = 1000L, seed = 1L, keep_draws = FALSE, h2_method = PROJ_H2_METHOD) {
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
    g <- tt_grade(ra9, anchors); eo <- era_of(outcome(r))
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

# Point estimate of each pitcher's talent RA9 as of the base season (the one-year model at
# his current age, no change, no noise): what the frozen grade anchors are built from.
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

# 20-80 anchors for the true talent grade: mean and SD of talent RA9 among MLB pitchers
# with 50+ IP in the given base seasons (2023-2025 for the frozen file).
projection_anchors <- function(points, lines, seasons) {
  q <- lines[lines$level == "MLB" & lines$season %in% seasons & lines$outs >= 150, c("pitcher", "season")]
  x <- dplyr::inner_join(points, q, by = c("pitcher", "season"))
  tibble::tibble(mean = mean(x$ra9), unit = stats::sd(x$ra9))
}

PROJECTION_ANCHORS_FILE <- "projection_anchors.csv"
read_projection_anchors <- function() {
  readr::read_csv(file.path(repo_root(), "models", PROJECTION_ANCHORS_FILE), show_col_types = FALSE)
}
