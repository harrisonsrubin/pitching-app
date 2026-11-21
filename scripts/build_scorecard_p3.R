# Usage: Rscript scripts/build_scorecard_p3.R
# P3 scorecard (README 9): the projection, time-forward. For each base season y in
# 2021-2025 the projection is fit on data through y only and scored on y+1 (next season)
# or on the rest of season y after 100/250/500 pitches. Same rules as P1: MLB pitchers
# with 50+ IP in both seasons, weights = the smaller IP, park-adjusted RA9, linear
# benchmarks cross-validated by pitcher. Writes scorecard/p3.md (aggregates only).
source("R/load.R")
t_start <- Sys.time()
BASES <- 2021:2025
NS <- c("100", "250", "500")
MIN_IP <- 50; MIN_ROS_IP <- 30
inp <- projection_inputs(2026L)
anchors <- read_projection_anchors()
lines <- inp$lines
pp <- inp$parks %>% dplyr::filter(level == "MLB") %>% dplyr::select(pitcher, season, ppf_runs)
act <- lines %>% dplyr::filter(level == "MLB", outs > 0) %>% dplyr::left_join(pp, by = c("pitcher", "season")) %>%
  dplyr::mutate(ip = outs / 3, ra9 = 27 * r / outs, ra9_adj = ra9 / dplyr::coalesce(ppf_runs, 1), era = 27 * er / outs)
lg <- inp$league[inp$league$level == "MLB", ]
fg <- dplyr::bind_rows(lapply(2020:2026, function(s) if (store_exists(fangraphs_key(s))) store_read(fangraphs_key(s)))) %>%
  dplyr::transmute(pitcher = as.integer(mlbam_id), season = as.integer(season), pitching_plus)
marcel <- dplyr::bind_rows(lapply(BASES + 1L, function(s)
  dplyr::mutate(marcel_ra9(dplyr::select(act, pitcher, season, ip, ra9, age), s, lg$lg_ra9[lg$season == s - 1]), season = s)))

next_frames <- list(); tp <- list(); cal <- list(); aaa <- list(); two_frames <- list()
for (y in c(BASES, 2026L)) {
  i <- trim_inputs(inp, y)
  models <- fit_projection_models(i$lines, i$signals, NULL, i$parks, i$league, horizons = 1:2)
  map <- fit_runs_map(i$lines, i$parks)
  er_r <- i$league$er_r[i$league$level == "MLB" & i$league$season == y]
  tp[[as.character(y)]] <- dplyr::mutate(talent_point(models, i$lines, i$signals, i$parks, i$league, y, map),
                                         era_tt = ra9 * er_r, season = y)
  if (y == 2026L) break
  curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) aging_curve(i$lines, cpt)), PROJ_COMPONENTS)
  proj <- project_pitchers(models, i$lines, i$signals, i$parks, i$league, y, map, anchors, curves,
                           n_draws = 500L, seed = y, keep_draws = TRUE)
  h1 <- proj[proj$horizon == 1, ]
  dr <- attr(proj, "draws")
  if (y + 2L <= 2026L) {   # season 2: the two-year model vs next season stepped forward with aging
    alt <- if (PROJ_H2_METHOD == "step") "model" else "step"
    proj_alt <- project_pitchers(models, i$lines, i$signals, i$parks, i$league, y, map, anchors, curves,
                                 n_draws = 200L, seed = y, h2_method = alt)
    two <- dplyr::inner_join(
      dplyr::transmute(proj[proj$horizon == 2 & proj$basis == "MLB", ], pitcher, cur = ra9),
      dplyr::transmute(proj_alt[proj_alt$horizon == 2, ], pitcher, alt = ra9), by = "pitcher") %>%
      dplyr::inner_join(dplyr::transmute(act[act$season == y, ], pitcher, ip_now = ip), by = "pitcher") %>%
      dplyr::inner_join(dplyr::transmute(act[act$season == y + 2L, ], pitcher, ip2 = ip, ra9_2 = ra9_adj), by = "pitcher") %>%
      dplyr::filter(ip_now >= MIN_IP, ip2 >= MIN_IP) %>% dplyr::mutate(base = y)
    names(two)[names(two) == "cur"] <- PROJ_H2_METHOD; names(two)[names(two) == "alt"] <- alt
    two_frames[[as.character(y)]] <- two
  }
  now <- act[act$season == y, c("pitcher", "ip", "ra9_adj")]
  nxt <- act[act$season == y + 1, c("pitcher", "ip", "ra9_adj", "era")]
  f <- h1 %>% dplyr::select(pitcher, basis, ours_ra9 = ra9, ours_era = era, era_out_lo, era_out_hi, step_forward, step_back) %>%
    dplyr::inner_join(dplyr::rename(nxt, ip_next = ip, ra9_next = ra9_adj, era_next = era), by = "pitcher") %>%
    dplyr::left_join(dplyr::rename(now, ip_now = ip, prior_ra9 = ra9_adj), by = "pitcher") %>%
    dplyr::left_join(dplyr::filter(marcel, season == y + 1) %>% dplyr::select(pitcher, marcel), by = "pitcher") %>%
    dplyr::left_join(dplyr::filter(fg, season == y) %>% dplyr::select(pitcher, pitching_plus), by = "pitcher") %>%
    dplyr::mutate(base = y)
  f$crps <- vapply(seq_len(nrow(f)), function(k) crps_draws(dr$era_out[as.character(f$pitcher[k]), ], f$era_next[k]), 0)
  next_frames[[as.character(y)]] <- f
  message("Base ", y, " done (", round(difftime(Sys.time(), t_start, units = "mins"), 1), " min)")
}
nf <- dplyr::bind_rows(next_frames)
q <- nf[nf$basis == "MLB" & !is.na(nf$ip_now) & nf$ip_now >= MIN_IP & nf$ip_next >= MIN_IP, ]
q$w <- pmin(q$ip_now, q$ip_next)
q <- q[stats::complete.cases(q[c("marcel", "pitching_plus", "prior_ra9")]), ]
raw_rmse <- function(y, p, w) sqrt(stats::weighted.mean((y - p)^2, w))
cvr <- function(f) cv_rmse(q, f, q$w)
next_tbl <- tibble::tibble(
  metric = c("Projection (ours)", "Marcel", "Pitching+"),
  rmse = c(cvr(ra9_next ~ ours_ra9)$rmse, cvr(ra9_next ~ marcel)$rmse, cvr(ra9_next ~ pitching_plus)$rmse),
  pearson = c(cvr(ra9_next ~ ours_ra9)$pearson, cvr(ra9_next ~ marcel)$pearson, cvr(ra9_next ~ pitching_plus)$pearson))
era_tbl <- tibble::tibble(
  metric = c("Projection (ours)", "Marcel", "Pitching+"),
  rmse = c(cv_rmse(q, era_next ~ ours_era, q$w)$rmse, cv_rmse(q, era_next ~ marcel, q$w)$rmse, cv_rmse(q, era_next ~ pitching_plus, q$w)$rmse))
base_prior <- cvr(ra9_next ~ prior_ra9)$rmse
incr <- tibble::tibble(metric = c("Projection (ours)", "Pitching+"),
                       delta = c(cvr(ra9_next ~ prior_ra9 + ours_ra9)$rmse, cvr(ra9_next ~ prior_ra9 + pitching_plus)$rmse) - base_prior)

# Two seasons out: which season-2 method predicts better (raw, weighted RMSE)
tw <- dplyr::bind_rows(two_frames); tw$w <- pmin(tw$ip_now, tw$ip2)
two_tbl <- tibble::tibble(method = c("model", "step"),
                          rmse = c(raw_rmse(tw$ra9_2, tw$model, tw$w), raw_rmse(tw$ra9_2, tw$step, tw$w)),
                          cv_rmse = c(cv_rmse(tw, ra9_2 ~ model, tw$w)$rmse, cv_rmse(tw, ra9_2 ~ step, tw$w)$rmse))

# Ranges and odds
cov_all <- coverage(nf$era_out_lo, nf$era_out_hi, nf$era_next[TRUE])
cov_q <- coverage(q$era_out_lo, q$era_out_hi, q$era_next)
crps_q <- stats::weighted.mean(q$crps, q$w)
tps <- dplyr::bind_rows(tp)
real <- dplyr::inner_join(dplyr::select(tps, pitcher, season, era_now = era_tt),
                          dplyr::transmute(tps, pitcher, season = season - 1L, era_next_tt = era_tt), by = c("pitcher", "season"))
st <- dplyr::inner_join(dplyr::select(nf, pitcher, season = base, step_forward, step_back), real, by = c("pitcher", "season"))
cal_f <- step_calibration(st$step_forward, st$era_next_tt - st$era_now <= -0.75)
cal_b <- step_calibration(st$step_back, st$era_next_tt - st$era_now >= 0.75)

# Triple-A translation: mostly-Triple-A pitchers who pitched 30+ MLB innings the next season
aq <- nf[nf$basis == "MLB-equivalent" & nf$ip_next >= 30, ]
aaa_tbl <- if (nrow(aq)) tibble::tibble(n = nrow(aq), bias = stats::weighted.mean(aq$ra9_next - aq$ours_ra9, aq$ip_next),
                                        rmse = raw_rmse(aq$ra9_next, aq$ours_ra9, aq$ip_next),
                                        league_rmse = raw_rmse(aq$ra9_next, lg$lg_ra9[match(aq$base + 1L, lg$season)], aq$ip_next),
                                        coverage = coverage(aq$era_out_lo, aq$era_out_hi, aq$era_next)) else NULL

# Rest of season after 100/250/500 pitches: rest-of-season model fit on earlier seasons only
ros_frames <- list()
for (y in BASES) {
  ip <- trim_inputs(inp, y - 1L)
  iy <- trim_inputs(inp, y)
  w_train <- ip$windows; w_score <- iy$windows[iy$windows$season == y, ]
  map <- fit_runs_map(ip$lines, ip$parks)
  preds <- lapply(PROJ_COMPONENTS, function(cpt) {
    m <- fit_component(training_rows(ip$lines, ip$signals, w_train, ip$parks, ip$league, 0L, cpt), cpt)
    r <- training_rows(iy$lines, iy$signals, w_score, iy$parks, iy$league, 0L, cpt)
    r <- r[r$base_season == y, ]
    tibble::tibble(pitcher = r$pitcher, window = r$window, !!cpt := stats::plogis(predict_component(m, r)))
  })
  pr <- Reduce(function(a, b) dplyr::inner_join(a, b, by = c("pitcher", "window")), preds)
  pr$ours <- ra9_from_rates(map, pr$k, pr$bbhbp, pr$hr, pr$hbip)
  ws <- w_score[w_score$level == "MLB", ]
  full <- ws[ws$window == "to_date", c("pitcher", "runs", "outs")]
  part <- ws[ws$window %in% NS, c("pitcher", "window", "runs", "outs")]
  ros <- dplyr::inner_join(part, full, by = "pitcher", suffix = c("_w", "_all")) %>%
    dplyr::mutate(ros_outs = outs_all - outs_w, ros_ip = ros_outs / 3, window_ra9 = 27 * runs_w / pmax(outs_w, 1)) %>%
    dplyr::left_join(dplyr::filter(pp, season == y) %>% dplyr::select(pitcher, ppf_runs), by = "pitcher") %>%
    dplyr::mutate(ros_ra9 = 27 * (runs_all - runs_w) / pmax(ros_outs, 1) / dplyr::coalesce(ppf_runs, 1)) %>%
    dplyr::left_join(dplyr::filter(marcel, season == y) %>% dplyr::select(pitcher, marcel), by = "pitcher") %>%
    dplyr::mutate(marcel = dplyr::coalesce(marcel, lg$lg_ra9[lg$season == y - 1])) %>%
    dplyr::inner_join(dplyr::select(pr, pitcher, window, ours), by = c("pitcher", "window")) %>%
    dplyr::filter(ros_ip >= MIN_ROS_IP) %>% dplyr::mutate(season = y)
  ros_frames[[as.character(y)]] <- ros
}
if (!length(ros_frames) || all(vapply(ros_frames, nrow, 1L) == 0)) stop("No rest-of-season rows: build windows first")
ros_all <- dplyr::bind_rows(ros_frames)
ros_tbl <- dplyr::bind_rows(lapply(NS, function(n) {
  d <- ros_all[ros_all$window == n, ]
  tibble::tibble(pitches = n, n = nrow(d),
                 ours = cv_rmse(d, ros_ra9 ~ ours, d$ros_ip)$rmse,
                 marcel_window = cv_rmse(d, ros_ra9 ~ marcel + window_ra9, d$ros_ip)$rmse)
}))

# ---- write -------------------------------------------------------------------------
pf <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
g <- function(t, m, col = "rmse") t[[col]][t$metric == m]
crit <- c(
  sprintf("%s — Projection beats Marcel on next-season RA9 RMSE (%.3f vs %.3f)", pf(g(next_tbl, "Projection (ours)") < g(next_tbl, "Marcel")),
          g(next_tbl, "Projection (ours)"), g(next_tbl, "Marcel")),
  sprintf("%s — Projection beats Pitching+ on next-season RA9 RMSE (%.3f vs %.3f)", pf(g(next_tbl, "Projection (ours)") < g(next_tbl, "Pitching+")),
          g(next_tbl, "Projection (ours)"), g(next_tbl, "Pitching+")),
  sprintf("%s — Projection adds more than Pitching+ on top of prior RA9 (Δ RMSE %.4f vs %.4f)",
          pf(g(incr, "Projection (ours)", "delta") < g(incr, "Pitching+", "delta")), g(incr, "Projection (ours)", "delta"), g(incr, "Pitching+", "delta")),
  vapply(seq_len(nrow(ros_tbl)), function(k) sprintf("%s — Projection beats Marcel + window RA9 on rest-of-season RA9 RMSE after %s pitches (%.3f vs %.3f)",
          pf(ros_tbl$ours[k] < ros_tbl$marcel_window[k]), ros_tbl$pitches[k], ros_tbl$ours[k], ros_tbl$marcel_window[k]), ""),
  sprintf("%s — 90%% season ranges cover 85–95%% of next-season ERAs (%.1f%%, qualified pitchers)", pf(cov_q >= 0.85 && cov_q <= 0.95), 100 * cov_q))
md_tbl <- function(t, digits = 3) {
  fmt <- function(x) if (is.numeric(x)) formatC(x, digits = digits, format = "f") else as.character(x)
  c(paste0("| ", paste(names(t), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(t))),
    apply(as.data.frame(lapply(t, fmt)), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
out <- c("# P3 scorecard: projection", "",
         sprintf("Generated %s. Base seasons %d–%d, each fit on data through that season only and scored on the next season (or the rest of that season).",
                 format(Sys.Date()), min(BASES), max(BASES)),
         sprintf("Next season: MLB pitchers with %d+ IP in both seasons, weighted by the smaller (n = %s). RA9 is park-adjusted; benchmarks pass through a linear fit cross-validated by pitcher, and so does the projection.",
                 MIN_IP, format(nrow(q), big.mark = ",")),
         "", "## Success criteria", "", paste0("- ", crit), "",
         "## Next-season park-adjusted RA9", "", md_tbl(next_tbl), "",
         "## Next-season ERA", "", md_tbl(era_tbl), "",
         sprintf("## Two seasons out (park-adjusted RA9, raw projection; n = %s; in use: %s)", format(nrow(tw), big.mark = ","), PROJ_H2_METHOD), "",
         "\"model\" = the two-year model; \"step\" = next season's projection stepped forward with the aging curve.", "", md_tbl(two_tbl), "",
         sprintf("## Incremental value over prior RA9 (prior alone: RMSE %.3f)", base_prior), "", md_tbl(incr, 4), "",
         "## Rest of season (park-adjusted RA9, 30+ IP after the window)", "", md_tbl(ros_tbl), "",
         "## Ranges", "",
         sprintf("- 90%% season ranges contain %.1f%% of next-season ERAs for qualified pitchers and %.1f%% for every pitcher projected.", 100 * cov_q, 100 * cov_all),
         sprintf("- Mean CRPS of the next-season ERA distribution (qualified, weighted): %.3f.", crps_q), "",
         "## Step forward / Step back calibration", "",
         "Realized = the next season's talent estimate (fit through that season) moved at least 0.75 ERA from this season's.", "",
         "Step forward:", "", md_tbl(dplyr::mutate(cal_f, bin = as.character(bin))), "",
         "Step back:", "", md_tbl(dplyr::mutate(cal_b, bin = as.character(bin))), "",
         "## Triple-A translation", "",
         if (is.null(aaa_tbl)) "No mostly-Triple-A pitcher reached 30 MLB innings the next season." else
           c("Mostly-Triple-A pitchers who threw 30+ MLB innings the next season (RA9, weighted by MLB IP):", "", md_tbl(aaa_tbl)),
         "", "## What is and is not held out", "",
         "The projection never sees the season it is scored on. The pitch models and grade anchors are fixed P1 models trained on MLB 2023–2026; each pitcher's pitches are scored by a model that never saw him, but the league-level fit has seen the scored seasons. That overlap is in the pitch-quality inputs, not in any pitcher's results.")
writeLines(out, file.path(repo_root(), "scorecard", "p3.md"))
message("Wrote scorecard/p3.md in ", round(difftime(Sys.time(), t_start, units = "mins"), 1), " min")
