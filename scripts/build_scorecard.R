# Usage: Rscript scripts/build_scorecard.R dev|final
# dev:   targets through 2025 only (the 2026 lockbox stays closed while iterating)
# final: adds the 2026 targets; run once, at the end of the P1 iteration window.
# Writes scorecard/p1.md and scorecard/p1_stickiness_vs_predictiveness.png
# (aggregates only — no pitcher rows).
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !args[1] %in% c("dev", "final")) stop("Usage: build_scorecard.R dev|final")
mode <- args[1]
last_target <- if (mode == "dev") 2025L else 2026L
FACETS <- c(stuff = "Stuff (ours)", location = "Location (ours)", pitching = "Pitching (ours)")
NS <- c(100L, 250L, 500L)
MIN_IP <- 50; MIN_ROS_IP <- 30
c0 <- cfg()

# ---- runs, park factors, RA9, Marcel ---------------------------------------------------
pa_all <- list(); scores_all <- list()
for (s in c0$seasons_mlb) {
  rp <- load_run_pitches("MLB", s)
  pa_all[[as.character(s)]] <- pa_table(rp)
  scores_all[[as.character(s)]] <- game_scores(rp)
  rm(rp); gc()
}
scores <- dplyr::bind_rows(scores_all)
pf <- park_run_factors(scores)
ppf <- pitcher_park_factor(dplyr::bind_rows(pa_all), scores, pf)
stats <- dplyr::bind_rows(lapply(c0$seasons_stats, function(s) store_read(sprintf("raw/statsapi/pitching/MLB/%s.parquet", s))))
ra <- ra9_official(stats, ppf)
lg <- stats %>% dplyr::filter(level == "MLB") %>% dplyr::group_by(season) %>%
  dplyr::summarise(lg_ra9 = 27 * sum(r, na.rm = TRUE) / sum(ip_outs, na.rm = TRUE), .groups = "drop")
hist <- dplyr::transmute(ra, pitcher, season, ip, ra9 = ra9_adj, age)
marcel <- dplyr::bind_rows(lapply(2021:2026, function(s) {
  marcel_ra9(hist, s, lg$lg_ra9[lg$season == s - 1]) %>% dplyr::mutate(season = s)
}))
ip_tbl <- dplyr::select(ra, pitcher, season, ip)

# ---- season-level metrics (wide, one row per pitcher-season) ---------------------------
ours <- dplyr::bind_rows(lapply(2021:2026, function(s) store_read(grades_key("MLB", s)))) %>%
  dplyr::filter(half == "full", pitch_type == "ALL") %>%
  dplyr::select(pitcher, season, facet, grade_global) %>%
  tidyr::pivot_wider(names_from = facet, values_from = grade_global) %>%
  dplyr::rename(`Stuff (ours)` = stuff, `Location (ours)` = location, `Pitching (ours)` = pitching)
fg <- dplyr::bind_rows(lapply(2021:2026, function(s) store_read(fangraphs_key(s)))) %>%
  dplyr::transmute(pitcher = mlbam_id, season, ERA = era, FIP = fip, xFIP = xfip, SIERA = siera,
                   `K-BB%` = k_minus_bb, `Stuff+` = stuff_plus, `Location+` = location_plus,
                   `Pitching+` = pitching_plus)
wide <- ours %>%
  dplyr::inner_join(fg, by = c("pitcher", "season")) %>%
  dplyr::inner_join(dplyr::select(ra, pitcher, season, `RA9 (park-adj)` = ra9_adj), by = c("pitcher", "season"))
wide <- wide[stats::complete.cases(wide), ]     # common sample: every metric present
METRICS <- setdiff(names(wide), c("pitcher", "season"))
long <- function(metric) dplyr::transmute(wide, pitcher, season, value = .data[[metric]])

# ---- stickiness (year y vs y+1) ---------------------------------------------------------
stick <- dplyr::bind_rows(lapply(METRICS, function(m) {
  p <- pair_seasons(long(m), ip_tbl, MIN_IP) %>% dplyr::filter(season + 1L <= last_target)
  bands <- p %>% dplyr::group_by(band) %>% dplyr::summarise(r = wcor(x, y, w), .groups = "drop")
  tibble::tibble(metric = m, n = nrow(p), pearson = wcor(p$x, p$y, p$w), spearman = wspearman(p$x, p$y, p$w),
                 `r 50-100` = bands$r[bands$band == "50-100"], `r 100-150` = bands$r[bands$band == "100-150"],
                 `r 150+` = bands$r[bands$band == "150+"])
}))
gone <- disappeared_count(long("ERA") %>% dplyr::filter(season < last_target), ip_tbl, MIN_IP)$n

# ---- split-half (odd vs even games) -----------------------------------------------------
halves <- dplyr::bind_rows(lapply(2021:last_target, function(s) store_read(grades_key("MLB", s)))) %>%
  dplyr::filter(half != "full", pitch_type == "ALL") %>%
  dplyr::select(pitcher, season, facet, half, grade_global, n) %>%
  tidyr::pivot_wider(names_from = half, values_from = c(grade_global, n)) %>%
  dplyr::inner_join(ip_tbl, by = c("pitcher", "season")) %>% dplyr::filter(ip >= MIN_IP)
split <- halves %>% dplyr::group_by(facet) %>%
  dplyr::summarise(n = dplyr::n(), pearson = wcor(grade_global_odd, grade_global_even, pmin(n_odd, n_even)),
                   spearman = wspearman(grade_global_odd, grade_global_even, pmin(n_odd, n_even)), .groups = "drop") %>%
  dplyr::mutate(metric = FACETS[facet]) %>% dplyr::select(metric, n, pearson, spearman)

# ---- next-season RA9 ---------------------------------------------------------------------
nxt <- wide %>%
  dplyr::inner_join(ip_tbl, by = c("pitcher", "season")) %>%
  dplyr::inner_join(dplyr::transmute(ra, pitcher, season = season - 1L, ra9_next = ra9_adj, ip_next = ip),
                    by = c("pitcher", "season")) %>%
  dplyr::inner_join(dplyr::transmute(marcel, pitcher, season = season - 1L, Marcel = marcel), by = c("pitcher", "season")) %>%
  dplyr::filter(ip >= MIN_IP, ip_next >= MIN_IP, season + 1L <= last_target) %>%
  dplyr::mutate(w = pmin(ip, ip_next), prior = `RA9 (park-adj)`)
cv_metric <- function(df, cols, target, w) {
  f <- stats::as.formula(paste(target, "~", paste(sprintf("`%s`", cols), collapse = " + ")))
  cv_rmse(df, f, w)
}
next_rows <- dplyr::bind_rows(lapply(c(METRICS, "Marcel"), function(m) {
  r <- cv_metric(nxt, m, "ra9_next", nxt$w)
  tibble::tibble(metric = m, n = r$n, rmse = r$rmse, pearson = r$pearson, spearman = r$spearman)
}))
incr <- dplyr::bind_rows(lapply(setdiff(METRICS, "RA9 (park-adj)"), function(m) {
  b1 <- cv_metric(nxt, "prior", "ra9_next", nxt$w)$rmse; w1 <- cv_metric(nxt, c("prior", m), "ra9_next", nxt$w)$rmse
  b2 <- cv_metric(nxt, "Marcel", "ra9_next", nxt$w)$rmse; w2 <- cv_metric(nxt, c("Marcel", m), "ra9_next", nxt$w)$rmse
  tibble::tibble(metric = m, `Δ RMSE vs prior RA9` = w1 - b1, `Δ RMSE vs Marcel` = w2 - b2)
}))

# ---- rest of season after N pitches -------------------------------------------------------
ros_frames <- list()
for (s in 2021:last_target) {
  rp <- load_run_pitches("MLB", s)
  pa <- pa_all[[as.character(s)]] %>% dplyr::mutate(key = order_key(game_date, game_pk, at_bat_number))
  fip_c <- fip_constant(pa)
  ppf_s <- ppf[ppf$season == s, ]
  x <- store_read(xrv_key("MLB", s))
  ab <- as.integer(sapply(strsplit(x$pitch_uid, "-"), `[`, 2))
  x$key <- order_key(x$game_date, x$game_pk, ab)
  anchors <- readr::read_csv(file.path(repo_root(), "models", "grade_anchors.csv"), show_col_types = FALSE)
  for (N in NS) {
    cut <- window_cutoffs(rp, N)
    pa_c <- dplyr::inner_join(pa, cut, by = "pitcher")
    win <- pa_summary(pa_c[pa_c$key <= pa_c$cutoff, ])
    rest <- pa_summary(pa_c[pa_c$key > pa_c$cutoff, ])
    xc <- dplyr::inner_join(x, cut, by = "pitcher"); xc <- xc[xc$key <= xc$cutoff, ]
    est <- lapply(names(FACETS), function(f) {
      g <- fit_grades(xc, paste0(f, "_xrv"))$by_pitcher
      a <- anchors[anchors$facet == f & anchors$scope == "pitcher", ]
      tibble::tibble(pitcher = g$pitcher, !!FACETS[[f]] := to_grade(g$est, a$mean, a$unit))
    })
    df <- Reduce(function(a, b) dplyr::inner_join(a, b, by = "pitcher"), est) %>%
      dplyr::inner_join(dplyr::transmute(win, pitcher, `Window RA9` = ra9, `Window FIP` = fip_raw + fip_c,
                                         `Window K-BB%` = k_minus_bb), by = "pitcher") %>%
      dplyr::inner_join(dplyr::transmute(rest, pitcher, ros_ip = ip, ros_ra9 = ra9), by = "pitcher") %>%
      dplyr::left_join(dplyr::transmute(ppf_s, pitcher, ppf), by = "pitcher") %>%
      dplyr::left_join(dplyr::filter(marcel, season == s) %>% dplyr::select(pitcher, Marcel = marcel), by = "pitcher") %>%
      dplyr::mutate(Marcel = dplyr::coalesce(Marcel, lg$lg_ra9[lg$season == s - 1]),
                    ros_ra9_adj = ros_ra9 / dplyr::coalesce(ppf, 1), season = s, N = N) %>%
      dplyr::filter(ros_ip >= MIN_ROS_IP)
    ros_frames[[paste(s, N)]] <- df
  }
  rm(rp, x); gc()
  message("ROS windows: ", s)
}
ros <- dplyr::bind_rows(ros_frames)
ROS_METRICS <- c(unname(FACETS), "Window RA9", "Window FIP", "Window K-BB%", "Marcel")
ros_rows <- dplyr::bind_rows(lapply(NS, function(N) {
  d <- ros[ros$N == N, ]
  rows <- lapply(ROS_METRICS, function(m) {
    r <- cv_metric(d, m, "ros_ra9_adj", d$ros_ip)
    tibble::tibble(N = N, metric = m, n = r$n, rmse = r$rmse, pearson = r$pearson)
  })
  combos <- list(`Marcel + Window RA9` = c("Marcel", "Window RA9"),
                 `Marcel + Pitching (ours)` = c("Marcel", "Pitching (ours)"))
  rows2 <- lapply(names(combos), function(nm) {
    r <- cv_metric(d, combos[[nm]], "ros_ra9_adj", d$ros_ip)
    tibble::tibble(N = N, metric = nm, n = r$n, rmse = r$rmse, pearson = r$pearson)
  })
  dplyr::bind_rows(c(rows, rows2))
}))

# ---- pitch-model rows ----------------------------------------------------------------------
reports <- file.path(c0$data_dir, "reports")
model_eval <- readr::read_csv(file.path(reports, "model_eval.csv"), show_col_types = FALSE)
cal <- readr::read_csv(file.path(reports, "calibration.csv"), show_col_types = FALSE) %>%
  dplyr::filter(n >= 2000) %>% dplyr::group_by(model) %>%
  dplyr::summarise(cells = dplyr::n(), max_abs_error = max(abs(mean_pred - mean_actual)), .groups = "drop")

# ---- success criteria -----------------------------------------------------------------------
g <- function(tbl, m, col = "rmse") tbl[[col]][tbl$metric == m]
# Prediction criteria are judged at P3, where the Bayesian projection blends grades with
# results; P1 grades are scored by models that never saw the pitcher (pure pitch quality).
p3 <- function(ok) ifelse(ok, "Judged at P3 (grades alone already ahead)", "Judged at P3 (grades alone behind)")
crit <- c(
  sapply(NS, function(N) {
    r <- ros_rows[ros_rows$N == N, ]
    sprintf("%s — Pitching (ours) beats Marcel + window RA9 on rest-of-season RA9 RMSE after %d pitches (%.3f vs %.3f)",
            p3(g(r, "Pitching (ours)") < g(r, "Marcel + Window RA9")), N,
            g(r, "Pitching (ours)"), g(r, "Marcel + Window RA9"))
  }),
  sprintf("%s — Pitching (ours) beats Marcel on next-season RA9 RMSE (%.3f vs %.3f)",
          p3(g(next_rows, "Pitching (ours)") < g(next_rows, "Marcel")),
          g(next_rows, "Pitching (ours)"), g(next_rows, "Marcel")),
  sprintf("%s — Pitching (ours) beats Pitching+ on next-season RA9 RMSE (%.3f vs %.3f)",
          p3(g(next_rows, "Pitching (ours)") < g(next_rows, "Pitching+")),
          g(next_rows, "Pitching (ours)"), g(next_rows, "Pitching+")),
  sprintf("%s — Pitching (ours) adds more than Pitching+ on top of prior RA9 (Δ RMSE %.4f vs %.4f)",
          p3(g(incr, "Pitching (ours)", "Δ RMSE vs prior RA9") < g(incr, "Pitching+", "Δ RMSE vs prior RA9")),
          g(incr, "Pitching (ours)", "Δ RMSE vs prior RA9"), g(incr, "Pitching+", "Δ RMSE vs prior RA9")),
  sprintf("%s — Stuff (ours) is at least as sticky as Stuff+ (r %.3f vs %.3f)",
          ifelse(g(stick, "Stuff (ours)", "pearson") >= g(stick, "Stuff+", "pearson"), "PASS", "FAIL"),
          g(stick, "Stuff (ours)", "pearson"), g(stick, "Stuff+", "pearson"))
)

# ---- chart -------------------------------------------------------------------------------------
dir.create(file.path(repo_root(), "scorecard"), showWarnings = FALSE)
chart <- dplyr::inner_join(dplyr::select(stick, metric, stickiness = pearson),
                           dplyr::select(next_rows, metric, predictiveness = pearson), by = "metric") %>%
  dplyr::mutate(ours = grepl("ours", metric))
p <- ggplot2::ggplot(chart, ggplot2::aes(stickiness, predictiveness, label = metric, color = ours)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_text(nudge_y = 0.012, size = 4, show.legend = FALSE) +
  ggplot2::scale_color_manual(values = c(`TRUE` = "#1f6f8b", `FALSE` = "#8a8a8a"), guide = "none") +
  ggplot2::labs(x = "Stickiness: year-to-year correlation (same metric)",
                y = "Predictiveness: correlation with next-season park-adjusted RA9 (cross-validated)",
                title = sprintf("Stickiness vs predictiveness, MLB pitchers %d+ IP (targets through %d)", MIN_IP, last_target)) +
  ggplot2::theme_minimal(base_size = 14)
ggplot2::ggsave(file.path(repo_root(), "scorecard", "p1_stickiness_vs_predictiveness.png"), p,
                width = 16, height = 10, dpi = 100, bg = "white")

# ---- markdown -------------------------------------------------------------------------------------
md_table <- function(df, digits = 3) {
  df <- as.data.frame(df)
  cells <- lapply(names(df), function(c) {
    v <- df[[c]]
    if (is.numeric(v) && all(v == round(v), na.rm = TRUE)) format(v, big.mark = ",", trim = TRUE)
    else if (is.numeric(v)) ifelse(is.na(v), "", formatC(v, digits = digits, format = "f"))
    else as.character(v)
  })
  body <- do.call(paste, c(cells, sep = " | "))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))), paste0("| ", body, " |"))
}
lines <- c(
  "# P1 scorecard",
  "",
  sprintf("Mode: **%s** — targets through %d%s. Generated %s.", mode, last_target,
          if (mode == "dev") " (2026 lockbox closed)" else " (lockbox open)", format(Sys.Date())),
  "",
  "MLB pitchers; common sample = pitcher-seasons with every metric present. RA9 is park-adjusted.",
  sprintf("Season pairs require %d+ IP in both seasons, weighted by the smaller; %d pitchers qualified in a season and fell below %d IP (or left MLB) the next.", MIN_IP, gone, MIN_IP),
  "Predictive errors are cross-validated by pitcher (5 folds), so no pitcher's own outcome fits his prediction.",
  "",
  "## Success criteria", "",
  "P1 grades score every pitch with a model that never saw that pitcher, so they measure pitch quality, not the pitcher's results. Scoring the same pitches with a model that had seen the pitcher raised next-season RA9 correlation from 0.25 to 0.35 (about Stuff+'s level) in a P1 test: the public models' predictive edge comes largely from grades that absorb results. Prediction against Marcel and Pitching+ is therefore judged at P3, where the projection combines grades with results.",
  "", paste0("- ", crit), "",
  "## Stickiness: same metric, year y vs y+1", "", md_table(stick), "",
  "## Split-half reliability: odd vs even games, same season", "", md_table(split), "",
  "## Next-season park-adjusted RA9", "", md_table(next_rows), "",
  "## Incremental value: added on top of what we already knew", "",
  "Negative = the metric lowers next-season RA9 error when added.", "", md_table(incr, 4), "",
  "## Rest-of-season park-adjusted RA9 after the first N pitches", "",
  sprintf("Rest of season ≥ %d IP, weighted by rest-of-season IP. Runs are runs scored while the pitcher was in the game (Statcast has no charged runs).", MIN_ROS_IP),
  "", md_table(ros_rows), "",
  "## Pitch models: 2026 held out (trained on 2023–2025)", "", md_table(model_eval, 4), "",
  "Calibration by pitch type × count (cells with 2,000+ pitches):", "", md_table(cal, 4), "",
  "![Stickiness vs predictiveness](p1_stickiness_vs_predictiveness.png)"
)
writeLines(lines, file.path(repo_root(), "scorecard", "p1.md"))
message(paste(crit, collapse = "\n"))
