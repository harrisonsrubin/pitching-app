# Pipeline entry point: Rscript run.R <command> [arguments]
source("R/load.R")

# Usage: Rscript run.R backfill_statcast <MLB|AAA> <first_season> <last_season>
cmd_backfill_statcast <- function(args) {
  if (length(args) != 3) stop("Usage: Rscript scripts/backfill_statcast.R <MLB|AAA> <first_season> <last_season>")
  level <- args[1]
  seasons <- seq(as.integer(args[2]), as.integer(args[3]))
  log <- backfill_statcast(level, seasons)
  print(dplyr::count(log, status))
  for (s in seasons) message("Compacted: ", compact_season(level, s))
  if (backfill_failed(log)) {
    message("Some days failed; see the log in ", file.path(cfg()$data_dir, "logs"))
    quit(status = 1)
  }
}

# Usage: Rscript run.R backfill_statsapi [schedules] [parks] [stats] [transactions] [current]
cmd_backfill_statsapi <- function(args) {
  stages <- args
  if (length(stages) == 0) stages <- c("schedules", "parks", "stats", "transactions")
  c0 <- cfg()
  current <- as.integer(format(Sys.Date(), "%Y"))
  if ("current" %in% stages) {
    stages <- c(stages, "stats", "transactions")
    c0$seasons_stats <- current
    c0$seasons_aaa <- current
  }
  levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)

  if ("schedules" %in% stages || "parks" %in% stages) {
    schedules <- dplyr::bind_rows(lapply(names(levels_seasons), function(lv) {
      dplyr::bind_rows(lapply(levels_seasons[[lv]], function(s) {
        message("Schedule ", lv, " ", s)
        fetch_schedule(s, lv)
      }))
    }))
    message("Finished games: ", nrow(schedules))
  }

  if ("parks" %in% stages) {
    venues <- fetch_venues(sort(unique(schedules$venue_id)))
    parks <- build_parks(schedules, venues, dplyr::select(read_park_overrides(), -dplyr::any_of("note"))) %>%
      fill_missing_elevation()
    readr::write_csv(parks, file.path(repo_root(), "reference", "parks.csv"), na = "")
    message("Wrote reference/parks.csv: ", nrow(parks), " venues")
  }

  if ("stats" %in% stages) {
    stat_seasons <- list(MLB = c0$seasons_stats, AAA = c0$seasons_aaa)
    for (lv in names(stat_seasons)) for (s in stat_seasons[[lv]]) {
      key <- sprintf("raw/statsapi/pitching/%s/%s.parquet", lv, s)
      if (!needs_fetch(key, s, current)) next
      message("Pitching stats ", lv, " ", s)
      store_write(fetch_pitching_stats(s, lv), key)
    }
  }

  if ("transactions" %in% stages) {
    for (y in c0$seasons_stats) {
      key <- sprintf("raw/statsapi/transactions/%s.parquet", y)
      if (!needs_fetch(key, y, current)) next
      message("Transactions ", y)
      store_write(fetch_transactions(as.Date(sprintf("%s-01-01", y)), as.Date(sprintf("%s-12-31", y))), key)
    }
  }
}

# Usage: Rscript run.R backfill_weather
cmd_backfill_weather <- function(args) {
  c0 <- cfg()
  parks <- readr::read_csv(file.path(repo_root(), "reference", "parks.csv"), show_col_types = FALSE)
  levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)
  for (lv in names(levels_seasons)) for (s in levels_seasons[[lv]]) {
    key <- game_weather_key(lv, s)
    if (store_exists(key)) next
    message("Weather ", lv, " ", s)
    w <- build_game_weather(fetch_schedule(s, lv), parks)
    message("  games: ", nrow(w), "  missing temp: ", sum(is.na(w$temp_c)))
    store_write(w, key)
  }
}

# Usage: Rscript run.R build_features <MLB|AAA> <first_season> <last_season>
cmd_build_features <- function(args) {
  if (length(args) != 3) stop("Usage: Rscript scripts/build_features.R <MLB|AAA> <first> <last>")
  refs <- read_refs()
  for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
    d <- build_features_season(args[1], s, refs)
    store_write(d, features_key(args[1], s))
    message(args[1], " ", s, ": ", nrow(d), " pitches with features")
    rm(d); gc()
  }
}

# Usage: Rscript run.R build_grades
cmd_build_grades <- function(args) {
  FACETS <- c("stuff", "location", "pitching")
  c0 <- cfg()

  ref_fits <- list()
  for (s in c0$seasons_ref) {
    x <- store_read(xrv_key("MLB", s))
    for (f in FACETS) ref_fits[[f]][[as.character(s)]] <- fit_grades(x, paste0(f, "_xrv"))
    message("anchor fits: MLB ", s)
  }
  anchors <- dplyr::bind_rows(lapply(FACETS, function(f) grade_anchors(ref_fits[[f]], f)))
  readr::write_csv(anchors, file.path(repo_root(), "models", "grade_anchors.csv"))

  levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)
  for (lv in names(levels_seasons)) for (s in levels_seasons[[lv]]) {
    x <- store_read(xrv_key(lv, s))
    fits <- if (lv == "MLB" && s %in% c0$seasons_ref) lapply(ref_fits, `[[`, as.character(s)) else NULL
    g <- grade_season(x, lv, s, anchors, fits)
    store_write(g$grades, grades_key(lv, s))
    store_write(g$components, components_key(lv, s))
    message(lv, " ", s, ": ", sum(g$grades$half == "full" & g$grades$pitch_type == "ALL") / length(FACETS), " pitchers graded")
    rm(x, g); gc()
  }
}

# Usage: Rscript run.R build_outputs <MLB|AAA> <first_season> <last_season>
cmd_build_outputs <- function(args) {
  if (length(args) != 3) stop("Usage: Rscript scripts/build_outputs.R <MLB|AAA> <first> <last>")
  anchors <- read_anchors()
  for (s in seq(as.integer(args[2]), as.integer(args[3]))) { build_outputs(args[1], s, anchors); gc() }
}

# Usage: Rscript run.R build_scorecard dev|final
cmd_build_scorecard <- function(args) {
  if (length(args) != 1 || !args[1] %in% c("dev", "final")) stop("Usage: build_scorecard.R dev|final")
  mode <- args[1]
  last_target <- if (mode == "dev") 2025L else 2026L
  FACETS <- c(stuff = "Stuff (ours)", location = "Location (ours)", pitching = "Pitching (ours)")
  NS <- c(100L, 250L, 500L)
  MIN_IP <- 50; MIN_ROS_IP <- 30
  c0 <- cfg()

  # Runs, park factors, RA9 and Marcel
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

  # Season-level metrics, one row per pitcher-season
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
  wide <- wide[stats::complete.cases(wide), ]  # common sample: every metric present
  METRICS <- setdiff(names(wide), c("pitcher", "season"))
  long <- function(metric) dplyr::transmute(wide, pitcher, season, value = .data[[metric]])

  # Stickiness, year y vs y+1
  stick <- dplyr::bind_rows(lapply(METRICS, function(m) {
    p <- pair_seasons(long(m), ip_tbl, MIN_IP) %>% dplyr::filter(season + 1L <= last_target)
    bands <- p %>% dplyr::group_by(band) %>% dplyr::summarise(r = wcor(x, y, w), .groups = "drop")
    tibble::tibble(metric = m, n = nrow(p), pearson = wcor(p$x, p$y, p$w), spearman = wspearman(p$x, p$y, p$w),
                   `r 50-100` = bands$r[bands$band == "50-100"], `r 100-150` = bands$r[bands$band == "100-150"],
                   `r 150+` = bands$r[bands$band == "150+"])
  }))
  gone <- disappeared_count(long("ERA") %>% dplyr::filter(season < last_target), ip_tbl, MIN_IP)$n

  # Split-half, odd vs even games
  halves <- dplyr::bind_rows(lapply(2021:last_target, function(s) store_read(grades_key("MLB", s)))) %>%
    dplyr::filter(half != "full", pitch_type == "ALL") %>%
    dplyr::select(pitcher, season, facet, half, grade_global, n) %>%
    tidyr::pivot_wider(names_from = half, values_from = c(grade_global, n)) %>%
    dplyr::inner_join(ip_tbl, by = c("pitcher", "season")) %>% dplyr::filter(ip >= MIN_IP)
  split <- halves %>% dplyr::group_by(facet) %>%
    dplyr::summarise(n = dplyr::n(), pearson = wcor(grade_global_odd, grade_global_even, pmin(n_odd, n_even)),
                     spearman = wspearman(grade_global_odd, grade_global_even, pmin(n_odd, n_even)), .groups = "drop") %>%
    dplyr::mutate(metric = FACETS[facet]) %>% dplyr::select(metric, n, pearson, spearman)

  # Next-season RA9
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

  # Rest of season after N pitches
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

  # Pitch-model rows
  reports <- file.path(c0$data_dir, "reports")
  model_eval <- readr::read_csv(file.path(reports, "model_eval.csv"), show_col_types = FALSE)
  cal <- readr::read_csv(file.path(reports, "calibration.csv"), show_col_types = FALSE) %>%
    dplyr::filter(n >= 2000) %>% dplyr::group_by(model) %>%
    dplyr::summarise(cells = dplyr::n(), max_abs_error = max(abs(mean_pred - mean_actual)), .groups = "drop")

  # Success criteria
  g <- function(tbl, m, col = "rmse") tbl[[col]][tbl$metric == m]
  # Prediction criteria are judged by the projection, not the grades alone
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

  # Chart
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

  # Markdown
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
}

# Usage: Rscript run.R build_scorecard_p3
cmd_build_scorecard_p3 <- function(args) {
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

  next_frames <- list(); tp <- list(); cal <- list(); aaa <- list(); two_frames <- list(); pt_frames <- list()
  rw_all <- realized_war_inputs(inp$lines, inp$parks)
  for (y in c(BASES, 2026L)) {
    i <- trim_inputs(inp, y)
    models <- fit_projection_models(i$lines, i$signals, NULL, i$parks, i$league, horizons = 1:2)
    map <- fit_runs_map(i$lines, i$parks)
    er_r <- i$league$er_r[i$league$level == "MLB" & i$league$season == y]
    tp[[as.character(y)]] <- dplyr::mutate(talent_point(models, i$lines, i$signals, i$parks, i$league, y, map),
                                           era_tt = ra9 * er_r, season = y)
    if (y == 2026L) break
    curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) aging_curve(i$lines, cpt)), PROJ_COMPONENTS)
    pt <- fit_playing_time(pt_rows(i$lines, i$il, i$profiles, i$league, 2012:(y - 1L)))
    pt_new <- pt_rows(i$lines, i$il, i$profiles, i$league, y, with_target = FALSE)
    war_k <- replacement_scale(realized_war_inputs(i$lines, i$parks), i$league, 2021:y)
    proj <- project_pitchers(models, i$lines, i$signals, i$parks, i$league, y, map, anchors, curves,
                             n_draws = 500L, seed = y, keep_draws = TRUE, pt = pt, pt_new = pt_new, war_k = war_k)
    h1 <- proj[proj$horizon == 1, ]
    dr <- attr(proj, "draws")
    if (y + 2L <= 2026L) {  # season 2: two-year model vs next season aged forward
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
    # Playing time and WAR: every projected pitcher, realized zeros included
    lg_y <- lg$lg_ra9[lg$season == y]; lg_n <- lg$lg_ra9[lg$season == y + 1]
    mlb_y <- i$lines[i$lines$level == "MLB" & i$lines$season == y, ]; mlb_y1 <- i$lines[i$lines$level == "MLB" & i$lines$season == y - 1, ]
    a_n <- rw_all[rw_all$season == y + 1, ]
    pf <- h1 %>% dplyr::transmute(base = y, pitcher, ra9, ip, ip_lo, ip_hi, war, war_lo, war_hi) %>%
      dplyr::left_join(dplyr::transmute(a_n, pitcher, outs_act = outs, ra9_act = ra9_adj, sp_act = starter), by = "pitcher") %>%
      dplyr::mutate(ip_act = dplyr::coalesce(outs_act, 0) / 3,
                    war_act = war_from(dplyr::coalesce(ra9_act, 0), dplyr::coalesce(outs_act, 0), dplyr::coalesce(sp_act, 0), lg_n, war_k),
                    ip_y = dplyr::coalesce(mlb_y$outs[match(pitcher, mlb_y$pitcher)], 0) / 3,
                    ip_y1 = dplyr::coalesce(mlb_y1$outs[match(pitcher, mlb_y1$pitcher)], 0) / 3,
                    sp_y = as.numeric(dplyr::coalesce(mlb_y$gs_share[match(pitcher, mlb_y$pitcher)], 0) >= 0.5),
                    marcel_ip = 0.5 * ip_y + 0.1 * ip_y1 + ifelse(sp_y == 1, 50, 25),
                    marcel_war = war_from(ra9, marcel_ip * 3, sp_y, lg_y, war_k))
    pt_frames[[as.character(y)]] <- pf
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

  # Playing time and WAR
  ptf <- dplyr::bind_rows(pt_frames); ptf$w <- 1; ptf$w_act <- ptf$ip_act + 10
  pt_tbl <- tibble::tibble(metric = c("Innings", "WAR"),
                           ours = c(raw_rmse(ptf$ip_act, ptf$ip, ptf$w), raw_rmse(ptf$war_act, ptf$war, ptf$w)),
                           marcel = c(raw_rmse(ptf$ip_act, ptf$marcel_ip, ptf$w), raw_rmse(ptf$war_act, ptf$marcel_war, ptf$w)),
                           ours_actual_weighted = c(raw_rmse(ptf$ip_act, ptf$ip, ptf$w_act), raw_rmse(ptf$war_act, ptf$war, ptf$w_act)),
                           marcel_actual_weighted = c(raw_rmse(ptf$ip_act, ptf$marcel_ip, ptf$w_act), raw_rmse(ptf$war_act, ptf$marcel_war, ptf$w_act)),
                           ours_design_weighted = c(raw_rmse(ptf$ip_act, ptf$ip, pmax(ptf$ip, ptf$ip_act, 1)), raw_rmse(ptf$war_act, ptf$war, pmax(ptf$ip, ptf$ip_act, 1))),
                           marcel_design_weighted = c(raw_rmse(ptf$ip_act, ptf$marcel_ip, pmax(ptf$marcel_ip, ptf$ip_act, 1)), raw_rmse(ptf$war_act, ptf$marcel_war, pmax(ptf$marcel_ip, ptf$ip_act, 1))),
                           coverage_90 = c(coverage(ptf$ip_lo, ptf$ip_hi, ptf$ip_act), coverage(ptf$war_lo, ptf$war_hi, ptf$war_act)))
  war_tot <- ptf %>% dplyr::group_by(base) %>% dplyr::summarise(projected = sum(war), realized = sum(war_act), .groups = "drop")
  part <- nf[nf$ip_next >= 20, ]
  cov_20 <- coverage(part$era_out_lo, part$era_out_hi, part$era_next)

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

  # Triple-A translation: mostly-AAA pitchers with 30+ MLB innings the next season
  aq <- nf[nf$basis == "MLB-equivalent" & nf$ip_next >= 30, ]
  aaa_tbl <- if (nrow(aq)) tibble::tibble(n = nrow(aq), bias = stats::weighted.mean(aq$ra9_next - aq$ours_ra9, aq$ip_next),
                                          rmse = raw_rmse(aq$ra9_next, aq$ours_ra9, aq$ip_next),
                                          league_rmse = raw_rmse(aq$ra9_next, lg$lg_ra9[match(aq$base + 1L, lg$season)], aq$ip_next),
                                          coverage = coverage(aq$era_out_lo, aq$era_out_hi, aq$era_next)) else NULL

  # Rest of season after 100/250/500 pitches, model fit on earlier seasons only
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

  # Write
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
    sprintf("%s — 90%% season ranges cover 85–95%% of next-season ERAs (%.1f%%, qualified pitchers)", pf(cov_q >= 0.85 && cov_q <= 0.95), 100 * cov_q),
    sprintf("%s — 90%% season ERA ranges cover 85–95%% for every pitcher with 20+ IP next season (%.1f%%)", pf(cov_20 >= 0.85 && cov_20 <= 0.95), 100 * cov_20),
    sprintf("%s — Innings projection beats Marcel playing time (RMSE %.1f vs %.1f IP)", pf(pt_tbl$ours[1] < pt_tbl$marcel[1]), pt_tbl$ours[1], pt_tbl$marcel[1]),
    sprintf("%s — WAR projection beats Marcel playing time (RMSE %.2f vs %.2f)", pf(pt_tbl$ours[2] < pt_tbl$marcel[2]), pt_tbl$ours[2], pt_tbl$marcel[2]),
    sprintf("%s — 90%% innings ranges cover 85–95%% (%.1f%%)", pf(pt_tbl$coverage_90[1] >= 0.85 && pt_tbl$coverage_90[1] <= 0.95), 100 * pt_tbl$coverage_90[1]),
    sprintf("%s — 90%% WAR ranges cover 85–95%% (%.1f%%)", pf(pt_tbl$coverage_90[2] >= 0.85 && pt_tbl$coverage_90[2] <= 0.95), 100 * pt_tbl$coverage_90[2]))
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
           "## Playing time and WAR (every projected pitcher, realized zeros included)", "",
           "Marcel playing time: 0.5 x last season's IP + 0.1 x the season before + 50 (starters) or 25 (relievers), with our ERA projection.", "",
           "Pass/fail uses plain RMSE, the right test for an expected-value forecast. The actual-weighted columns weight by actual IP + 10; the design-weighted columns weight each forecast by the larger of its own projection and actual IP (the metric first planned). Both reward forecasts that run high (Marcel gives every Triple-A arm 25 IP).", "",
           md_tbl(pt_tbl), "", "League WAR by base season (projected sum of expected WAR vs realized):", "", md_tbl(war_tot, 0), "",
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
}

# Usage: Rscript run.R changed_seasons <before_sha> <after_sha>
cmd_changed_seasons <- function(args) {
  if (length(args) != 2) stop("Usage: Rscript scripts/changed_seasons.R <before_sha> <after_sha>")
  paths <- if (grepl("^0+$", args[1])) {
    file.path("corrections", list.files(file.path(repo_root(), "corrections")))
  } else {
    system2("git", c("diff", "--name-only", args[1], args[2]), stdout = TRUE)
  }
  writeLines(paste(changed_seasons(paths), collapse = " "))
}

# Usage: Rscript run.R fetch_fangraphs <first_season> <last_season>
cmd_fetch_fangraphs <- function(args) {
  if (length(args) != 2) stop("Usage: Rscript scripts/fetch_fangraphs.R <first> <last>")
  for (s in seq(as.integer(args[1]), as.integer(args[2]))) {
    f <- fetch_fangraphs(s)
    store_write(f, fangraphs_key(s))
    q <- f[f$ip_outs >= 150, ]
    message(s, ": ", nrow(f), " pitchers; Stuff+ present for ", round(100 * mean(!is.na(q$stuff_plus)), 1),
            "% of those with 50+ IP")
  }
}

# Usage: Rscript run.R fetch_inputs <MLB|AAA> <season>
cmd_fetch_inputs <- function(args) {
  if (length(args) != 2) stop("Usage: Rscript scripts/fetch_inputs.R <MLB|AAA> <season>")
  lv <- args[1]; s <- as.integer(args[2])
  repo <- Sys.getenv("PV_DATA_REPO")
  if (!nzchar(repo)) stop("Set PV_DATA_REPO to the private data repo (owner/name).")

  current <- is_current_season(s)
  if (current) {
    release_download_dir(sprintf("raw/statcast/%s/%s", lv, s), repo)
    release_download_dir(sprintf("raw/weather/daily/%s", lv), repo)
  } else {
    release_download_file(statcast_season_key(lv, s), repo)
  }
  if (current || !store_exists(game_weather_key(lv, s))) {
    ok <- tryCatch({ release_download_file(game_weather_key(lv, s), repo); TRUE }, error = function(e) FALSE)
    if (!ok) message("No season weather file for ", lv, " ", s, " yet; daily files only")
  }
  ensure_schedule(lv, s, repo, current)
  # Season stats only feed G, GS and IP in the pitcher index
  stats_key <- sprintf("raw/statsapi/pitching/%s/%s.parquet", lv, s)
  if (!store_exists(stats_key))
    tryCatch(release_download_file(stats_key, repo), error = function(e) message("No stored stats for ", lv, " ", s))
  for (name in names(MODEL_SPECS)) release_download_dir(sprintf("models/lgb/%s", name), repo)
  message("Inputs ready for ", lv, " ", s, if (current) " (current season)")
}

# Usage: Rscript run.R fit_refs
cmd_fit_refs <- function(args) {
  cols <- c("pitcher", "season", "level", "pitch_type", "pitch_group", "arm_angle", "rel_z", "rel_x",
            "hb", "ivb", "vaa", "plate_z", "ext", "balls", "strikes", "stand", "p_throws", "in_play",
            "delta_run_exp", "estimated_woba_using_speedangle")
  d <- dplyr::bind_rows(lapply(cfg()$seasons_ref, function(s) {
    arrow::open_dataset(store_path(prepared_key("MLB", s))) %>% dplyr::select(dplyr::all_of(cols)) %>% dplyr::collect()
  }))
  d <- d[d$balls <= 3 & d$strikes <= 2, ]
  write_ref(fit_arm_angle_ref(d[!is.na(d$arm_angle), ]), "arm")
  write_ref(fit_slot_ref(d), "slot")
  write_ref(fit_vaa_ref(d), "vaa")
  bip <- fit_bip_xrv(d)
  write_ref(bip, "bip")
  d$target <- denoised_target(d, bip)
  d$same_side <- as.integer(d$stand == d$p_throws)
  mix <- d %>% dplyr::count(balls, strikes, same_side) %>% dplyr::mutate(share = n / sum(n)) %>% dplyr::select(-n)
  write_ref(mix, "count_mix")
  base <- d %>% dplyr::filter(!is.na(target)) %>%
    dplyr::group_by(balls, strikes, same_side) %>% dplyr::summarise(mean_target = mean(target), .groups = "drop")
  write_ref(base, "count_baseline")
  zone_cols <- c("batter", "sz_top", "sz_bot")
  zpos <- arrow::open_dataset(store_path(prepared_key("MLB", 2025L))) %>% dplyr::select(dplyr::all_of(zone_cols)) %>% dplyr::collect()
  zabs <- arrow::open_dataset(store_path(prepared_key("MLB", ABS_ERA$MLB[1]))) %>% dplyr::select(dplyr::all_of(zone_cols)) %>% dplyr::collect()
  zm <- fit_zone_map(zpos, zabs)
  write_ref(zm, "zone_map")
  print(as.data.frame(zm))
  print(as.data.frame(bip))
  message("Mean target ", round(mean(d$target, na.rm = TRUE), 5), " vs mean delta_run_exp ",
          round(mean(d$delta_run_exp, na.rm = TRUE), 5))
}

# Usage: Rscript run.R freeze_projection_anchors
cmd_freeze_projection_anchors <- function(args) {
  inp <- projection_inputs(through = 2025L)
  pts <- dplyr::bind_rows(lapply(2023:2025, function(s) {
    i <- trim_inputs(inp, s)
    models <- fit_projection_models(i$lines, i$signals, NULL, i$parks, i$league, horizons = 1L)
    map <- fit_runs_map(i$lines, i$parks)
    dplyr::mutate(talent_point(models, i$lines, i$signals, i$parks, i$league, s, map), season = s)
  }))
  a <- projection_anchors(pts, inp$lines, 2023:2025)
  readr::write_csv(a, file.path(repo_root(), "models", PROJECTION_ANCHORS_FILE))
  print(a)
}

# Usage: Rscript run.R hash_password
cmd_hash_password <- function(args) {
  pw <- if (interactive()) readline("Admin password: ") else {
    message("Admin password (input is not hidden in a plain terminal): ", appendLF = FALSE)
    system("stty -echo", ignore.stderr = TRUE); on.exit(system("stty echo", ignore.stderr = TRUE))
    x <- readLines(file("stdin"), n = 1); message(""); x
  }
  if (!nzchar(pw)) stop("Empty password.")
  writeLines(bcrypt::hashpw(pw))
}

# Usage: Rscript run.R ingest_daily [YYYY-MM-DD]
cmd_ingest_daily <- function(args) {
  today <- if (length(args) >= 1) as.Date(args[1]) else as.Date(format(Sys.time(), tz = "America/New_York", "%Y-%m-%d"))
  parks <- readr::read_csv(file.path(repo_root(), "reference", "parks.csv"), show_col_types = FALSE)
  failed <- FALSE

  ingest_level <- function(lv) {
    dates <- ingest_dates(today)
    seasons <- unique(as.integer(format(dates, "%Y")))
    sched_days <- function(s, l) {
      d <- game_days(s, l)
      d[d %in% dates]
    }
    log <- backfill_statcast(lv, seasons, days = sched_days, today = today)
    if (nrow(log) > 0) message(lv, ": ", paste(log$date, log$status, collapse = "; "))
    bad <- backfill_failed(log)

    wday <- weather_date(today)
    wkey <- sprintf("raw/weather/daily/%s/%s.parquet", lv, format(wday))
    games <- dplyr::filter(fetch_schedule(as.integer(format(wday, "%Y")), lv), game_date == wday)
    if (nrow(games) > 0 && !store_exists(wkey)) {
      w <- build_game_weather(games, parks)
      if (weather_usable(w)) store_write(w, wkey) else {
        message(lv, " weather for ", wday, ": not available yet (will retry next run)")
      }
    }
    bad
  }

  for (lv in names(cfg()$sport_ids)) {
    res <- tryCatch(ingest_level(lv), error = function(e) {
      message(lv, ": FAILED - ", conditionMessage(e))
      TRUE
    })
    failed <- failed || isTRUE(res)
  }
  if (failed) quit(status = 1)
}

# Usage: Rscript run.R migrate_corrections migrate <old_app_dir> | verify
cmd_migrate_corrections <- function(args) {
  mode <- args[1]
  seasons <- 2024:2026
  report_dir <- file.path(cfg()$data_dir, "reports")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(repo_root(), "corrections"), showWarnings = FALSE)
  report <- file.path(report_dir, "corrections_migration_report.md")

  if (identical(mode, "migrate")) {
    old_dir <- args[2]
    lines <- c("# Corrections migration", "", "| season | old rows | unique play_ids | mapped | unmapped |", "|---|---|---|---|---|")
    for (y in seasons) {
      old <- read_old_corrections(file.path(old_dir, sprintf("pitch_corrections_%s.csv", y)))
      idx <- play_id_index(file.path(old_dir, sprintf("pitch_data_%s_processed.csv", y)))
      m <- migrate_corrections(old, idx)
      readr::write_csv(m$mapped, file.path(repo_root(), "corrections", sprintf("pitch_corrections_%s.csv", y)))
      readr::write_csv(m$unmapped, file.path(report_dir, sprintf("corrections_unmapped_%s.csv", y)))
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s |", y, nrow(old), dplyr::n_distinct(old$play_id),
                                nrow(m$mapped), nrow(m$unmapped)))
      message(y, ": old ", nrow(old), ", mapped ", nrow(m$mapped), ", unmapped ", nrow(m$unmapped))
    }
    writeLines(lines, report)
  } else if (identical(mode, "verify")) {
    lines <- c("", "## Check against Savant pitch types", "",
               "| season | corrections | found in Statcast | original type matches Savant |", "|---|---|---|---|")
    for (y in seasons) {
      key <- statcast_season_key("MLB", y)
      if (!store_exists(key)) { message(y, ": Statcast not backfilled yet"); next }
      mapped <- readr::read_csv(file.path(repo_root(), "corrections", sprintf("pitch_corrections_%s.csv", y)),
                                col_types = readr::cols(.default = "c"))
      sc <- arrow::read_parquet(store_path(key), col_select = c("pitch_uid", "pitch_type"))
      v <- verify_against_statcast(mapped, sc)
      readr::write_csv(v$mismatches, file.path(report_dir, sprintf("corrections_mismatches_%s.csv", y)))
      lines <- c(lines, sprintf("| %s | %s | %s (%.1f%%) | %.1f%% |", y, nrow(mapped), v$n_found,
                                100 * v$n_found / max(nrow(mapped), 1), 100 * v$match_rate))
      message(y, ": found ", v$n_found, " of ", nrow(mapped), ", match rate ", round(v$match_rate, 3))
    }
    write(lines, file = report, append = TRUE)
  } else {
    stop("Usage: Rscript scripts/migrate_corrections.R migrate <old_app_dir> | verify")
  }
}

# Usage: Rscript run.R prepare_pitches <MLB|AAA> <first_season> <last_season>
cmd_prepare_pitches <- function(args) {
  if (length(args) != 3) stop("Usage: Rscript scripts/prepare_pitches.R <MLB|AAA> <first> <last>")
  for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
    p <- prepare_pitches(args[1], s)
    store_write(p$pitches, prepared_key(args[1], s))
    store_write(p$offsets, park_offsets_key(args[1], s))
    message(args[1], " ", s, ": ", nrow(p$pitches), " pitches prepared")
    rm(p); gc()
  }
}

# Usage: Rscript run.R project <season> [--no-publish]
cmd_project <- function(args) {
  if (!length(args)) stop("Usage: Rscript scripts/project.R <season> [--no-publish]")
  season <- as.integer(args[1])
  app_repo <- Sys.getenv("PV_APP_REPO", unset = "harrisonsrubin/pitching-app")
  data_repo <- Sys.getenv("PV_DATA_REPO")
  token <- Sys.getenv("APP_GH_TOKEN", unset = "")
  token <- if (nzchar(token)) token else NULL
  if (nzchar(data_repo)) fetch_projection_inputs(data_repo, app_repo, token)
  check_projection_inputs(season)
  res <- run_projection_from(projection_inputs(season), season, read_projection_anchors())
  keys <- write_projection_outputs(res, season)
  if (!"--no-publish" %in% args) publish_outputs(keys, app_repo, token)
  message("Projections written for ", season)
}

# Usage: Rscript run.R publish season <MLB|AAA> <season> | publish scorecard
cmd_publish <- function(args) {
  app_repo <- Sys.getenv("PV_APP_REPO", unset = "harrisonsrubin/pitching-app")
  app_token <- Sys.getenv("APP_GH_TOKEN", unset = "")
  app_token <- if (nzchar(app_token)) app_token else NULL

  if (length(args) == 3 && args[1] == "season") {
    lv <- args[2]; s <- as.integer(args[3])
    data_repo <- Sys.getenv("PV_DATA_REPO")
    if (!nzchar(data_repo)) stop("Set PV_DATA_REPO to the private data repo (owner/name).")
    assert_private_repo(data_repo, app_repo)
    publish_display(lv, s, data_repo)
    publish_outputs(outputs_for(lv, s), app_repo, app_token)
    message("Published ", lv, " ", s)
  } else if (length(args) == 1 && args[1] == "scorecard") {
    publish_outputs(stage_scorecard(), app_repo, app_token)
    message("Published scorecard")
  } else {
    stop("Usage: Rscript scripts/publish.R season <MLB|AAA> <season> | scorecard")
  }
}

# Usage: Rscript run.R score_pitches <MLB|AAA> <first_season> <last_season>
cmd_score_pitches <- function(args) {
  if (length(args) != 3) stop("Usage: Rscript scripts/score_pitches.R <MLB|AAA> <first> <last>")
  refs <- read_refs()
  for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
    d <- store_read(features_key(args[1], s))
    x <- score_pitches(d, refs)
    store_write(x, xrv_key(args[1], s))
    message(args[1], " ", s, ": scored ", nrow(x), "; mean stuff_xrv ", round(mean(x$stuff_xrv, na.rm = TRUE), 5),
            " vs mean target ", round(mean(x$target, na.rm = TRUE), 5))
    rm(d, x); gc()
  }
}

# Usage: Rscript run.R sync_store upload|upload-file|download <path>
cmd_sync_store <- function(args) {
  repo <- Sys.getenv("PV_DATA_REPO")
  if (!nzchar(repo)) stop("Set PV_DATA_REPO to the private data repo (owner/name).")
  if (length(args) != 2) stop("Usage: Rscript scripts/sync_store.R upload|upload-file|download <path>")
  path <- args[2]

  if (identical(args[1], "upload")) {
    keys <- release_upload_dir(path, repo)  # skips files already uploaded at the same size
    message("Synced ", length(keys), " file(s) under ", path)
  } else if (identical(args[1], "upload-file")) {
    if (!store_exists(path)) stop("No such file in the store: ", path)
    release_upload(path, repo)
    message("Uploaded ", path)
  } else if (identical(args[1], "download")) {
    keys <- release_download_dir(path, repo)
    message("Restored ", length(keys), " file(s) under ", path)
  } else {
    stop("Usage: Rscript scripts/sync_store.R upload|upload-file|download <path>")
  }
}

# Usage: Rscript run.R train_models forward | oof [names...]
cmd_train_models <- function(args) {
  if (length(args) < 1 || !args[1] %in% c("forward", "oof")) stop("Usage: train_models.R forward|oof [names...]")
  reports <- file.path(cfg()$data_dir, "reports")
  dir.create(reports, recursive = TRUE, showWarnings = FALSE)

  if (args[1] == "forward") {
    refs <- read_refs()
    names <- c("stuff", "location", "pitching", "stuff_physics", "pitching_noseq",
               "p_swing", "p_whiff", "p_cstrike", "xwoba_contact", "p_gb")
    res <- lapply(names, function(n) { message("forward: ", n); r <- forward_eval(n, refs); gc(); r })
    ev <- dplyr::bind_rows(lapply(res, `[[`, "eval"))
    readr::write_csv(ev, file.path(reports, "model_eval.csv"))
    readr::write_csv(dplyr::bind_rows(lapply(res, `[[`, "calibration")), file.path(reports, "calibration.csv"))
    print(as.data.frame(ev))
  } else {
    names <- if (length(args) > 1) args[-1] else names(MODEL_SPECS)
    for (n in names) { message("oof: ", n); train_oof(n); gc() }
  }
}

# Usage: Rscript run.R update_season <MLB|AAA> <season>
cmd_update_season <- function(args) {
  if (length(args) != 2) stop("Usage: Rscript scripts/update_season.R <MLB|AAA> <season>")
  lv <- args[1]; s <- as.integer(args[2])
  step <- function(label, expr) {
    t0 <- Sys.time(); force(expr)
    message(sprintf("[%s %s] %s done in %.1f min", lv, s, label, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    invisible(gc())
  }

  if (has_day_files(lv, s)) step("compact", compact_season(lv, s))
  step("weather", store_write(assemble_season_weather(lv, s), game_weather_key(lv, s)))
  step("prepare", {
    p <- prepare_pitches(lv, s)
    store_write(p$pitches, prepared_key(lv, s)); store_write(p$offsets, park_offsets_key(lv, s))
    message(nrow(p$pitches), " pitches prepared"); rm(p)
  })
  refs <- read_refs()
  step("features", { d <- build_features_season(lv, s, refs); store_write(d, features_key(lv, s)); rm(d) })
  step("score", {
    d <- store_read(features_key(lv, s)); x <- score_pitches(d, refs)
    store_write(x, xrv_key(lv, s)); message(nrow(x), " pitches scored"); rm(d, x)
  })
  step("outputs", build_outputs(lv, s))
}

# Usage: Rscript run.R validate_data
cmd_validate_data <- function(args) {
  c0 <- cfg()
  levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)
  res <- dplyr::bind_rows(lapply(names(levels_seasons), function(lv) {
    dplyr::bind_rows(lapply(levels_seasons[[lv]], function(s) {
      if (!store_exists(statcast_season_key(lv, s))) {
        return(tibble::tibble(level = lv, season = s, check = "season file exists", value = 0, pass = FALSE))
      }
      validate_statcast_season(lv, s)
    }))
  }))

  fmt <- function(v) ifelse(v > 1, format(round(v), big.mark = ","), sprintf("%.4f", v))
  out <- file.path(c0$data_dir, "reports", "data_report.md")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  lines <- c("# Data report", "", sprintf("Generated %s", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
             "| level | season | check | value | pass |", "|---|---|---|---|---|",
             sprintf("| %s | %s | %s | %s | %s |", res$level, res$season, res$check, fmt(res$value),
                     ifelse(is.na(res$pass), "n/a", ifelse(res$pass, "yes", "**NO**"))))
  writeLines(lines, out)
  failed <- res[!is.na(res$pass) & !res$pass, ]
  message("Checks: ", nrow(res), "  failed: ", nrow(failed), "  report: ", out)
  if (nrow(failed) > 0) {
    print(as.data.frame(failed))
    quit(status = 1)
  }
}

# Commands main() can dispatch to
COMMANDS <- c("backfill_statcast", "backfill_statsapi", "backfill_weather", "build_features", "build_grades", "build_outputs", "build_scorecard", "build_scorecard_p3", "changed_seasons", "fetch_fangraphs", "fetch_inputs", "fit_refs", "freeze_projection_anchors", "hash_password", "ingest_daily", "migrate_corrections", "prepare_pitches", "project", "publish", "score_pitches", "sync_store", "train_models", "update_season", "validate_data")

# Run the named command, or list commands
main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  if (!length(argv) || !argv[1] %in% COMMANDS) {
    message("Usage: Rscript run.R <command> [arguments]\nCommands: ", paste(COMMANDS, collapse = ", "))
    if (length(argv)) quit(status = 1) else return(invisible())
  }
  get(paste0("cmd_", argv[1]))(argv[-1])
}

if (sys.nframe() == 0L) main()
