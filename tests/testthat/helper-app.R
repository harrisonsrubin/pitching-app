# App tests: source app/R/*.R (Shiny auto-loads them in the real app) and build a tiny
# local data dir the app can read in local mode (PV_APP_LOCAL=1).

source_app <- function() {
  for (f in sort(list.files(file.path(repo_root(), "app", "R"), pattern = "\\.R$", full.names = TRUE)))
    sys.source(f, envir = globalenv())
  invisible(TRUE)
}

# Horizontal values follow Statcast's catcher's view: a righty's release side and
# fastball run are negative.
fixture_display <- function() {
  set.seed(7)
  mk <- function(pitcher, types, n, throws, date) {
    tibble::tibble(
      pitch_uid = paste0(pitcher, "-", seq_len(n), "-1"), game_pk = as.integer(pitcher * 10 + seq_len(n) %% 3),
      game_date = as.Date(date) + seq_len(n) %% 3, pitcher = as.integer(pitcher), batter = 9L,
      stand = rep(c("L", "R"), length.out = n), p_throws = throws,
      pitch_type = rep(types, length.out = n), pitch_type_savant = rep(types, length.out = n),
      balls = 0L, strikes = 0L, description = "ball", events = NA_character_,
      velo = 92 + rnorm(n), spin_rate = 2300, ivb = 14 + rnorm(n), hb = ifelse(throws == "L", 1, -1) * (10 + rnorm(n)),
      ext = 6.4, rel_x = ifelse(throws == "L", 2, -2), rel_z = 5.8, arm_angle = 42, vaa = -5,
      plate_x = rnorm(n, 0, 0.6), plate_z = rnorm(n, 2.5, 0.6), sz_top = 3.4, sz_bot = 1.6,
      stuff_xrv = rnorm(n, 0, 0.01), location_xrv = rnorm(n, 0, 0.01), pitching_xrv = rnorm(n, 0, 0.01))
  }
  dplyr::bind_rows(mk(101, c("4S", "SL"), 60, "R", "2026-04-01"),
                   mk(102, "SNK", 40, "L", "2026-05-01"),
                   mk(103, c("4S", "CH", "CB"), 50, "R", "2026-06-01"))
}

make_app_fixture <- function(dir) {
  out <- function(name) file.path(dir, "outputs", name)
  dir.create(file.path(dir, "outputs"), recursive = TRUE, showWarnings = FALSE)
  d <- fixture_display()
  types <- dplyr::distinct(d, pitcher, pitch_type) %>% dplyr::count(pitcher, pitch_type)
  g <- dplyr::bind_rows(lapply(c("stuff", "location", "pitching"), function(f) {
    by <- dplyr::bind_rows(dplyr::distinct(d, pitcher, pitch_type), dplyr::distinct(d, pitcher) %>% dplyr::mutate(pitch_type = "ALL"))
    by %>% dplyr::mutate(facet = f, n = 30L, xrv_per100 = 0.1,
                         grade_type = ifelse(pitch_type == "ALL", NA_real_, 50 + pitcher %% 10),
                         grade_global = 48 + pitcher %% 10, grade_lo80 = grade_global - 6, grade_hi80 = grade_global + 6)
  })) %>% dplyr::mutate(level = "MLB", season = 2026L, half = "full", .before = 1)
  g$grade_global[g$pitcher == 103 & g$pitch_type == "CB" & g$facet == "location"] <- NA_real_
  arrow::write_parquet(g, out("grades_MLB_2026.parquet"))
  comp <- dplyr::bind_rows(dplyr::distinct(d, pitcher, pitch_type), dplyr::distinct(d, pitcher) %>% dplyr::mutate(pitch_type = "ALL")) %>%
    dplyr::mutate(level = "MLB", season = 2026L, n = 30L, x_swing = 0.45, x_whiff_per_swing = 0.25,
                  x_chase = ifelse(pitcher == 102, NaN, 0.3), x_cstrike_per_take = 0.3, x_woba_contact = 0.36, x_gb = 0.45,
                  .before = 1)
  arrow::write_parquet(comp, out("components_MLB_2026.parquet"))
  arrow::write_parquet(tibble::tibble(
    level = "MLB", season = 2026L, pitcher = c(101L, 102L, 103L), name = c("Ace One", "Traded Two", "Three Pitch"),
    throws = c("R", "L", "R"), team_abbrev = c("DET", "NYY", "SEA"), teams = c("DET", "DET,NYY", "SEA"),
    pitches = c(60L, 40L, 50L), g = c(10L, 8L, 9L), gs = c(10L, 0L, 9L), ip_outs = c(180L, 30L, 150L), age = 27L),
    out("pitchers_MLB_2026.parquet"))
  arrow::write_parquet(movement_ref(d, min_n = 1), out("movement_ref_MLB_2026.parquet"))
  arrow::write_parquet(pitch_percentiles(d), out("pitch_pctl_MLB_2026.parquet"))
  writeLines(c("# P1 scorecard", "", "## Success criteria", "", "Grades measure the pitch.", "",
               "- PASS — Stuff (ours) is at least as sticky as Stuff+", "",
               "## Stickiness", "", "| metric | n | pearson |", "|---|---|---|", "| Stuff (ours) | 1,073 | 0.865 |", "",
               "![Stickiness vs predictiveness](scorecard_chart.png)"), out("scorecard.md"))
  grDevices::png(out("scorecard_chart.png"), 100, 100); graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new(); grDevices::dev.off()
  for (k in 0:31) {
    p <- file.path(dir, sprintf("display/MLB/2026/bucket_%d.parquet", k))
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(d[d$pitcher %% 32L == k, ], p)
  }
  invisible(dir)
}

local_app_fixture <- function(.frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .frame)
  make_app_fixture(dir)
  withr::local_envvar(PV_APP_LOCAL = "1", PV_DATA_DIR = dir, .local_envir = .frame)
  source_app()
  dir
}
