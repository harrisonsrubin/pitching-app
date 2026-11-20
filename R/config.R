# Project-wide settings. Every other file reads seasons, levels, and paths from here.

repo_root <- function() {
  if (exists(".pv_root", envir = globalenv())) return(get(".pv_root", envir = globalenv()))
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "R", "load.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Could not find the repo root (R/load.R).")
    root <- parent
  }
  root
}

cfg <- function() {
  data_dir <- Sys.getenv("PV_DATA_DIR", unset = "")
  if (!nzchar(data_dir)) data_dir <- file.path(repo_root(), "data")
  last <- last_season()   # R/seasons.R: rolls to 2027 once its first game is played
  list(
    seasons_mlb   = 2020:last,
    seasons_aaa   = 2023:last,
    seasons_stats = 2010:last,
    sport_ids     = c(MLB = 1L, AAA = 11L),
    data_dir      = data_dir,
    user_agent    = "pitching-app-ingest/0.1",
    # P1 pitch models: training seasons (README 5.2), grade anchor seasons (README 7.6)
    seasons_train = 2023:last,
    seasons_ref   = 2023:2025,
    recency_decay = 0.8,
    n_folds       = 5L,
    lgb_params    = list(learning_rate = 0.05, num_leaves = 63L, min_data_in_leaf = 500L,
                         feature_fraction = 0.8, bagging_fraction = 0.8, bagging_freq = 1L,
                         max_bin = 63L, num_threads = 6L, seed = 2026L, verbose = -1L),
    lgb_max_rounds = 3000L,
    lgb_early_stop = 100L
  )
}

`%>%` <- magrittr::`%>%`
