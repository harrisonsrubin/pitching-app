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
  list(
    seasons_mlb   = 2020:2026,
    seasons_aaa   = 2023:2026,
    seasons_stats = 2010:2026,
    sport_ids     = c(MLB = 1L, AAA = 11L),
    data_dir      = data_dir,
    user_agent    = "pitching-app-ingest/0.1"
  )
}

`%>%` <- magrittr::`%>%`
