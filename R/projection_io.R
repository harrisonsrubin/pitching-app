# P3a projection run (design: pipeline). Reads only stored Stats API season lines and
# published outputs, fits, simulates, and writes:
#   outputs/projections_<season>.parquet   one row per pitcher x horizon (1-5)
#   outputs/luck_<level>_<season>.parquet  the luck gap for the base season
# In CI, scripts/project.R first downloads the season lines and the published outputs.

projections_key <- function(season) outputs_key(sprintf("projections_%s.parquet", season))
luck_key <- function(level, season) outputs_key(sprintf("luck_%s_%s.parquet", level, season))
projection_keys <- function(season) c(projections_key(season), luck_key("MLB", season), luck_key("AAA", season))

read_tagged_store <- function(prefix, through) {
  f <- list.files(store_path("outputs"), pattern = sprintf("^%s_(MLB|AAA)_\\d{4}\\.parquet$", prefix), full.names = TRUE)
  dplyr::bind_rows(lapply(f, function(p) {
    m <- regmatches(basename(p), regexec("_(MLB|AAA)_(\\d{4})", basename(p)))[[1]]
    if (as.integer(m[3]) > through) return(NULL)
    d <- tibble::as_tibble(arrow::read_parquet(p)); d$level <- m[2]; d$season <- as.integer(m[3]); d
  }))
}

projection_inputs <- function(through) {
  keys <- c(sprintf("raw/statsapi/pitching/MLB/%s.parquet", 2010:through),
            sprintf("raw/statsapi/pitching/AAA/%s.parquet", 2023:through))
  keys <- keys[store_exists(keys)]
  lines <- season_lines(dplyr::bind_rows(lapply(keys, store_read)))
  grades <- read_tagged_store("grades", through); comps <- read_tagged_store("components", through)
  signals <- if (nrow(grades)) pitch_signals(grades, comps) else tibble::tibble(level = character(), season = integer(), pitcher = integer())
  list(lines = lines, league = league_table(lines), signals = signals,
       windows = read_tagged_store("windows", through), parks = read_tagged_store("parks", through),
       pitchers = read_tagged_store("pitchers", through))
}

trim_inputs <- function(inp, season) {
  cut <- function(d) if (is.null(d) || !nrow(d)) d else d[d$season <= season, ]
  lapply(inp, cut)
}

# Everything after the inputs: models, runs map, aging curves, simulation, luck.
run_projection_from <- function(inp, season, anchors, n_draws = 1000L, seed = 1L) {
  t0 <- Sys.time()
  i <- trim_inputs(inp, season)
  models <- fit_projection_models(i$lines, i$signals, NULL, i$parks, i$league, horizons = 1:2)
  map <- fit_runs_map(i$lines, i$parks)
  curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) aging_curve(i$lines, cpt)), PROJ_COMPONENTS)
  proj <- project_pitchers(models, i$lines, i$signals, i$parks, i$league, season, map, anchors, curves, n_draws, seed)
  today <- attr(proj, "today")
  names_tbl <- if (!is.null(i$pitchers) && nrow(i$pitchers)) i$pitchers %>% dplyr::arrange(dplyr::desc(season), level != "MLB") %>%
    dplyr::distinct(pitcher, .keep_all = TRUE) %>% dplyr::select(pitcher, name) else tibble::tibble(pitcher = integer(), name = character())
  proj <- dplyr::left_join(proj, names_tbl, by = "pitcher") %>% dplyr::mutate(base_season = as.integer(season)) %>%
    dplyr::relocate(pitcher, name, basis, base_season)
  lg <- i$league[i$league$level == "MLB" & i$league$season == season, ]
  strand <- fit_strand(i$lines)
  luck <- lapply(c("MLB", "AAA"), function(lv) {
    if (lv == "AAA") return(NULL)   # Triple-A results vs MLB-equivalent talent would not compare
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

PROJECTION_OUTPUT_PREFIXES <- c("grades", "components", "windows", "parks", "pitchers")

# CI: the Stats API season lines from the data repo, and the published outputs the fit
# reads from the app's public release (with the app repo's token; the data token is
# limited to the data repo). Any failed download stops the run.
fetch_projection_inputs <- function(data_repo, app_repo, app_token = NULL) {
  for (lv in c("MLB", "AAA")) release_download_dir(sprintf("raw/statsapi/pitching/%s", lv), data_repo)
  dir.create(store_path("outputs"), recursive = TRUE, showWarnings = FALSE)
  for (pre in PROJECTION_OUTPUT_PREFIXES) {
    st <- gh_run(c("release", "download", OUTPUTS_TAG, "--repo", app_repo, "--pattern", paste0(pre, "_*"),
                   "--dir", store_path("outputs"), "--skip-existing"), token = app_token)
    if (!identical(as.integer(st), 0L)) stop("Outputs download failed for ", pre, "_* from ", app_repo)
  }
  invisible(TRUE)
}

# The projection must not run (and publish) on partial inputs: the base season's MLB
# grades, components, windows, parks and pitcher index have to be there.
check_projection_inputs <- function(season) {
  keys <- outputs_key(sprintf("%s_MLB_%s.parquet", PROJECTION_OUTPUT_PREFIXES, season))
  missing <- keys[!store_exists(keys)]
  if (length(missing)) stop("Projection inputs missing: ", paste(basename(missing), collapse = ", "))
  TRUE
}
