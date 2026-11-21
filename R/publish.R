# Publishing (P2 design 4): aggregate outputs to the public app repo's "outputs" release;
# pitch-level display files to the private store only. Nothing under display/ is ever
# passed to the public repo.

OUTPUTS_TAG <- "outputs"

outputs_for <- function(level, season) {
  c(grades_output_key(level, season), components_output_key(level, season), pitchers_key(level, season),
    movement_ref_key(level, season), pitch_pctl_key(level, season),
    profiles_key(level, season), command_key(level, season), tunnel_key(level, season), ANCHORS_OUTPUT,
    windows_key(level, season), parks_key(level, season))
}

publish_outputs <- function(keys, app_repo, token = NULL) {
  stopifnot(all(startsWith(keys, "outputs/")))
  missing <- keys[!file.exists(store_path(keys))]
  if (length(missing)) stop("Missing outputs: ", paste(missing, collapse = ", "))
  if (!identical(as.integer(gh_run(c("release", "view", OUTPUTS_TAG, "--repo", app_repo), token = token)), 0L)) {
    st <- gh_run(c("release", "create", OUTPUTS_TAG, "--repo", app_repo, "--title", "App outputs",
                   "--notes", "Aggregated grades and references the app reads."), token = token)
    if (!identical(as.integer(st), 0L)) stop("gh release create failed for ", OUTPUTS_TAG)
  }
  st <- gh_run(c("release", "upload", OUTPUTS_TAG, store_path(keys), "--repo", app_repo, "--clobber"), token = token)
  if (!identical(as.integer(st), 0L)) stop("gh release upload failed for ", OUTPUTS_TAG)
  invisible(keys)
}

# Pitch-level data may only go to a private repo that is not the app repo.
assert_private_repo <- function(data_repo, app_repo) {
  if (identical(tolower(data_repo), tolower(app_repo))) stop("PV_DATA_REPO is the app repo; refusing to publish pitch data there")
  private <- tryCatch(gh_output(c("api", paste0("repos/", data_repo), "--jq", ".private")), error = function(e) "")
  if (!identical(trimws(paste(private, collapse = "")), "true"))
    stop("PV_DATA_REPO (", data_repo, ") is not private or not reachable; refusing to publish pitch data")
  invisible(TRUE)
}

publish_display <- function(level, season, data_repo) {
  keys <- vapply(0:(N_BUCKETS - 1L), function(k) display_key(level, season, k), "")
  # Rebuilt files can keep their size, so display is always re-sent (in one batch).
  release_upload_dir(sprintf("display/%s/%s", level, season), data_repo, force = TRUE)
  invisible(keys)
}

# The scorecard page renders the committed scorecard markdown and chart.
stage_scorecard <- function() {
  dir.create(store_path("outputs"), recursive = TRUE, showWarnings = FALSE)
  md <- readLines(file.path(repo_root(), "scorecard", "p1.md"))
  md <- gsub("](p1_stickiness_vs_predictiveness.png)", "](scorecard_chart.png)", md, fixed = TRUE)
  writeLines(md, store_path("outputs/scorecard.md"))
  file.copy(file.path(repo_root(), "scorecard", "p1_stickiness_vs_predictiveness.png"),
            store_path("outputs/scorecard_chart.png"), overwrite = TRUE)
  c("outputs/scorecard.md", "outputs/scorecard_chart.png")
}
