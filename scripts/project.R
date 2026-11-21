# Usage: Rscript scripts/project.R <season> [--no-publish]
# P3a true-talent projection from the given (latest) season: fits the component models on
# stored season lines and published outputs, simulates, and writes
# outputs/projections_<season>.parquet and outputs/luck_<level>_<season>.parquet, then
# publishes them to the app's outputs release.
# With PV_DATA_REPO set (CI) it first downloads the Stats API season lines and any
# published outputs it lacks; locally it uses the store as is.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
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
