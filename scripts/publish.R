# Usage:
#   Rscript scripts/publish.R season <MLB|AAA> <season>   # outputs (public) + display (private)
#   Rscript scripts/publish.R scorecard                   # scorecard page files (public)
# Env: PV_APP_REPO (default harrisonsrubin/pitching-app) and APP_GH_TOKEN (optional; else
# GH_TOKEN / gh login) for the public outputs; PV_DATA_REPO + GH_TOKEN for the private store.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
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
