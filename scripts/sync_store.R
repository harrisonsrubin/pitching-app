# Sync the local data store with the private data repo's releases
# (one release per store directory; see release_tag() in R/store.R).
# Usage:
#   Rscript scripts/sync_store.R upload <dir>        # every file under a store directory
#   Rscript scripts/sync_store.R upload-file <key>   # a single file
#   Rscript scripts/sync_store.R download <dir>      # restore one store directory
# Needs env PV_DATA_REPO (owner/name) and GH_TOKEN with access to that repo.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
repo <- Sys.getenv("PV_DATA_REPO")
if (!nzchar(repo)) stop("Set PV_DATA_REPO to the private data repo (owner/name).")
if (length(args) != 2) stop("Usage: Rscript scripts/sync_store.R upload|upload-file|download <path>")
path <- args[2]

if (identical(args[1], "upload")) {
  keys <- store_list(path)
  for (k in keys) release_upload(k, repo)
  message("Uploaded ", length(keys), " file(s) under ", path)
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
