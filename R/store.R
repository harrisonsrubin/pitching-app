# Data store: parquet files under cfg()$data_dir, addressed by keys like
# "raw/statcast/MLB/2025/2025-04-01.parquet". The same keys map to GitHub release
# assets (slashes flattened to "__") so Actions can sync the store to a private repo.

store_path <- function(key) file.path(cfg()$data_dir, key)

store_write <- function(df, key) {
  path <- store_path(key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  arrow::write_parquet(df, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not move ", tmp, " into place at ", path)
  }
  invisible(path)
}

store_read <- function(key) tibble::as_tibble(arrow::read_parquet(store_path(key)))

store_exists <- function(key) file.exists(store_path(key))

store_list <- function(prefix) {
  base <- store_path(prefix)
  if (!dir.exists(base)) return(character())
  files <- list.files(base, pattern = "\\.parquet$", recursive = TRUE)
  sort(file.path(prefix, files))
}

# One release per store directory (e.g. "raw--statcast--MLB--2026"), so no release
# comes near GitHub's per-release asset limit. Assets keep their file names.
release_tag <- function(key) gsub("/", "--", dirname(key), fixed = TRUE)

# The one place that shells out to the GitHub CLI, so tests can mock it.
gh_run <- function(args) system2("gh", args)

ensure_release <- function(repo, tag) {
  if (identical(as.integer(gh_run(c("release", "view", tag, "--repo", repo))), 0L)) return(invisible(tag))
  status <- gh_run(c("release", "create", tag, "--repo", repo, "--title", tag, "--notes", "Data store"))
  if (!identical(as.integer(status), 0L)) stop("gh release create failed for ", tag)
  invisible(tag)
}

release_upload <- function(key, repo, tag = release_tag(key)) {
  ensure_release(repo, tag)
  status <- gh_run(c("release", "upload", tag, store_path(key), "--repo", repo, "--clobber"))
  if (!identical(as.integer(status), 0L)) stop("gh release upload failed for ", key)
  invisible(key)
}

# Restores every asset of the release that holds `prefix` (a store directory).
release_download_dir <- function(prefix, repo) {
  dest <- store_path(prefix)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  tag <- gsub("/", "--", prefix, fixed = TRUE)
  status <- gh_run(c("release", "download", tag, "--repo", repo, "--dir", dest, "--clobber"))
  if (!identical(as.integer(status), 0L)) stop("gh release download failed for ", tag)
  invisible(store_list(prefix))
}
