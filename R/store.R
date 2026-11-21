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
  files <- list.files(base, pattern = "\\.(parquet|txt)$", recursive = TRUE)   # data + LightGBM boosters
  sort(file.path(prefix, files))
}

# One release per store directory (e.g. "raw--statcast--MLB--2026"), so no release
# comes near GitHub's per-release asset limit. Assets keep their file names.
release_tag <- function(key) gsub("/", "--", dirname(key), fixed = TRUE)

# The one place that shells out to the GitHub CLI, so tests can mock it. `token`
# overrides GH_TOKEN for one call (publishing to the app repo uses its own token).
# system2() goes through the shell, so every argument is quoted (e.g. "Data store"),
# and the pager is off: `gh release view` would otherwise wait for a keypress.
gh_run <- function(args, token = NULL) {
  env <- c("GH_PAGER=cat", "PAGER=cat", if (!is.null(token)) paste0("GH_TOKEN=", shQuote(token)))
  system2("gh", shQuote(args), env = env)
}

# Text output of a gh call (mockable like gh_run).
gh_output <- function(args) suppressWarnings(system2("gh", shQuote(args), stdout = TRUE, stderr = FALSE,
                                                    env = c("GH_PAGER=cat", "PAGER=cat")))

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

# Restores one file from its directory's release.
release_download_file <- function(key, repo) {
  dest <- dirname(store_path(key))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  status <- gh_run(c("release", "download", release_tag(key), "--repo", repo,
                     "--pattern", basename(key), "--dir", dest, "--clobber"))
  if (!identical(as.integer(status), 0L)) stop("gh release download failed for ", key)
  invisible(key)
}

# Uploads every file under a store directory, one gh call per release per `batch` files,
# skipping files the release already holds at the same size (unless `force`). GitHub
# allows 5,000 API calls an hour, so one call per file does not scale to a full store.
# GitHub also caps uploads per hour separately from API calls, so each batch is followed
# by a pause of `seconds_per_file` per file sent.
upload_pause <- function(seconds) Sys.sleep(seconds)

release_upload_dir <- function(prefix, repo, batch = 50, force = FALSE, seconds_per_file = 1) {
  keys <- store_list(prefix)
  for (tag in unique(release_tag(keys))) {
    k <- keys[release_tag(keys) == tag]
    ensure_release(repo, tag)
    if (!force) {
      have <- gh_output(c("release", "view", tag, "--repo", repo, "--json", "assets",
                          "--jq", ".assets[] | \"\\(.name)\\t\\(.size)\""))
      have <- strsplit(have[nzchar(have)], "\t", fixed = TRUE)
      sizes <- stats::setNames(as.numeric(vapply(have, `[`, "", 2)), vapply(have, `[`, "", 1))
      k <- k[!(basename(k) %in% names(sizes) & sizes[basename(k)] == file.size(store_path(k)))]
    }
    for (chunk in split(k, ceiling(seq_along(k) / batch))) {
      status <- gh_run(c("release", "upload", tag, store_path(chunk), "--repo", repo, "--clobber"))
      if (!identical(as.integer(status), 0L)) stop("gh release upload failed for ", tag)
      upload_pause(length(chunk) * seconds_per_file)
    }
    if (length(k)) message(tag, ": uploaded ", length(k), " file(s)")
  }
  invisible(keys)
}
