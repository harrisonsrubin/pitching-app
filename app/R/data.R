# Where the app's data comes from (P2 design 4.3).
# Public outputs (grades, components, pitcher index, references, scorecard) load once at
# startup from the app repo's "outputs" release. Pitch-level display data is private:
# one bucket file per pitcher, downloaded on demand from the private data repo with a
# read-only token and cached for the session. In local mode (PV_APP_LOCAL=1) both are
# read from PV_DATA_DIR instead. The token stays on the server: it is never returned.

app_config <- function() {
  list(local = identical(Sys.getenv("PV_APP_LOCAL"), "1"),
       data_dir = Sys.getenv("PV_DATA_DIR", unset = "../data"),
       app_repo = Sys.getenv("PV_APP_REPO", unset = "harrisonsrubin/pitching-app"),
       data_repo = Sys.getenv("PV_DATA_REPO"),
       data_token = Sys.getenv("PV_DATA_TOKEN"),
       corrections_token = Sys.getenv("PV_CORRECTIONS_TOKEN"),
       admin_hash = Sys.getenv("PV_ADMIN_HASH"))
}

GITHUB_API <- "https://api.github.com"

gh_request <- function(url, token = NULL) {
  req <- httr2::request(url) %>%
    httr2::req_user_agent("pitching-app") %>%
    httr2::req_timeout(60) %>%
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(token) && nzchar(token)) req <- httr2::req_auth_bearer_token(req, token)
  req
}

TRANSIENT_STATUS <- c(403, 429, 500, 502, 503, 504)

# Retries rate limits and server errors (Connect Cloud egress IPs are shared, and the
# unauthenticated API allows 60 calls an hour; the app makes one at startup).
# Returns the last response.
perform_with_retry <- function(req, tries = 4, backoff = function(i) 2^i) {
  for (i in seq_len(tries)) {
    resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
    ok <- !is.null(resp) && !(httr2::resp_status(resp) %in% TRANSIENT_STATUS)
    if (ok || i == tries) return(resp)
    Sys.sleep(backoff(i))
  }
}

release_assets <- function(repo, tag, token = NULL, tries = 4, backoff = function(i) 2^i) {
  resp <- perform_with_retry(gh_request(sprintf("%s/repos/%s/releases/tags/%s", GITHUB_API, repo, tag), token), tries, backoff)
  if (is.null(resp)) stop("release ", tag, ": no response")
  if (httr2::resp_status(resp) != 200)
    stop("release ", tag, ": HTTP ", httr2::resp_status(resp),
         if (httr2::resp_status(resp) == 404) " (missing, or the token cannot see this repo)")
  httr2::resp_body_json(resp, simplifyVector = FALSE)$assets
}

download_asset <- function(asset, dest, token = NULL) {
  req <- gh_request(if (is.null(token)) asset$browser_download_url else asset$url, token) %>%
    httr2::req_headers(Accept = "application/octet-stream")
  resp <- perform_with_retry(req, tries = 3)
  if (is.null(resp)) stop(asset$name, ": no response")
  if (httr2::resp_status(resp) != 200) stop(asset$name, ": HTTP ", httr2::resp_status(resp))
  writeBin(httr2::resp_body_raw(resp), dest)
  dest
}

# Public outputs are read without a token: PV_DATA_TOKEN is limited to the private data
# repo, and GitHub answers "not found" when such a token asks about any other repo.
# Assets download from github.com links, which do not count against the API limit.
outputs_dir <- function(cfg) {
  if (cfg$local) return(file.path(cfg$data_dir, "outputs"))
  dir <- file.path(tempdir(), "pv_outputs")
  dir.create(dir, showWarnings = FALSE)
  for (a in release_assets(cfg$app_repo, "outputs")) download_asset(a, file.path(dir, a$name))
  dir
}

read_outputs <- function(dir, prefix) {
  f <- list.files(dir, pattern = sprintf("^%s_(MLB|AAA)_\\d{4}\\.parquet$", prefix), full.names = TRUE)
  dplyr::bind_rows(lapply(f, function(p) tibble::as_tibble(arrow::read_parquet(p))))
}

load_outputs <- function(cfg) {
  dir <- outputs_dir(cfg)
  pctl_files <- list.files(dir, pattern = "^pitch_pctl_(MLB|AAA)_\\d{4}\\.parquet$", full.names = TRUE)
  pctl <- dplyr::bind_rows(lapply(pctl_files, function(p) {
    m <- regmatches(basename(p), regexec("pitch_pctl_(MLB|AAA)_(\\d{4})", basename(p)))[[1]]
    tibble::as_tibble(arrow::read_parquet(p)) %>% dplyr::mutate(level = m[2], season = as.integer(m[3]))
  }))
  mref_files <- list.files(dir, pattern = "^movement_ref_(MLB|AAA)_\\d{4}\\.parquet$", full.names = TRUE)
  mref <- dplyr::bind_rows(lapply(mref_files, function(p) {
    m <- regmatches(basename(p), regexec("movement_ref_(MLB|AAA)_(\\d{4})", basename(p)))[[1]]
    tibble::as_tibble(arrow::read_parquet(p)) %>% dplyr::mutate(level = m[2], season = as.integer(m[3]))
  }))
  tagged <- function(prefix) {
    f <- list.files(dir, pattern = sprintf("^%s_(MLB|AAA)_\\d{4}\\.parquet$", prefix), full.names = TRUE)
    dplyr::bind_rows(lapply(f, function(p) {
      m <- regmatches(basename(p), regexec(sprintf("%s_(MLB|AAA)_(\\d{4})", prefix), basename(p)))[[1]]
      tibble::as_tibble(arrow::read_parquet(p)) %>% dplyr::mutate(level = m[2], season = as.integer(m[3]))
    }))
  }
  anchors_file <- file.path(dir, "grade_anchors.csv")
  grades <- read_outputs(dir, "grades")
  # Files built before the switch to 90% intervals carry 80% columns; show them until the
  # outputs are republished rather than failing.
  if (!is.null(grades) && "grade_lo80" %in% names(grades) && !"grade_lo90" %in% names(grades))
    grades <- dplyr::rename(grades, grade_lo90 = grade_lo80, grade_hi90 = grade_hi80)
  if (!is.null(grades) && nrow(grades))
    grades <- dplyr::mutate(grades, dplyr::across(dplyr::any_of(c("grade_type", "grade_global", "grade_lo90", "grade_hi90")), clamp_grade))
  list(grades = grades,
       profiles = tagged("profiles"), command = tagged("command"), tunnel = tagged("tunnel"),
       anchors = if (file.exists(anchors_file)) readr::read_csv(anchors_file, show_col_types = FALSE,
                   col_types = readr::cols(facet = "c", scope = "c", pitch_type = "c", mean = "d", unit = "d")) else NULL,
       components = read_outputs(dir, "components"),
       pitchers = read_outputs(dir, "pitchers"),
       movement_ref = mref, pctl = pctl,
       scorecard = list(md = file.path(dir, "scorecard.md"), chart = file.path(dir, "scorecard_chart.png")))
}

N_DISPLAY_BUCKETS <- 32L   # must equal the pipeline's N_BUCKETS (R/display.R); a test checks
display_bucket <- function(pitcher) as.integer(pitcher %% N_DISPLAY_BUCKETS)

# One cache for the whole R process (buckets are the same for every visitor), keeping
# the most recently used `max` buckets (~11 MB each at most) to bound memory.
bucket_cache <- function(max = 12L) {
  e <- new.env(); e$max <- max; e$items <- list(); e
}
cache_size <- function(cache) length(cache$items)
cache_has <- function(cache, key) !is.null(cache$items[[key]])
cache_get <- function(cache, key) {
  v <- cache$items[[key]]
  if (!is.null(v)) { cache$items[[key]] <- NULL; cache$items[[key]] <- v }   # most recent last
  v
}
cache_set <- function(cache, key, value) {
  cache$items[[key]] <- NULL
  cache$items[[key]] <- value
  while (length(cache$items) > cache$max) cache$items[[1]] <- NULL
  invisible(value)
}

# One pitcher's pitches, or NULL when they cannot be loaded (never an error).
fetch_display <- function(cfg, level, season, pitcher, cache) {
  k <- display_bucket(pitcher)
  key <- paste(level, season, k)
  b <- cache_get(cache, key)
  if (is.null(b)) {
    b <- tryCatch({
      if (cfg$local) {
        tibble::as_tibble(arrow::read_parquet(file.path(cfg$data_dir, sprintf("display/%s/%s/bucket_%d.parquet", level, season, k))))
      } else {
        if (!nzchar(cfg$data_token %||% "") || !nzchar(cfg$data_repo %||% "")) stop("PV_DATA_TOKEN or PV_DATA_REPO is not set")
        assets <- release_assets(cfg$data_repo, sprintf("display--%s--%s", level, season), cfg$data_token)
        a <- Filter(function(x) identical(x$name, sprintf("bucket_%d.parquet", k)), assets)
        if (length(a) == 0) stop("bucket not published")
        tmp <- tempfile(fileext = ".parquet")
        on.exit(unlink(tmp), add = TRUE)
        tibble::as_tibble(arrow::read_parquet(download_asset(a[[1]], tmp, cfg$data_token)))
      }
    }, error = function(e) {
      # Shows in the host's logs; the message never contains the token.
      message("Pitch data unavailable for ", level, " ", season, " bucket ", k, ": ", conditionMessage(e))
      NULL
    })
    if (!is.null(b)) cache_set(cache, key, b)
  }
  if (is.null(b)) return(NULL)
  b[b$pitcher == pitcher, ]
}
