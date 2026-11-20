test_that("PITCH_LABELS match the repo's labels and every label has a color", {
  source_app()
  expect_setequal(PITCH_LABELS, unique(pitch_type_table()$label))
  expect_setequal(names(PITCH_COLORS), PITCH_LABELS)
})

test_that("grade_color bins", {
  source_app()
  expect_equal(grade_color(c(70, 65, 57, 50, 42, 35, 29.9, NA)),
               c("#1a7f37", "#8fd19e", "#e6f4c3", "#fff7cc", "#fdd49e", "#f4a582", "#c0392b", NA))
})

test_that("local mode loads fixture outputs", {
  local_app_fixture()
  o <- load_outputs(app_config())
  expect_equal(sort(unique(o$grades$pitcher)), c(101L, 102L, 103L))
  expect_equal(nrow(o$pitchers), 3)
  expect_true(file.exists(o$scorecard$md)); expect_true(file.exists(o$scorecard$chart))
  expect_true(all(c("movement_ref", "pctl", "components") %in% names(o)))
})

test_that("fetch_display returns only that pitcher and caches the bucket", {
  dir <- local_app_fixture()
  cfg <- app_config(); cache <- bucket_cache()
  d <- fetch_display(cfg, "MLB", 2026L, 102L, cache)
  expect_equal(unique(d$pitcher), 102L)
  unlink(file.path(dir, "display"), recursive = TRUE)
  d2 <- fetch_display(cfg, "MLB", 2026L, 102L, cache)   # served from the cache
  expect_equal(nrow(d2), nrow(d))
  expect_null(fetch_display(cfg, "MLB", 2026L, 101L, bucket_cache()))   # files gone, no cache: NULL, no error
})

remote_cfg <- function(token = "secret-token") {
  list(local = FALSE, data_dir = NA, app_repo = "o/pitching-app", data_repo = "o/pitching-data", data_token = token)
}

test_that("fetch_display returns NULL without a token", {
  source_app()
  httr2::local_mocked_responses(function(req) stop("no request should be made"))
  expect_null(fetch_display(remote_cfg(""), "MLB", 2026L, 102L, bucket_cache()))
})

test_that("fetch_display returns NULL on HTTP 500", {
  source_app()
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 500))
  expect_null(fetch_display(remote_cfg(), "MLB", 2026L, 102L, bucket_cache()))
})

test_that("fetch_display downloads the pitcher's bucket from the private release with the token", {
  source_app()
  bucket <- tempfile(fileext = ".parquet")
  arrow::write_parquet(fixture_display()[fixture_display()$pitcher == 102, ], bucket)
  seen <- new.env(); seen$auth <- character(); seen$urls <- character()
  httr2::local_mocked_responses(function(req) {
    seen$urls <- c(seen$urls, req$url)
    seen$auth <- c(seen$auth, httr2::req_get_headers(req, "reveal")$Authorization %||% "")
    if (grepl("/releases/tags/display--MLB--2026$", req$url)) {
      body <- jsonlite::toJSON(list(assets = list(
        list(name = "bucket_6.parquet", url = "https://api.github.com/repos/o/pitching-data/releases/assets/66"),
        list(name = "bucket_7.parquet", url = "https://api.github.com/repos/o/pitching-data/releases/assets/77"))),
        auto_unbox = TRUE)
      return(httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"), body = charToRaw(body)))
    }
    if (grepl("/assets/66$", req$url))
      return(httr2::response(status_code = 200, body = readBin(bucket, "raw", file.size(bucket))))
    httr2::response(status_code = 404)
  })
  d <- fetch_display(remote_cfg(), "MLB", 2026L, 102L, bucket_cache())   # 102 %% 32 = 6
  expect_equal(unique(d$pitcher), 102L)
  expect_true(all(grepl("^Bearer secret-token$", seen$auth)))
  expect_true(any(grepl("/assets/66$", seen$urls)))
})

test_that("token never appears in returned objects", {
  local_app_fixture()
  withr::local_envvar(PV_DATA_TOKEN = "secret-token")
  o <- load_outputs(app_config())
  d <- fetch_display(app_config(), "MLB", 2026L, 101L, bucket_cache())
  txt <- paste(capture.output(str(o), str(d)), collapse = "\n")
  expect_false(grepl("secret-token", txt, fixed = TRUE))
})

test_that("outputs listing retries a rate-limited call and sends the token", {
  source_app()
  calls <- new.env(); calls$n <- 0L; calls$auth <- character()
  httr2::local_mocked_responses(function(req) {
    if (grepl("/releases/tags/outputs$", req$url)) {
      calls$n <- calls$n + 1L
      calls$auth <- c(calls$auth, httr2::req_get_headers(req, "reveal")$Authorization %||% "")
      if (calls$n == 1L) return(httr2::response(status_code = 403))
      return(httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"), body = charToRaw('{"assets":[]}')))
    }
    httr2::response(status_code = 404)
  })
  cfg <- list(local = FALSE, app_repo = "o/pitching-app", data_token = "tok")
  a <- release_assets(cfg$app_repo, "outputs", cfg$data_token, tries = 3, backoff = function(i) 0)
  expect_length(a, 0)
  expect_equal(calls$n, 2L)
  expect_true(all(calls$auth == "Bearer tok"))
})

test_that("the bucket cache is shared and capped", {
  local_app_fixture()
  cache <- bucket_cache(max = 2)
  cfg <- app_config()
  for (p in c(101L, 102L, 103L)) fetch_display(cfg, "MLB", 2026L, p, cache)   # three different buckets
  expect_equal(cache_size(cache), 2L)
  expect_false(cache_has(cache, "MLB 2026 5"))   # 101 %% 32: the oldest, evicted
})
