ed_cfg <- function() list(local = FALSE, app_repo = "o/pitching-app", corrections_token = "write-token",
                          admin_hash = bcrypt::hashpw("open sesame"))

corr_csv <- function(rows) {
  paste(c("pitch_uid,play_id,new_pitch_type,original_pitch_type,corrected_at,corrected_by",
          rows), collapse = "\n")
}

# Mock GitHub contents API. `puts` statuses are returned in order for successive PUTs.
gh_mock <- function(existing = NULL, puts = 201L) {
  st <- new.env(); st$puts <- list(); st$gets <- 0L; st$put_status <- puts
  httr2::local_mocked_responses(function(req) {
    method <- req$method %||% (if (is.null(req$body)) "GET" else "PUT")
    if (identical(method, "PUT")) {
      b <- req$body$data
      if (is.raw(b)) b <- jsonlite::fromJSON(rawToChar(b))
      st$puts[[length(st$puts) + 1]] <- b
      s <- st$put_status[min(length(st$puts), length(st$put_status))]
      return(httr2::response(status_code = s, headers = list(`Content-Type` = "application/json"), body = charToRaw("{}")))
    }
    st$gets <- st$gets + 1L
    if (is.null(existing)) return(httr2::response(status_code = 404))
    accept <- httr2::req_get_headers(req)$Accept %||% ""
    if (grepl("raw", accept)) return(httr2::response(status_code = 200, body = charToRaw(existing)))
    httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(sprintf('{"sha":"sha%d"}', st$gets)))
  }, env = parent.frame())
  st
}

new_rows <- function() tibble::tibble(pitch_uid = c("1-1-1", "1-2-1"), new_pitch_type = "SL", original_pitch_type = "CUT")

test_that("wrong password fails and 5 failures lock", {
  source_app()
  cfg <- ed_cfg()
  expect_false(check_password("nope", cfg$admin_hash))
  expect_true(check_password("open sesame", cfg$admin_hash))
  expect_false(check_password("open sesame", ""))
  shiny::testServer(adminServer, args = list(cfg = cfg), {
    for (i in 1:5) session$setInputs(pw = "nope", login = i)
    session$setInputs(pw = "open sesame", login = 6)
    expect_false(is_admin())
    expect_match(output$status$html, "Too many attempts")
    expect_false(grepl(cfg$admin_hash, output$status$html, fixed = TRUE))
  })
  admin_lock_reset()
  shiny::testServer(adminServer, args = list(cfg = cfg), {
    session$setInputs(pw = "open sesame", login = 1)
    expect_true(is_admin())
  })
})

test_that("upsert keeps latest per pitch_uid", {
  source_app()
  old <- tibble::tibble(pitch_uid = c("1-1-1", "9-9-9"), play_id = NA_character_, new_pitch_type = c("CH", "4S"),
                        original_pitch_type = "CUT", corrected_at = "2026-01-01T00:00:00Z", corrected_by = "admin")
  u <- upsert_corrections(old, new_rows(), now = as.POSIXct("2026-11-20 12:00:00", tz = "UTC"))
  expect_equal(sort(u$pitch_uid), c("1-1-1", "1-2-1", "9-9-9"))
  expect_equal(u$new_pitch_type[u$pitch_uid == "1-1-1"], "SL")
  expect_equal(u$corrected_at[u$pitch_uid == "1-1-1"], "2026-11-20T12:00:00Z")
  expect_named(u, c("pitch_uid", "play_id", "new_pitch_type", "original_pitch_type", "corrected_at", "corrected_by"))
})

test_that("unknown label and unknown pitch are rejected", {
  source_app()
  d <- fixture_display()
  ok <- tibble::tibble(pitch_uid = d$pitch_uid[1], new_pitch_type = "REMOVED", original_pitch_type = "4S")
  expect_length(validate_corrections(ok, d), 0)
  bad <- tibble::tibble(pitch_uid = c(d$pitch_uid[1], "0-0-0"), new_pitch_type = c("SC", "SL"), original_pitch_type = "4S")
  msg <- validate_corrections(bad, d)
  expect_true(any(grepl("SC", msg))); expect_true(any(grepl("0-0-0", msg)))
})

test_that("save creates the file on 404", {
  source_app()
  st <- gh_mock(existing = NULL)
  r <- save_corrections(new_rows(), 2026L, ed_cfg())
  expect_true(r$ok)
  expect_length(st$puts, 1)
  expect_null(st$puts[[1]]$sha)
  csv <- rawToChar(jsonlite::base64_dec(st$puts[[1]]$content))
  expect_match(csv, "^pitch_uid,play_id,new_pitch_type")
  expect_match(csv, "1-2-1,,SL,CUT")
})

test_that("save retries once on 409 then succeeds", {
  source_app()
  st <- gh_mock(existing = corr_csv("5-5-5,,CH,4S,2026-01-01T00:00:00Z,admin"), puts = c(409L, 200L))
  r <- save_corrections(new_rows(), 2026L, ed_cfg())
  expect_true(r$ok)
  expect_length(st$puts, 2)
  expect_false(identical(st$puts[[1]]$sha, st$puts[[2]]$sha))   # re-read before retrying
  csv <- rawToChar(jsonlite::base64_dec(st$puts[[2]]$content))
  expect_match(csv, "5-5-5")   # the other change is kept
})

test_that("save gives up after a second 409 without a third PUT", {
  source_app()
  st <- gh_mock(existing = corr_csv(character()), puts = c(409L, 409L, 200L))
  r <- save_corrections(new_rows(), 2026L, ed_cfg())
  expect_false(r$ok)
  expect_length(st$puts, 2)
  expect_match(r$message, "changed")
})

test_that("save refuses a non-corrections path", {
  source_app()
  expect_error(github_put_file("app/app.R", "x", NULL, "m", ed_cfg()), "corrections")
  expect_equal(corrections_path(2026L), "corrections/pitch_corrections_2026.csv")
})

test_that("apply_selection relabels and REMOVED drops", {
  source_app()
  d <- fixture_display()
  u <- d$pitch_uid[1:3]
  r <- apply_selection(d, u[1:2], "CUT")
  expect_true(all(r$pitch_type[r$pitch_uid %in% u[1:2]] == "CUT"))
  r2 <- apply_selection(d, u[3], "REMOVED")
  expect_false(u[3] %in% r2$pitch_uid)
  e <- tibble::tibble(pitch_uid = u[1], new_pitch_type = "SP")
  expect_equal(apply_selection_edits(d, e)$pitch_type[1], "SP")
})

test_that("editor is locked until admin", {
  local_app_fixture()
  cfg <- ed_cfg()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All")))
  d <- fixture_display()
  display <- list(rows = shiny::reactive(d[d$pitcher == 101, ]),
                  edits = shiny::reactiveVal(tibble::tibble(pitch_uid = character(), new_pitch_type = character())))
  shiny::testServer(editorServer, args = list(cfg = cfg, sel = sel, display = display, outputs = load_outputs(app_config())), {
    expect_match(output$panel$html, "Admin password")
    session$setInputs(`admin-pw` = "open sesame", `admin-login` = 1)
    expect_match(output$panel$html, "Save")
  })
})

test_that("removed pitches of this pitcher are listed for undo", {
  source_app()
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  pa <- sub("-\\d+$", "", d$pitch_uid[1])
  corr <- tibble::tibble(pitch_uid = c(paste0(pa, "-9"), "999-1-1", d$pitch_uid[2]), play_id = NA,
                         new_pitch_type = c("REMOVED", "REMOVED", "SL"), original_pitch_type = c("4S", "CH", "4S"),
                         corrected_at = "2026-01-01T00:00:00Z", corrected_by = "admin")
  r <- removed_for_pitcher(corr, d)
  expect_equal(r$pitch_uid, paste0(pa, "-9"))
  expect_equal(r$original_pitch_type, "4S")
})

