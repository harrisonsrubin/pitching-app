corr_cols <- c("pitch_uid", "play_id", "new_pitch_type", "original_pitch_type", "corrected_at", "corrected_by")

test_that("latest correction wins", {
  df <- tibble::tibble(play_id = c("a", "a"), new_pitch_type = c("SL", "CB"),
                       original_pitch_type = "SL",
                       corrected_at = as.POSIXct(c("2026-01-01", "2026-02-01"), tz = "UTC"))
  out <- dedupe_corrections(df)
  expect_equal(nrow(out), 1)
  expect_equal(out$new_pitch_type, "CB")
})

test_that("old correction files parse fractional-second timestamps", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("play_id,new_pitch_type,original_pitch_type,corrected_at",
               "a,SL,SW,2026-03-02 21:08:48.953971"), p)
  d <- read_old_corrections(p)
  expect_s3_class(d$corrected_at, "POSIXct")
  expect_equal(format(d$corrected_at, "%Y-%m-%d %H:%M:%S"), "2026-03-02 21:08:48")
})

test_that("unmapped play_ids are reported, not dropped; schema is fixed", {
  old <- tibble::tibble(play_id = c("a", "zzz"), new_pitch_type = "CB", original_pitch_type = "SL",
                        corrected_at = as.POSIXct("2026-01-01", tz = "UTC"))
  idx <- tibble::tibble(play_id = "a", pitch_uid = "1-1-1")
  m <- migrate_corrections(old, idx)
  expect_equal(nrow(m$mapped), 1)
  expect_equal(m$unmapped$play_id, "zzz")
  expect_equal(names(m$mapped), corr_cols)
  expect_equal(m$mapped$corrected_by, "admin")
})

test_that("play_id index converts 0-based atBatIndex to 1-based at_bat_number", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("game_pk,x,play_id,pitchNumber,atBatIndex,y",
               "777,1,abc,3,0,9",
               "777,1,def,1,4,9"), p)
  idx <- play_id_index(p)
  expect_equal(idx$pitch_uid[idx$play_id == "abc"], "777-1-3")
  expect_equal(idx$pitch_uid[idx$play_id == "def"], "777-5-1")
})

test_that("verification compares original type to the mapped Savant type", {
  mapped <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-2", "9-9-9"),
                           original_pitch_type = c("SW", "4S", "SL"))
  sc <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-2"), pitch_type = c("ST", "SI"))
  v <- verify_against_statcast(mapped, sc)
  expect_equal(v$n_found, 2L)
  expect_equal(v$match_rate, 0.5)
  expect_equal(v$mismatches$pitch_uid, "1-1-2")
})

test_that("rows with no original type (e.g. REMOVED tags) are left out of the match rate", {
  mapped <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-2"),
                           original_pitch_type = c("SW", NA))
  sc <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-2"), pitch_type = c("ST", "SI"))
  v <- verify_against_statcast(mapped, sc)
  expect_equal(v$n_found, 1L)
  expect_equal(v$match_rate, 1)
})
