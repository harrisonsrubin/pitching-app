test_that("corrections relabel and REMOVED drops", {
  d <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-2", "1-1-3"), pitch_type = c("4S", "SL", "CH"))
  corr <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-3"), new_pitch_type = c("CUT", "REMOVED"))
  out <- apply_corrections(d, corr)
  expect_equal(out$pitch_uid, c("1-1-1", "1-1-2"))
  expect_equal(out$pitch_type, c("CUT", "SL"))
})

test_that("outcome flags", {
  f <- outcome_flags(c("swinging_strike", "foul", "called_strike", "hit_into_play", "ball"))
  expect_equal(f$swing, c(1L, 1L, 0L, 1L, 0L))
  expect_equal(f$whiff, c(1L, 0L, 0L, 0L, 0L))
  expect_equal(f$take, c(0L, 0L, 1L, 0L, 1L))
  expect_equal(f$called_strike, c(0L, 0L, 1L, 0L, 0L))
  expect_equal(f$in_play, c(0L, 0L, 0L, 1L, 0L))
})

test_that("pitchouts, intentional and automatic pitches are excluded", {
  d <- tibble::tibble(description = c("ball", "pitchout", "intent_ball", "automatic_ball", "ball"),
                      pitch_type = c("4S", "4S", "4S", "4S", NA))
  expect_equal(nrow(exclude_non_competitive(d)), 1)
})

test_that("corrections written as Savant codes map to app labels; unknown labels stop", {
  d <- tibble::tibble(pitch_uid = c("1-1-1", "1-1-2"), pitch_type = c("4S", "SL"))
  out <- apply_corrections(d, tibble::tibble(pitch_uid = "1-1-1", new_pitch_type = "SC"))
  expect_equal(out$pitch_type, c("CH", "SL"))
  expect_error(apply_corrections(d, tibble::tibble(pitch_uid = "1-1-1", new_pitch_type = "XX")), "XX")
})
