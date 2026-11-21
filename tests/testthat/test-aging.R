sim_careers <- function(n = 3000, slope = 0.02, quit_slope = NULL, seed = 3) {
  set.seed(seed)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    start <- sample(21:30, 1); len <- sample(4:12, 1)
    base <- rnorm(1, -1.4, 0.3)
    my_slope <- if (is.null(quit_slope)) slope else sample(c(slope, quit_slope), 1)
    ages <- start:(start + len - 1)
    seasons <- 2010L + seq_along(ages) - 1L + sample(0:3, 1)
    keep <- seasons <= 2026L
    ages <- ages[keep]; seasons <- seasons[keep]
    p <- plogis(base - my_slope * pmax(ages - 27, 0))
    tbf <- rep(600, length(ages))
    if (!is.null(quit_slope) && my_slope == quit_slope) {   # steep decliners stop being seen after 31
      stay <- ages <= 31; ages <- ages[stay]; seasons <- seasons[stay]; p <- p[stay]; tbf <- tbf[stay]
    }
    if (!length(ages)) next
    out[[i]] <- tibble::tibble(pitcher = i, season = seasons, level = "MLB", age = ages, tbf = tbf,
                               k = stats::rbinom(length(ages), tbf, p))
  }
  dplyr::bind_rows(out) %>% dplyr::mutate(bip = tbf, bbhbp = 0, hr = 0, hbip = 0)
}

test_that("the aging curve recovers a known decline after 27", {
  cur <- aging_curve(sim_careers(), "k")
  expect_equal(cur$cum[cur$age == 27], 0)
  expect_equal(cur$cum[cur$age == 35], -0.16, tolerance = 0.05 / 0.16)
  expect_true(all(c(20, 42) %in% cur$age))
})

test_that("pitchers who stop being seen pull the corrected curve down", {
  lines <- sim_careers(quit_slope = 0.10)
  raw <- aging_curve(lines, "k", correct = FALSE)
  cor <- aging_curve(lines, "k", correct = TRUE)
  expect_lt(cor$cum[cor$age == 34], raw$cum[raw$age == 34])
})

test_that("aging deltas are per age with weights", {
  d <- aging_deltas(sim_careers(n = 300), "k")
  expect_true(all(c("age", "delta", "n") %in% names(d)))
  expect_false(any(is.na(d$delta)))
})
