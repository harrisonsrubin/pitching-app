# Fake pitcher seasons: MLB 2012-2024, a tiny-sample rookie (9999), a mostly-AAA arm (8888)
sim_world <- function(n = 400, seed = 5) {
  set.seed(seed)
  out <- list()
  for (i in seq_len(n)) {
    start <- sample(2012:2022, 1); len <- sample(3:6, 1); seasons <- start:min(start + len - 1, 2024)
    if (i == 1) seasons <- 2018:2024  # the full-time veteran
    u <- rnorm(4, 0, c(0.25, 0.2, 0.2, 0.08)); age0 <- sample(22:30, 1)
    tbf <- if (i == 1) c(rep(700, length(seasons))) else sample(c(250, 700), 1)
    for (j in seq_along(seasons)) {
      t <- tbf[1]
      k <- rbinom(1, t, plogis(-1.4 + u[1])); bbhbp <- rbinom(1, t, plogis(-2.4 + u[2]))
      hr <- rbinom(1, t, plogis(-3.5 + u[3])); bip <- t - k - bbhbp - hr
      hbip <- rbinom(1, bip, plogis(-0.85 + u[4]))
      outs <- round(bip - hbip + k + 0.0)
      runs <- round(0.13 * (hbip + bbhbp) + 1.4 * hr + 0.05 * outs)
      out[[length(out) + 1]] <- tibble::tibble(pitcher = i, season = seasons[j], level = "MLB", age = age0 + j - 1,
        age_missing = FALSE, tbf = t, k = k, bbhbp = bbhbp, hr = hr, bip = bip, hbip = hbip, outs = outs,
        r = runs, er = round(0.92 * runs), h = hbip + hr, bb = bbhbp, hbp = 0, gs_share = 0.5)
    }
  }
  lines <- dplyr::bind_rows(out)
  rookie <- tibble::tibble(pitcher = 9999L, season = 2024L, level = "MLB", age = 23, age_missing = FALSE, tbf = 36,
                           k = 9, bbhbp = 4, hr = 1, bip = 22, hbip = 7, outs = 24, r = 4, er = 4, h = 8, bb = 4, hbp = 0, gs_share = 0)
  aaa <- tibble::tibble(pitcher = 8888L, season = 2024L, level = c("AAA", "MLB"), age = 25, age_missing = FALSE,
                        tbf = c(300, 100), k = c(80, 22), bbhbp = c(25, 9), hr = c(8, 3), bip = c(187, 66), hbip = c(55, 20),
                        outs = c(220, 70), r = c(30, 12), er = c(28, 11), h = c(63, 23), bb = c(25, 9), hbp = 0, gs_share = 1)
  dplyr::bind_rows(lines, rookie, aaa)
}

