# Denoised run-value target (README 7.1): delta_run_exp, except that balls in play
# are replaced by the run value expected from their xwOBA. That strips defense, park,
# and batted-ball luck out of what the pitch models learn. Units: runs from the
# batting team's view (lower = better for the pitcher).

# Per count: OLS of delta_run_exp on xwOBA over balls in play, plus the count's mean
# in-play run value as a fallback for balls in play with no xwOBA.
fit_bip_xrv <- function(d) {
  bip <- d[d$in_play == 1 & !is.na(d$delta_run_exp), ]
  bip %>%
    dplyr::group_by(balls, strikes) %>%
    dplyr::group_modify(function(g, key) {
      ok <- !is.na(g$estimated_woba_using_speedangle)
      cf <- stats::coef(stats::lm(delta_run_exp ~ estimated_woba_using_speedangle, data = g[ok, ]))
      tibble::tibble(intercept = unname(cf[1]), slope = unname(cf[2]), bip_mean = mean(g$delta_run_exp))
    }) %>%
    dplyr::ungroup()
}

denoised_target <- function(d, bip) {
  i <- match(paste(d$balls, d$strikes), paste(bip$balls, bip$strikes))
  xw <- d$estimated_woba_using_speedangle
  bip_value <- ifelse(is.na(xw), bip$bip_mean[i], bip$intercept[i] + bip$slope[i] * xw)
  ifelse(d$in_play == 1, bip_value, d$delta_run_exp)
}
