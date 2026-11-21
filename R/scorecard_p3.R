# P3 scorecard helpers (README 9): range coverage, CRPS from simulation draws, and
# calibration of the Step forward / Step back odds.

coverage <- function(lo, hi, actual) {
  ok <- !is.na(lo) & !is.na(hi) & !is.na(actual)
  if (!any(ok)) return(NA_real_)
  mean(actual[ok] >= lo[ok] & actual[ok] <= hi[ok])
}

# Sample CRPS: E|X - y| - E|X - X'| / 2 over the draws X.
crps_draws <- function(draws, actual) {
  draws <- draws[!is.na(draws)]
  mean(abs(draws - actual)) - 0.5 * mean(abs(outer(draws, draws, "-")))
}

step_calibration <- function(pred, realized, breaks = c(0, 0.05, 0.1, 0.2, 0.3, 1)) {
  bin <- cut(pred, breaks, include.lowest = TRUE)
  tibble::tibble(bin = bin, pred = pred, real = as.numeric(realized)) %>%
    dplyr::filter(!is.na(bin), !is.na(real)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(predicted = mean(pred), observed = mean(real), n = dplyr::n(), .groups = "drop")
}
