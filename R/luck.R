# P3a luck gap (design: actual ERA minus expected, attributed to BABIP, strand rate and
# HR/FB). Expected ERA = the runs map at the pitcher's true-talent rates, put in his parks
# (home runs and hits on balls in play scaled by his park factors, runs by his run park
# factor), times the league's earned/total run ratio. MLB only: Triple-A results do not
# compare with MLB-equivalent talent. Pieces, in ERA:
#   babip  = runs-map BABIP slope x (actual - expected BABIP)
#   hr_fb  = runs-map HR slope x (actual - expected HR rate): home runs beyond his talent,
#            shown only where Statcast fly-ball counts exist (else NA, folded into other)
#   strand = runs from stranding more or fewer runners than his K and BB+HBP rates predict
#   other  = the rest (sequencing, defense, unearned runs, runs-map error), so the pieces
#            always add up to the gap.

LUCK_MIN_OUTS <- 60L   # 20 innings: below that one bad outing is the whole "gap"

lob_pct <- function(h, bb, hbp, r, hr) {
  den <- h + bb + hbp - 1.4 * hr
  ifelse(den > 0, (h + bb + hbp - r) / den, NA_real_)
}

fit_strand <- function(lines) {
  l <- lines[lines$level == "MLB" & lines$outs >= 150, ]
  l$lob <- lob_pct(l$h, l$bb, l$hbp, l$r, l$hr)
  l$k_rate <- l$k / l$tbf; l$bbhbp_rate <- l$bbhbp / l$tbf
  stats::lm(lob ~ k_rate + bbhbp_rate, data = l[is.finite(l$lob), ], weights = outs)
}

luck_gap <- function(line, expected, map, er_r, strand, fb = NULL, parks = NULL) {
  b <- stats::coef(map)
  d <- line %>% dplyr::filter(outs >= LUCK_MIN_OUTS) %>%
    dplyr::inner_join(dplyr::rename_with(expected, ~ paste0("x_", .x), -pitcher), by = "pitcher")
  if (!is.null(fb) && nrow(fb)) d <- dplyr::left_join(d, dplyr::select(fb, pitcher, fb), by = "pitcher") else d$fb <- NA_real_
  if (!is.null(parks) && nrow(parks)) {
    d <- dplyr::left_join(d, dplyr::select(parks, pitcher, ppf_runs, ppf_hr, ppf_hbip), by = "pitcher")
  } else { d$ppf_runs <- NA_real_; d$ppf_hr <- NA_real_; d$ppf_hbip <- NA_real_ }
  d %>% dplyr::mutate(
    ppf_runs = dplyr::coalesce(ppf_runs, 1), x_hr = x_hr * dplyr::coalesce(ppf_hr, 1), x_hbip = x_hbip * dplyr::coalesce(ppf_hbip, 1),
    actual_era = 27 * er / outs,
    expected_era = ra9_from_rates(map, x_k, x_bbhbp, x_hr / dplyr::coalesce(ppf_hr, 1), x_hbip / dplyr::coalesce(ppf_hbip, 1)) * ppf_runs * er_r,
    gap = actual_era - expected_era,
    babip = ifelse(bip > 0, b[["babip"]] * (hbip / bip - x_hbip) * er_r, 0),
    hr_fb = ifelse(is.na(fb), NA_real_, b[["hr_r"]] * (hr / tbf - x_hr) * er_r),
    lob_act = lob_pct(h, bb, hbp, r, hr),
    lob_exp = stats::predict(strand, newdata = data.frame(k_rate = k / tbf, bbhbp_rate = bbhbp / tbf)),
    strand = ifelse(is.na(lob_act), 0, -(lob_act - lob_exp) * (h + bb + hbp - 1.4 * hr) * 27 / outs * er_r),
    other = gap - babip - strand - dplyr::coalesce(hr_fb, 0)) %>%
    dplyr::select(pitcher, season, level, actual_era, expected_era, gap, babip, strand, hr_fb, other)
}
