# P3a aging (design: aging from within-pitcher changes). For each component, the mean
# change in logit rate from age a to a+1 among MLB pitchers seen in consecutive seasons,
# weighted by the harmonic mean of the two seasons' trials, then smoothed and summed into
# a curve that is 0 at age 27.
#
# Survivor correction (builder's choice, Lichtman-style "phantom season"): a pitcher with
# a qualifying season s who has no qualifying season s+1, while season s+1 exists in the
# data, left the league or the sample. He gets an imputed next season equal to his
# age group's mean change minus half that group's SD of changes, so the decline of
# pitchers who disappear is not lost. Careers that end at the last season in the data are
# censored, not dropouts, and get nothing imputed.

AGE_RANGE <- 20:42

aging_deltas <- function(lines, component, min_tbf = 100, correct = TRUE) {
  tcol <- component_trials(component)
  l <- lines[lines$level == "MLB" & lines[[tcol]] >= min_tbf, ]
  l$trials <- l[[tcol]]
  l$lg <- stats::qlogis((l[[component]] + 0.5) / (l$trials + 1))
  l <- l[, c("pitcher", "season", "age", "trials", "lg")]
  nxt <- dplyr::mutate(l, season = season - 1L) %>% dplyr::rename(trials2 = trials, lg2 = lg, age2 = age)
  pairs <- dplyr::inner_join(l, nxt, by = c("pitcher", "season")) %>%
    dplyr::mutate(delta = lg2 - lg, w = 2 / (1 / trials + 1 / trials2), age = floor(age))
  by_age <- pairs %>% dplyr::group_by(age) %>%
    dplyr::summarise(m = stats::weighted.mean(delta, w), s = stats::sd(delta), .groups = "drop")
  if (correct) {
    last_season <- max(lines$season)
    drops <- dplyr::anti_join(l, nxt, by = c("pitcher", "season")) %>%
      dplyr::filter(season < last_season) %>%
      dplyr::mutate(age = floor(age)) %>%
      dplyr::inner_join(by_age, by = "age") %>%
      dplyr::transmute(age, delta = m - 0.5 * dplyr::coalesce(s, 0), w = trials / 2)
    pairs <- dplyr::bind_rows(dplyr::select(pairs, age, delta, w), drops)
  }
  pairs %>% dplyr::group_by(age) %>%
    dplyr::summarise(delta = stats::weighted.mean(delta, w), n = sum(w), .groups = "drop") %>%
    dplyr::filter(!is.na(delta))
}

aging_curve <- function(lines, component, correct = TRUE) {
  d <- aging_deltas(lines, component, correct = correct)
  fit <- stats::lm(delta ~ splines::ns(age, df = 4), data = d, weights = n)
  ages <- AGE_RANGE
  step <- stats::predict(fit, newdata = data.frame(age = pmin(pmax(ages, min(d$age)), max(d$age))))
  names(step) <- ages
  cum <- vapply(ages, function(a) {
    if (a == 27) 0
    else if (a > 27) sum(step[as.character(27:(a - 1))])
    else -sum(step[as.character(a:26)])
  }, 0)
  tibble::tibble(age = ages, cum = unname(cum))
}
