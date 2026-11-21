# P3a component models (design approach B: lme4 + simulation). One binomial GLMM per
# component and horizon, forecasting target-season rates from information through the
# base season:
#   logit p = league(target) + f(past results, their sample size) + grade signals
#             + age spline + role + Triple-A share [+ window] + pitcher + pitcher-season change
# The past_rel x log1p(past_trials) interaction lets the weight on results grow with the
# sample, estimated rather than hand-set. The pitcher-season effect is the year-to-year
# talent change (plus season-level overdispersion) that the simulation carries forward.

GRADE_TERMS <- c("has_grades", "stuff", "location", "pitching", "x_whiff", "x_chase", "x_cstrike", "x_woba_contact", "x_gb")
# For the stuff-vs-results weight: only terms that carry a pitcher's quality count (not the
# has-grades indicator or the sample size on their own).
SIGNAL_WEIGHT_TERMS <- setdiff(GRADE_TERMS, "has_grades")
RESULT_WEIGHT_TERMS <- c("past_rel", "past_rel:log1p(past_trials)", "past_rel:aaa_share")

component_formula <- function(rows) {
  varies <- function(v) v %in% names(rows) && length(unique(rows[[v]])) > 1
  terms <- c("past_rel * log1p(past_trials)",
             GRADE_TERMS[vapply(GRADE_TERMS, varies, TRUE)],
             "splines::ns(age_target, df = 4)", if (varies("gs_share")) "gs_share",
             if (varies("aaa_share")) c("aaa_share", "past_rel:aaa_share"),
             if (varies("window")) "window")
  stats::as.formula(paste("cbind(events, trials - events) ~ offset(lg_off) +", paste(terms, collapse = " + "),
                          "+ (1 | pitcher) + (1 | pitcher_season)"))
}

fit_component <- function(rows, component) {
  rows <- rows[rows$trials >= 1 & !is.na(rows$events), ]
  rows$events <- pmin(round(rows$events), rows$trials)    # park-adjusted counts are not integers
  rows$trials <- round(rows$trials)
  # The season-change effect groups on one combined pitcher-season key: an interaction
  # with a one-level season factor (a single training season) breaks predict().
  rows$pitcher_season <- paste(rows$pitcher, rows$target_season, sep = "_")
  if ("window" %in% names(rows)) rows$window <- factor(rows$window)
  # The league offset is centered (the intercept absorbs its mean): glmer's PIRLS fails
  # to converge with a raw offset far from 0 (about -3.5 for home runs).
  lg_center <- mean(rows$lg_logit)
  rows$lg_off <- rows$lg_logit - lg_center
  f <- component_formula(rows)
  fit <- lme4::glmer(f, data = rows, family = stats::binomial(), nAGQ = 0,
                     control = lme4::glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
  m <- list(fit = fit, component = component, horizon = unique(rows$horizon), formula = f, lg_center = lg_center,
            windows = if ("window" %in% names(rows)) levels(rows$window) else NULL)
  m$unc <- fit_uncertainty(m, rows)
  m$x_means <- colMeans(lme4::getME(fit, "X"))
  m
}

# How uncertain the talent of a pitcher the fit has not seen is, given his past sample:
# on the training rows, the squared error of the fixed-effects prediction (no pitcher
# effect, i.e. as if unseen) minus binomial noise and the season-change variance, by
# bins of past trials, fitted as v(n) = a * kk / (kk + n) (large for small samples).
fit_uncertainty <- function(m, rows) {
  pr <- stats::predict(m$fit, re.form = NA, type = "link")
  p <- stats::plogis(pr)
  obs <- stats::qlogis((rows$events + 0.5) / (rows$trials + 1))
  excess <- (obs - pr)^2 - 1 / ((rows$trials + 1) * p * (1 - p)) - change_variance(m)
  bins <- dplyr::ntile(rows$past_trials, 10)
  b <- tibble::tibble(n = rows$past_trials, e = excess, bin = bins) %>% dplyr::group_by(bin) %>%
    dplyr::summarise(n = stats::median(n), e = mean(e), w = dplyr::n(), .groups = "drop")
  best <- c(a = 0, kk = 100); best_sse <- Inf
  for (kk in 10^seq(1, 4, by = 0.1)) {
    x <- kk / (kk + b$n)
    a <- max(sum(b$w * b$e * x) / sum(b$w * x^2), 0)
    sse <- sum(b$w * (b$e - a * x)^2)
    if (sse < best_sse) { best_sse <- sse; best <- c(a = a, kk = kk) }
  }
  best
}

talent_uncertainty <- function(m, past_trials) {
  if (is.null(m$unc)) return(rep(0, length(past_trials)))
  m$unc[["a"]] * m$unc[["kk"]] / (m$unc[["kk"]] + past_trials)
}

prep_newdata <- function(m, newdata) {
  newdata$pitcher_season <- paste(newdata$pitcher, newdata$target_season, sep = "_")
  newdata$lg_off <- newdata$lg_logit - m$lg_center
  if (!is.null(m$windows)) newdata$window <- factor(ifelse(newdata$window %in% m$windows, newdata$window,
                                                           m$windows[length(m$windows)]), levels = m$windows)
  newdata
}

# True-talent logit for each row: fixed part + the pitcher's own effect (0 for a pitcher
# the fit never saw). The season-change effect is left out (the simulation draws it).
predict_component <- function(m, newdata) {
  stats::predict(m$fit, newdata = prep_newdata(m, newdata), re.form = ~ (1 | pitcher),
                 allow.new.levels = TRUE, type = "link")
}

fixed_design <- function(m, newdata) {
  nd <- prep_newdata(m, newdata)
  nd$events <- 0; nd$trials <- 1
  tt <- stats::delete.response(stats::terms(m$fit, fixed.only = TRUE))
  X <- stats::model.matrix(tt, stats::model.frame(tt, nd, na.action = stats::na.pass))
  X[, colnames(X) %in% names(lme4::fixef(m$fit)), drop = FALSE]
}

# Share of how far a row's estimate moved from the typical training pitcher that comes
# from the pitch-model signals rather than from his results: each part is measured from
# the training means, |signals| / (|signals| + |results|), 0 when neither moves it.
stuff_weight <- function(m, newdata) {
  X <- fixed_design(m, newdata); b <- lme4::fixef(m$fit)[colnames(X)]
  mu <- if (!is.null(m$x_means)) m$x_means[colnames(X)] else rep(0, ncol(X))
  Xc <- sweep(X, 2, mu)
  part <- function(cols) { k <- colnames(Xc) %in% cols; if (!any(k)) rep(0, nrow(Xc)) else as.vector(Xc[, k, drop = FALSE] %*% b[k]) }
  g <- abs(part(SIGNAL_WEIGHT_TERMS)); r <- abs(part(RESULT_WEIGHT_TERMS))
  if ("has_grades" %in% names(newdata)) g[newdata$has_grades == 0] <- 0   # no signals, nothing from them
  ifelse(g + r > 0, g / (g + r), 0)
}

change_variance <- function(m) {
  vc <- as.data.frame(lme4::VarCorr(m$fit))
  v <- vc$vcov[vc$grp == "pitcher_season"]
  if (length(v)) v else 0
}

pitcher_variance <- function(m) {
  vc <- as.data.frame(lme4::VarCorr(m$fit))
  vc$vcov[vc$grp == "pitcher"]
}
