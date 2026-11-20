# Grades (README 7.6). Pitch-level xRV is aggregated to pitcher × pitch type with
# hierarchical shrinkage: a pitcher effect shared across his whole arsenal plus a
# pitcher-by-pitch-type effect, so what a pitcher shows on his main pitches informs
# his low-sample ones. Season-only fits (no carry-over prior), so stickiness is not
# inflated by construction. 20-80 scale: 50 = reference-season average, 10 points =
# one standard deviation of estimated true talent (MLB 2023–2025).

Z80 <- stats::qnorm(0.9)   # 1.2816: half-width of an 80% interval in SDs

fit_grades <- function(x, col) {
  x <- x[!is.na(x[[col]]) & !is.na(x$pitch_type), ]
  df <- data.frame(y = x[[col]], pitch_type = factor(x$pitch_type),
                   pitcher = factor(x$pitcher), pt = factor(paste(x$pitcher, x$pitch_type, sep = "_")))
  m <- lme4::lmer(y ~ 0 + pitch_type + (1 | pitcher) + (1 | pt), data = df, REML = TRUE,
                  control = lme4::lmerControl(calc.derivs = FALSE))
  fe <- lme4::fixef(m); names(fe) <- sub("^pitch_type", "", names(fe))
  re <- as.data.frame(lme4::ranef(m, condVar = TRUE))
  rp <- re[re$grpvar == "pitcher", ]; rpt <- re[re$grpvar == "pt", ]
  vc <- as.data.frame(lme4::VarCorr(m))
  varcomp <- c(sigma2_p = vc$vcov[vc$grp == "pitcher"], sigma2_pt = vc$vcov[vc$grp == "pt"],
               sigma2_e = vc$vcov[vc$grp == "Residual"])

  by_type <- x %>%
    dplyr::count(pitcher, pitch_type) %>%
    dplyr::mutate(key = paste(pitcher, pitch_type, sep = "_"))
  ip <- match(as.character(by_type$pitcher), as.character(rp$grp))
  ipt <- match(by_type$key, as.character(rpt$grp))
  by_type <- by_type %>%
    dplyr::mutate(fe = unname(fe[pitch_type]),
                  u_p = rp$condval[ip], sd_p = rp$condsd[ip],
                  u_pt = rpt$condval[ipt], sd_pt = rpt$condsd[ipt],
                  est = fe + u_p + u_pt,
                  se = sqrt(sd_p^2 + sd_pt^2))
  by_pitcher <- by_type %>%
    dplyr::group_by(pitcher) %>%
    dplyr::mutate(w = n / sum(n)) %>%
    dplyr::summarise(est = sum(w * (fe + u_pt)) + dplyr::first(u_p),
                     se = sqrt(dplyr::first(sd_p)^2 + sum(w^2 * sd_pt^2)),
                     sum_w2 = sum(w^2), n = sum(n), .groups = "drop")
  list(by_type = dplyr::select(by_type, pitcher, pitch_type, n, est, se),
       by_pitcher = dplyr::select(by_pitcher, pitcher, n, est, se, sum_w2),
       varcomp = varcomp, fixef = fe,
       mean_all = mean(x[[col]]), n_all = nrow(x))
}

# Anchors from reference-season fits of one facet: list of fit_grades() results.
grade_anchors <- function(fits, facet) {
  s2p <- mean(sapply(fits, function(f) f$varcomp[["sigma2_p"]]))
  s2pt <- mean(sapply(fits, function(f) f$varcomp[["sigma2_pt"]]))
  unit_type <- sqrt(s2p + s2pt)
  bp <- dplyr::bind_rows(lapply(fits, `[[`, "by_pitcher"))
  mean_w2 <- stats::weighted.mean(bp$sum_w2, bp$n)
  unit_pitcher <- sqrt(s2p + s2pt * mean_w2)
  global_mean <- sum(sapply(fits, function(f) f$mean_all * f$n_all)) / sum(sapply(fits, `[[`, "n_all"))
  fe <- dplyr::bind_rows(lapply(fits, function(f) tibble::tibble(pitch_type = names(f$fixef), fe = unname(f$fixef))))
  type_means <- fe %>% dplyr::group_by(pitch_type) %>% dplyr::summarise(mean = mean(fe), .groups = "drop")
  dplyr::bind_rows(
    tibble::tibble(facet = facet, scope = "type", pitch_type = type_means$pitch_type,
                   mean = type_means$mean, unit = unit_type),
    tibble::tibble(facet = facet, scope = "global", pitch_type = NA_character_, mean = global_mean, unit = unit_type),
    tibble::tibble(facet = facet, scope = "pitcher", pitch_type = NA_character_, mean = global_mean, unit = unit_pitcher)
  )
}

# Lower expected run value is better for the pitcher, so the scale inverts.
to_grade <- function(est, mean, unit) 50 - 10 * (est - mean) / unit

# Expected component rates (README 7.4), raw means; P3 shrinks them.
component_rates <- function(x) {
  summarise_rates <- function(g) {
    g %>% dplyr::summarise(
      n = dplyr::n(),
      x_swing = mean(p_swing),
      x_whiff_per_swing = sum(p_swing * p_whiff) / sum(p_swing),
      x_chase = mean(p_swing[edge_dist > 0]),
      x_cstrike_per_take = sum((1 - p_swing) * p_cstrike) / sum(1 - p_swing),
      x_woba_contact = stats::weighted.mean(xwoba_contact, p_swing * (1 - p_whiff)),
      x_gb = stats::weighted.mean(p_gb, p_swing * (1 - p_whiff)),
      .groups = "drop")
  }
  dplyr::bind_rows(
    summarise_rates(dplyr::group_by(x, pitcher, pitch_type)),
    summarise_rates(dplyr::group_by(dplyr::mutate(x, pitch_type = "ALL"), pitcher, pitch_type))
  )
}

grades_key <- function(level, season) sprintf("grades/%s/%s.parquet", level, season)
components_key <- function(level, season) sprintf("components/%s/%s.parquet", level, season)

# Pitcher's games in date order; odd/even game index for split-half reliability.
game_half <- function(x) {
  g <- x %>% dplyr::distinct(pitcher, game_date, game_pk) %>%
    dplyr::arrange(pitcher, game_date, game_pk) %>%
    dplyr::group_by(pitcher) %>% dplyr::mutate(idx = dplyr::row_number()) %>% dplyr::ungroup()
  idx <- g$idx[match(paste(x$pitcher, x$game_pk), paste(g$pitcher, g$game_pk))]
  ifelse(idx %% 2 == 1, "odd", "even")
}

# Grade table for one facet fit, using the saved anchors.
grade_rows <- function(fit, anchors, facet) {
  a <- anchors[anchors$facet == facet, ]
  ty <- a[a$scope == "type", ]; gl <- a[a$scope == "global", ]; pi_ <- a[a$scope == "pitcher", ]
  bt <- fit$by_type
  it <- match(bt$pitch_type, ty$pitch_type)
  type_rows <- tibble::tibble(
    facet = facet, pitcher = bt$pitcher, pitch_type = bt$pitch_type, n = bt$n,
    xrv_per100 = 100 * bt$est,
    grade_type = to_grade(bt$est, ty$mean[it], ty$unit[it]),
    grade_global = to_grade(bt$est, gl$mean, gl$unit),
    grade_lo80 = to_grade(bt$est + Z80 * bt$se, gl$mean, gl$unit),
    grade_hi80 = to_grade(bt$est - Z80 * bt$se, gl$mean, gl$unit))
  bp <- fit$by_pitcher
  all_rows <- tibble::tibble(
    facet = facet, pitcher = bp$pitcher, pitch_type = "ALL", n = bp$n,
    xrv_per100 = 100 * bp$est,
    grade_type = NA_real_,
    grade_global = to_grade(bp$est, pi_$mean, pi_$unit),
    grade_lo80 = to_grade(bp$est + Z80 * bp$se, pi_$mean, pi_$unit),
    grade_hi80 = to_grade(bp$est - Z80 * bp$se, pi_$mean, pi_$unit))
  dplyr::bind_rows(type_rows, all_rows)
}

GRADE_FACETS <- c("stuff", "location", "pitching")

read_anchors <- function() {
  readr::read_csv(file.path(repo_root(), "models", "grade_anchors.csv"), show_col_types = FALSE,
                  col_types = readr::cols(facet = "c", scope = "c", pitch_type = "c", mean = "d", unit = "d"))
}

# Grades and component rates for one level-season on fixed anchors: full season, plus
# odd/even game halves for MLB (split-half reliability). `fits` optionally supplies
# already-computed full-season fits by facet.
grade_season <- function(x, level, season, anchors, fits = NULL) {
  x$half <- game_half(x)
  halves <- if (level == "MLB") c("full", "odd", "even") else "full"
  grades <- dplyr::bind_rows(lapply(halves, function(h) {
    xs <- if (h == "full") x else x[x$half == h, ]
    dplyr::bind_rows(lapply(GRADE_FACETS, function(f) {
      fit <- if (h == "full" && !is.null(fits[[f]])) fits[[f]] else fit_grades(xs, paste0(f, "_xrv"))
      grade_rows(fit, anchors, f)
    })) %>% dplyr::mutate(half = h)
  })) %>% dplyr::mutate(level = level, season = as.integer(season), .before = 1)
  components <- dplyr::mutate(component_rates(x), level = level, season = as.integer(season), .before = 1)
  list(grades = grades, components = components)
}
