# Grades and app aggregates: 20-80 grades, pitcher index, display files, Stuff Map / command /
# tunnel aggregates.

# ==== grades ==============================================================================

# Grades (README 7.6). Pitch-level xRV is aggregated to pitcher × pitch type with
# hierarchical shrinkage: a pitcher effect shared across his whole arsenal plus a
# pitcher-by-pitch-type effect, so what a pitcher shows on his main pitches informs
# his low-sample ones. Season-only fits (no carry-over prior), so stickiness is not
# inflated by construction. 20-80 scale: 50 = reference-season average, 10 points =
# one standard deviation of estimated true talent (MLB 2023–2025).

Z90 <- stats::qnorm(0.95)   # 1.6449: half-width of a 90% interval in SDs

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
    grade_lo90 = to_grade(bt$est + Z90 * bt$se, gl$mean, gl$unit),
    grade_hi90 = to_grade(bt$est - Z90 * bt$se, gl$mean, gl$unit))
  bp <- fit$by_pitcher
  all_rows <- tibble::tibble(
    facet = facet, pitcher = bp$pitcher, pitch_type = "ALL", n = bp$n,
    xrv_per100 = 100 * bp$est,
    grade_type = NA_real_,
    grade_global = to_grade(bp$est, pi_$mean, pi_$unit),
    grade_lo90 = to_grade(bp$est + Z90 * bp$se, pi_$mean, pi_$unit),
    grade_hi90 = to_grade(bp$est - Z90 * bp$se, pi_$mean, pi_$unit))
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

# ==== pitcher_index =======================================================================

# The app's pitcher list (P2 design 4.1): name, hand, team(s) and season line for every
# pitcher with graded pitches. A pitcher's team in a game comes from Statcast (top of
# the inning = the home team is pitching), so a traded pitcher is listed under every
# team he pitched for; his current team is the one from his latest game.

outputs_key <- function(name) file.path("outputs", name)

pitcher_index <- function(pitches, stats, people, level, season) {
  games <- pitches %>%
    dplyr::mutate(team = ifelse(inning_topbot == "Top", home_team, away_team)) %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(
      pitches = dplyr::n(),
      p_throws = dplyr::first(p_throws),
      team_abbrev = team[order(game_date, game_pk, decreasing = TRUE)][1],
      teams = paste(unique(team[order(game_date, game_pk)]), collapse = ","),
      .groups = "drop")
  games %>%
    dplyr::left_join(people, by = c("pitcher" = "player_id")) %>%
    dplyr::left_join(dplyr::select(stats, player_id, g, gs, ip_outs, age), by = c("pitcher" = "player_id")) %>%
    dplyr::mutate(throws = dplyr::coalesce(throws, p_throws), level = level, season = as.integer(season),
                  pitches = as.integer(pitches)) %>%
    dplyr::select(level, season, pitcher, name, throws, team_abbrev, teams, pitches, g, gs, ip_outs, age) %>%
    dplyr::arrange(dplyr::desc(pitches))
}

pitchers_key <- function(level, season) outputs_key(sprintf("pitchers_%s_%s.parquet", level, season))

build_pitcher_index <- function(level, season) {
  p <- arrow::open_dataset(store_path(features_key(level, season))) %>%
    dplyr::select(pitcher, game_pk, game_date, home_team, away_team, inning_topbot, p_throws) %>%
    dplyr::collect()
  stats_key <- sprintf("raw/statsapi/pitching/%s/%s.parquet", level, season)
  stats <- if (store_exists(stats_key)) store_read(stats_key) else
    tibble::tibble(player_id = integer(), g = integer(), gs = integer(), ip_outs = integer(), age = integer())
  idx <- pitcher_index(p, stats, fetch_people(unique(p$pitcher)), level, season)
  store_write(idx, pitchers_key(level, season))
  idx
}

# ==== display =============================================================================

# Pitch-level display data for the app (P2 design 4.2). Private: written to the private
# store only, one file per pitcher bucket so a pitcher's whole season (trades included)
# is one small download. Plus two aggregate references that are public outputs: league
# movement ellipses by hand × type × arm-angle decile, and per-type xRV percentiles.

N_BUCKETS <- 32L

DISPLAY_COLS <- c("pitch_uid", "game_pk", "game_date", "pitcher", "batter", "stand", "p_throws",
                  "pitch_type", "pitch_type_savant", "balls", "strikes", "description", "events",
                  "velo", "spin_rate", "ivb", "hb", "ext", "rel_x", "rel_z", "arm_angle", "vaa",
                  "plate_x", "plate_z", "sz_top", "sz_bot", "stuff_xrv", "location_xrv", "pitching_xrv")

pitcher_bucket <- function(pitcher) as.integer(pitcher %% N_BUCKETS)

display_key <- function(level, season, k) sprintf("display/%s/%s/bucket_%d.parquet", level, season, as.integer(k))
movement_ref_key <- function(level, season) outputs_key(sprintf("movement_ref_%s_%s.parquet", level, season))
pitch_pctl_key <- function(level, season) outputs_key(sprintf("pitch_pctl_%s_%s.parquet", level, season))

# features: one level-season of feature rows (mirrored frame); xrv: pitch_uid + facet
# xRVs; savant: pitch_uid + pitch_type_savant (Savant's label before corrections).
display_rows <- function(features, xrv, savant) {
  features %>%
    dplyr::inner_join(dplyr::select(xrv, pitch_uid, stuff_xrv, location_xrv, pitching_xrv), by = "pitch_uid") %>%
    dplyr::left_join(savant, by = "pitch_uid") %>%
    dplyr::mutate(hb = mirror_x(hb, p_throws), rel_x = mirror_x(rel_x, p_throws)) %>%
    dplyr::select(dplyr::all_of(DISPLAY_COLS))
}

split_buckets <- function(d) {
  k <- pitcher_bucket(d$pitcher)
  lapply(0:(N_BUCKETS - 1L), function(i) d[k == i, ])
}

write_display <- function(level, season, d) {
  b <- split_buckets(d)
  for (i in seq_along(b)) store_write(b[[i]], display_key(level, season, i - 1L))
  invisible(vapply(b, nrow, 1L))
}

movement_ref <- function(d, min_n = 200) {
  d %>%
    dplyr::filter(!is.na(arm_angle), !is.na(hb), !is.na(ivb)) %>%
    dplyr::group_by(p_throws) %>%
    dplyr::mutate(arm_decile = dplyr::ntile(arm_angle, 10)) %>%
    dplyr::group_by(p_throws, arm_decile) %>%
    dplyr::mutate(arm_lo = min(arm_angle), arm_hi = max(arm_angle)) %>%
    dplyr::group_by(p_throws, pitch_type, arm_decile, arm_lo, arm_hi) %>%
    dplyr::summarise(n = dplyr::n(), mean_hb = mean(hb), mean_ivb = mean(ivb),
                     sd_hb = stats::sd(hb), sd_ivb = stats::sd(ivb),
                     rho = if (dplyr::n() > 2) suppressWarnings(stats::cor(hb, ivb)) else NA_real_,
                     .groups = "drop") %>%
    dplyr::filter(n >= min_n) %>%
    dplyr::mutate(rho = dplyr::coalesce(rho, 0))
}

PCTL_FACETS <- c("stuff", "location", "pitching")

pitch_percentiles <- function(d) {
  dplyr::bind_rows(lapply(PCTL_FACETS, function(f) {
    d %>% dplyr::filter(!is.na(.data[[paste0(f, "_xrv")]])) %>%
      dplyr::group_by(pitch_type) %>%
      dplyr::summarise(q = list(stats::quantile(.data[[paste0(f, "_xrv")]], seq(0, 1, 0.01), names = FALSE)),
                       .groups = "drop") %>%
      dplyr::mutate(facet = f, .after = pitch_type)
  }))
}

# Percentile of each xRV among that type's pitches; lower xRV is better, so higher pctl.
pctl_of <- function(xrv, q) {
  p <- stats::approx(q, seq(0, 100, 1), xout = xrv, rule = 2, ties = "ordered")$y
  round(100 - p)
}

# ==== aggregates ==========================================================================

# Public per-pitcher aggregates for the P2b pages (Stuff Map, Command & Tunneling).
# Descriptive numbers only: none of these is a new grade.

# Statcast-style attack zones relative to each batter's own zone: distance from the
# zone's center as a share of its half-width (0.83 ft, with the ball) or half-height.
attack_zone <- function(plate_x, plate_z, sz_top, sz_bot) {
  half_h <- (sz_top - sz_bot) / 2
  ok <- !is.na(plate_x) & !is.na(plate_z) & !is.na(half_h) & half_h > 0
  m <- pmax(abs(plate_x) / 0.83, abs(plate_z - (sz_top + sz_bot) / 2) / half_h)
  out <- ifelse(m < 0.67, "Heart", ifelse(m < 1.33, "Shadow", ifelse(m < 2, "Chase", "Waste")))
  out[!ok] <- NA_character_
  out
}

# Where each pitch is when the hitter must decide: the plane 23.8 ft in front of the
# plate, found by running the pitch's own trajectory back from its plate location.
TUNNEL_FT <- 23.8
tunnel_position <- function(d, y_from_front = TUNNEL_FT) {
  y_front <- 17 / 12
  tp <- plane_time(d$vy0, d$ay, y_front)
  tt <- plane_time(d$vy0, d$ay, y_front + y_from_front)
  dt <- tp - tt; dt2 <- tp^2 - tt^2
  tibble::tibble(x_tun = d$plate_x - (d$vx0 * dt + 0.5 * d$ax * dt2),
                 z_tun = d$plate_z - (d$vz0 * dt + 0.5 * d$az * dt2))
}

# Pitch types thrown fewer than this many times are not published: a row that small is
# effectively one pitch's measurements, and pitch-level data stays private.
MIN_PUBLISHED_N <- 5L

pitch_profiles <- function(display) {
  display %>%
    dplyr::group_by(pitcher, p_throws, pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), velo = mean(velo, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
                     hb = mean(hb, na.rm = TRUE), spin = mean(spin_rate, na.rm = TRUE), ext = mean(ext, na.rm = TRUE),
                     arm_angle = mean(arm_angle, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(n >= MIN_PUBLISHED_N)
}

command_rates <- function(display) {
  z <- display %>%
    dplyr::mutate(az = attack_zone(plate_x, plate_z, sz_top, sz_bot),
                  in_zone = abs(plate_x) <= 0.83 & abs(plate_z - (sz_top + sz_bot) / 2) <= (sz_top - sz_bot) / 2) %>%
    dplyr::filter(!is.na(az))
  rates <- function(g) g %>% dplyr::summarise(
    n = dplyr::n(), zone = mean(in_zone), heart = mean(az == "Heart"), shadow = mean(az == "Shadow"),
    chase = mean(az == "Chase"), waste = mean(az == "Waste"), .groups = "drop")
  dplyr::bind_rows(rates(dplyr::group_by(z, pitcher, pitch_type)),
                   rates(dplyr::group_by(dplyr::mutate(z, pitch_type = "ALL"), pitcher, pitch_type))) %>%
    dplyr::filter(n >= MIN_PUBLISHED_N)
}

# Per batter side: two pitches thrown mostly to different sides would otherwise look far
# apart at the plate because of who they were thrown to, not deception.
tunnel_centroids <- function(display, min_n = 20) {
  display %>%
    dplyr::filter(!is.na(x_tun), !is.na(z_tun), !is.na(plate_x), !is.na(plate_z), !is.na(stand)) %>%
    dplyr::group_by(pitcher, stand, pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), x_tun = mean(x_tun), z_tun = mean(z_tun),
                     x_plate = mean(plate_x), z_plate = mean(plate_z), .groups = "drop") %>%
    dplyr::filter(n >= min_n)
}

profiles_key <- function(level, season) outputs_key(sprintf("profiles_%s_%s.parquet", level, season))
command_key <- function(level, season) outputs_key(sprintf("command_%s_%s.parquet", level, season))
tunnel_key <- function(level, season) outputs_key(sprintf("tunnel_%s_%s.parquet", level, season))
ANCHORS_OUTPUT <- "outputs/grade_anchors.csv"

# Publish the anchors these grades were built on, so the app's rolling grades use the same scale.
write_anchors_output <- function(anchors) {
  path <- store_path(ANCHORS_OUTPUT)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(anchors, path, na = "")
  invisible(path)
}
