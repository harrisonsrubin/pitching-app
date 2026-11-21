# P3a projection inputs (design: true-talent projection). Season lines from the Stats
# API in the four components per batter faced (strikeouts, walks + HBP, home runs, hits
# on balls in play), league environments, and each pitcher's pitch-model signals from
# the published grade and component outputs.

PROJ_COMPONENTS <- c("k", "bbhbp", "hr", "hbip")

# Trials for each component: batters faced, except hits on balls in play (per BIP).
component_trials <- function(component) if (identical(component, "hbip")) "bip" else "tbf"

season_lines <- function(stats) {
  l <- stats %>%
    dplyr::group_by(pitcher = as.integer(player_id), season = as.integer(season), level) %>%
    dplyr::summarise(age = suppressWarnings(max(age, na.rm = TRUE)),
                     g = sum(g, na.rm = TRUE), gs = sum(gs, na.rm = TRUE),
                     tbf = sum(tbf, na.rm = TRUE), k = sum(so, na.rm = TRUE),
                     bb = sum(bb, na.rm = TRUE), hbp = sum(hbp, na.rm = TRUE), hr = sum(hr, na.rm = TRUE),
                     h = sum(h, na.rm = TRUE), outs = sum(ip_outs, na.rm = TRUE),
                     r = sum(r, na.rm = TRUE), er = sum(er, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(age = ifelse(is.finite(age), age, NA_real_),
                  bbhbp = bb + hbp,
                  bip = pmax(tbf - k - bb - hbp - hr, 0),
                  hbip = pmax(h - hr, 0),
                  gs_share = ifelse(g > 0, gs / g, 0))
  l %>%
    dplyr::group_by(season, level) %>%
    dplyr::mutate(age_missing = is.na(age), age = ifelse(is.na(age), stats::median(age, na.rm = TRUE), age)) %>%
    dplyr::ungroup() %>%
    dplyr::select(pitcher, season, level, age, age_missing, tbf, k, bbhbp, hr, bip, hbip, outs, r, er, h, bb, hbp, gs_share)
}

league_table <- function(lines) {
  lines %>%
    dplyr::group_by(season, level) %>%
    dplyr::summarise(tbf = sum(tbf), k = sum(k), bbhbp = sum(bbhbp), hr = sum(hr), bip = sum(bip), hbip = sum(hbip),
                     outs = sum(outs), r = sum(r), er = sum(er), .groups = "drop") %>%
    dplyr::mutate(lg_k = k / tbf, lg_bbhbp = bbhbp / tbf, lg_hr = hr / tbf, lg_hbip = hbip / bip,
                  er_r = er / r, ip = outs / 3,
                  fip_const = 9 * er / ip - (13 * hr + 3 * bbhbp - 2 * k) / ip,
                  lg_ra9 = 27 * r / outs) %>%
    dplyr::select(season, level, lg_k, lg_bbhbp, lg_hr, lg_hbip, er_r, fip_const, lg_ra9)
}

pitch_signals <- function(grades, components) {
  g <- grades %>%
    dplyr::filter(pitch_type == "ALL", half == "full", facet %in% c("stuff", "location", "pitching")) %>%
    dplyr::select(level, season, pitcher, facet, grade_global) %>%
    tidyr::pivot_wider(names_from = facet, values_from = grade_global)
  x <- components %>%
    dplyr::filter(pitch_type != "ALL", !is.na(n), n > 0) %>%
    dplyr::group_by(level, season, pitcher) %>%
    dplyr::summarise(x_whiff = stats::weighted.mean(x_whiff_per_swing, n, na.rm = TRUE),
                     x_chase = stats::weighted.mean(x_chase, n, na.rm = TRUE),
                     x_cstrike = stats::weighted.mean(x_cstrike_per_take, n, na.rm = TRUE),
                     x_woba_contact = stats::weighted.mean(x_woba_contact, n, na.rm = TRUE),
                     x_gb = stats::weighted.mean(x_gb, n, na.rm = TRUE), .groups = "drop")
  dplyr::full_join(g, x, by = c("level", "season", "pitcher")) %>%
    dplyr::select(level, season, pitcher, dplyr::any_of(c("stuff", "location", "pitching")),
                  x_whiff, x_chase, x_cstrike, x_woba_contact, x_gb)
}
