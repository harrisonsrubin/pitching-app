# Per-park tracking offsets (README 6.2). Hawk-Eye installations are not calibrated
# identically, so the same pitch can read differently from park to park. Pitchers who
# throw in several parks reveal each park's offset: we fit, per metric, a pitcher ×
# pitch-type mean plus a venue offset by alternating least squares, so a venue whose
# home pitchers simply throw harder does not get a "fast gun" offset.
# Fit on one level-season at a time.

PARK_METRICS <- c("velo", "spin_rate", "rel_x", "rel_z", "ext", "hb", "ivb")
# Horizontal metrics are stored mirrored (lefties flipped), but a camera's bias is in
# the raw catcher-view frame. They are unmirrored to fit the offset and the offset is
# re-signed when applied, so lefties and righties at a park get the same correction.
HORIZONTAL_METRICS <- c("rel_x", "hb")

hand_sign <- function(d, m) {
  if (m %in% HORIZONTAL_METRICS && "p_throws" %in% names(d)) ifelse(d$p_throws == "L", -1, 1) else 1
}

fit_park_offsets <- function(d, metrics = PARK_METRICS, k = 2000, iters = 5) {
  grp_all <- paste(d$pitcher, d$pitch_type)
  out <- lapply(metrics, function(m) {
    raw <- d[[m]] * hand_sign(d, m)
    ok <- !is.na(raw) & !is.na(d$venue_id)
    x <- raw[ok]; v <- d$venue_id[ok]; g <- grp_all[ok]
    # only pitcher × pitch-type groups seen in at least two venues carry information
    n_venues <- tapply(v, g, function(z) length(unique(z)))
    keep <- n_venues[g] >= 2
    x <- x[keep]; v <- v[keep]; g <- g[keep]
    venues <- sort(unique(v))
    vi <- match(v, venues); gi <- match(g, unique(g))
    n_v <- tabulate(vi, length(venues))
    off <- numeric(length(venues))
    for (i in seq_len(iters)) {
      m_g <- rowsum(x - off[vi], gi)[, 1] / tabulate(gi)
      off <- rowsum(x - m_g[gi], vi)[, 1] / (n_v + k)
      off <- off - sum(off * n_v) / sum(n_v)
    }
    tibble::tibble(venue_id = venues, metric = m, offset = unname(off), n = n_v)
  })
  dplyr::bind_rows(out)
}

apply_park_offsets <- function(d, offsets) {
  for (m in unique(offsets$metric)) {
    o <- offsets[offsets$metric == m, ]
    adj <- o$offset[match(d$venue_id, o$venue_id)]
    d[[m]] <- d[[m]] - dplyr::coalesce(adj, 0) * hand_sign(d, m)
  }
  d
}
