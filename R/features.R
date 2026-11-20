# Model features (README 7.2–7.4). Everything here works on "prepared" pitches
# (R/prepare.R): left-handers mirrored, movement in inches and air-neutral, park
# tracking offsets removed. Reference tables are fit once on MLB 2023–2025 and saved
# as small CSVs in models/.

FB_TYPES <- c("4S", "SNK", "CUT")
PITCH_GROUP_CODES <- c(FB = 0L, BR = 1L, OS = 2L)

STUFF_PHYSICS <- c("velo", "perceived_velo", "ext", "hb", "ivb", "rel_z", "rel_x", "arm_angle",
                   "hb_vs_slot", "ivb_vs_slot", "vaa_resid", "spin_rate", "spin_axis_m",
                   "axis_gap", "spin_eff", "pitch_group_code")
FB_DIFFS <- c("d_velo", "d_hb", "d_ivb", "d_axis")

# ---- mirroring and units ---------------------------------------------------------

prepare_mirror <- function(d) {
  d$rel_x <- mirror_x(d$release_pos_x, d$p_throws)
  d$hb <- 12 * mirror_x(neutral_movement(d$pfx_x, d$rho), d$p_throws)
  d$ivb <- 12 * neutral_movement(d$pfx_z, d$rho)
  d$spin_axis_m <- mirror_axis(d$spin_axis, d$p_throws)
  d
}

# Reaction-time-equivalent velocity: release speed scaled by how much closer than
# a 6-ft-extension release the ball is let go (60.5 - 6 = 54.5 ft).
perceived_velo <- function(velo, ext) velo * 54.5 / (60.5 - ext)

# ---- arm angle (README 8.8: derived from release point where missing) ------------

ARM_ANGLE_FORMULA <- arm_angle ~ rel_z + I(rel_z^2) + rel_x + I(rel_x^2) + rel_z:rel_x

fit_arm_angle_ref <- function(d) {
  cf <- stats::coef(stats::lm(ARM_ANGLE_FORMULA, data = d))
  tibble::tibble(term = names(cf), coef = unname(cf))
}

predict_from_terms <- function(d, ref) {
  terms <- setdiff(ref$term, "(Intercept)")
  mm <- stats::model.matrix(stats::as.formula(paste("~", paste(terms, collapse = " + "))),
                            stats::model.frame(stats::as.formula(paste("~", paste(terms, collapse = " + "))),
                                               d, na.action = stats::na.pass))
  as.vector(mm[, ref$term, drop = FALSE] %*% ref$coef)
}

impute_arm_angle <- function(d, ref) {
  pm <- d %>%
    dplyr::group_by(pitcher, season, level) %>%
    dplyr::summarise(arm_med = stats::median(arm_angle, na.rm = TRUE), .groups = "drop")
  med <- pm$arm_med[match(paste(d$pitcher, d$season, d$level), paste(pm$pitcher, pm$season, pm$level))]
  miss <- is.na(d$arm_angle)
  fill <- ifelse(is.na(med), predict_from_terms(d, ref), med)
  d$arm_angle_imputed <- as.integer(miss)
  d$arm_angle <- ifelse(miss, fill, d$arm_angle)
  d
}

# ---- movement vs what the arm slot predicts --------------------------------------

SLOT_MIN_N <- 200

fit_slot_ref <- function(d) {
  d <- d[!is.na(d$arm_angle) & !is.na(d$hb) & !is.na(d$ivb), ]
  d$arm_bin <- floor(d$arm_angle / 5) * 5
  by_bin <- d %>%
    dplyr::group_by(pitch_type, arm_bin) %>%
    dplyr::summarise(hb_med = stats::median(hb), ivb_med = stats::median(ivb), n = dplyr::n(), .groups = "drop")
  by_type <- d %>%
    dplyr::group_by(pitch_type) %>%
    dplyr::summarise(hb_med = stats::median(hb), ivb_med = stats::median(ivb), n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(arm_bin = NA_real_)
  dplyr::bind_rows(by_bin, by_type)
}

apply_slot_ref <- function(d, ref) {
  bins <- ref[!is.na(ref$arm_bin) & ref$n >= SLOT_MIN_N, ]
  types <- ref[is.na(ref$arm_bin), ]
  arm_bin <- floor(d$arm_angle / 5) * 5
  ib <- match(paste(d$pitch_type, arm_bin), paste(bins$pitch_type, bins$arm_bin))
  it <- match(d$pitch_type, types$pitch_type)
  hb_med <- ifelse(is.na(ib), types$hb_med[it], bins$hb_med[ib])
  ivb_med <- ifelse(is.na(ib), types$ivb_med[it], bins$ivb_med[ib])
  d$hb_vs_slot <- d$hb - hb_med
  d$ivb_vs_slot <- d$ivb - ivb_med
  d
}

# ---- approach angle vs what location and release predict --------------------------

fit_vaa_ref <- function(d) {
  d %>%
    dplyr::filter(!is.na(vaa), !is.na(plate_z), !is.na(rel_z), !is.na(ext), !is.na(pitch_group)) %>%
    dplyr::group_by(pitch_group) %>%
    dplyr::group_modify(function(g, key) {
      cf <- stats::coef(stats::lm(vaa ~ plate_z + rel_z + ext, data = g))
      tibble::tibble(intercept = cf[[1]], b_plate_z = cf[[2]], b_rel_z = cf[[3]], b_ext = cf[[4]])
    }) %>%
    dplyr::ungroup()
}

apply_vaa_ref <- function(d, ref) {
  i <- match(d$pitch_group, ref$pitch_group)
  pred <- ref$intercept[i] + ref$b_plate_z[i] * d$plate_z + ref$b_rel_z[i] * d$rel_z + ref$b_ext[i] * d$ext
  d$vaa_resid <- d$vaa - pred
  d
}

# ---- fastball differentials ------------------------------------------------------

circ_mean <- function(deg) {
  r <- deg[!is.na(deg)] * pi / 180
  (atan2(mean(sin(r)), mean(cos(r))) * 180 / pi) %% 360
}

# Most-thrown fastball per pitcher-season-level (ties: 4S > SNK > CUT).
primary_fastball <- function(d) {
  fb <- d[d$pitch_type %in% FB_TYPES, ]
  if (nrow(fb) == 0) {
    return(tibble::tibble(pitcher = d$pitcher[0], season = d$season[0], level = d$level[0],
                          fb_type = character(), fb_velo = numeric(), fb_hb = numeric(),
                          fb_ivb = numeric(), fb_axis = numeric()))
  }
  fb %>%
    dplyr::group_by(pitcher, season, level, pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), fb_velo = mean(velo, na.rm = TRUE), fb_hb = mean(hb, na.rm = TRUE),
                     fb_ivb = mean(ivb, na.rm = TRUE), fb_axis = circ_mean(spin_axis_m), .groups = "drop") %>%
    dplyr::arrange(pitcher, season, level, dplyr::desc(n), match(pitch_type, FB_TYPES)) %>%
    dplyr::distinct(pitcher, season, level, .keep_all = TRUE) %>%
    dplyr::transmute(pitcher, season, level, fb_type = pitch_type, fb_velo, fb_hb, fb_ivb, fb_axis)
}

add_fastball_diffs <- function(d, fb) {
  i <- match(paste(d$pitcher, d$season, d$level), paste(fb$pitcher, fb$season, fb$level))
  d$d_velo <- d$velo - fb$fb_velo[i]
  d$d_hb <- d$hb - fb$fb_hb[i]
  d$d_ivb <- d$ivb - fb$fb_ivb[i]
  d$d_axis <- circ_diff(d$spin_axis_m, fb$fb_axis[i])
  d
}

# ---- all Stuff features ----------------------------------------------------------

# refs = list(arm = <arm_angle_ref>, slot = <slot_ref>, vaa = <vaa_ref>)
add_stuff_features <- function(d, refs) {
  d$perceived_velo <- perceived_velo(d$velo, d$ext)
  d <- impute_arm_angle(d, refs$arm)
  d <- apply_slot_ref(d, refs$slot)
  d <- apply_vaa_ref(d, refs$vaa)
  d <- add_fastball_diffs(d, primary_fastball(d))
  d$pitch_group_code <- unname(PITCH_GROUP_CODES[d$pitch_group])
  d
}

# ---- location (README 7.3) --------------------------------------------------------

ZONE_HALF_WIDTH <- 0.83   # half the plate (8.5 in) plus a ball radius (1.45 in), in feet
ZONE_PAD <- 0.12          # ball radius added above and below the batter's zone

add_location_features <- function(d) {
  d$same_side <- as.integer(d$stand == d$p_throws)
  d$x_in <- d$plate_x * ifelse(d$stand == "R", -1, 1)   # positive = inside to the batter
  d$z_norm <- (d$plate_z - d$sz_bot) / (d$sz_top - d$sz_bot)
  lo <- d$sz_bot - ZONE_PAD; hi <- d$sz_top + ZONE_PAD
  dx <- abs(d$plate_x) - ZONE_HALF_WIDTH
  dz <- pmax(lo - d$plate_z, d$plate_z - hi)
  outside <- pmax(dx, 0)^2 + pmax(dz, 0)^2
  d$edge_dist <- ifelse(dx <= 0 & dz <= 0, pmax(dx, dz), sqrt(outside))
  d
}

# ---- sequencing (README 7.4) ------------------------------------------------------

add_sequence_features <- function(d) {
  key <- paste(d$game_pk, d$at_bat_number, d$pitch_number)
  prev <- match(paste(d$game_pk, d$at_bat_number, d$pitch_number - 1L), key)
  d$prev_d_velo <- d$velo - d$velo[prev]
  d$prev_d_hb <- d$hb - d$hb[prev]
  d$prev_d_ivb <- d$ivb - d$ivb[prev]
  d$prev_d_x <- d$x_in - d$x_in[prev]
  d$prev_d_z <- d$plate_z - d$plate_z[prev]
  d$prev_same_type <- as.integer(d$pitch_type == d$pitch_type[prev])
  d$tto <- d$n_thruorder_pitcher
  d
}

LOCATION_FEATS <- c("x_in", "z_norm", "edge_dist", "balls", "strikes", "same_side", "pitch_group_code")
SEQ_FEATS <- c("prev_d_velo", "prev_d_hb", "prev_d_ivb", "prev_d_x", "prev_d_z", "prev_same_type", "tto")
FEATURE_SETS <- list(
  stuff          = c(STUFF_PHYSICS, FB_DIFFS, "balls", "strikes", "same_side"),
  stuff_physics  = c(STUFF_PHYSICS, "balls", "strikes", "same_side"),
  location       = LOCATION_FEATS,
  pitching       = unique(c(STUFF_PHYSICS, FB_DIFFS, LOCATION_FEATS, SEQ_FEATS)),
  pitching_noseq = unique(c(STUFF_PHYSICS, FB_DIFFS, LOCATION_FEATS))
)

# ---- reference tables in models/ --------------------------------------------------

REF_FILES <- c(arm = "arm_angle_ref.csv", slot = "slot_ref.csv", vaa = "vaa_ref.csv",
               bip = "bip_xrv.csv", count_mix = "count_mix.csv", count_baseline = "count_baseline.csv",
               zone_map = "zone_map.csv")

ref_path <- function(name) file.path(repo_root(), "models", REF_FILES[[name]])

read_refs <- function() {
  lapply(stats::setNames(names(REF_FILES), names(REF_FILES)),
         function(n) readr::read_csv(ref_path(n), show_col_types = FALSE))
}

write_ref <- function(x, name) {
  dir.create(dirname(ref_path(name)), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(x, ref_path(name))
}
