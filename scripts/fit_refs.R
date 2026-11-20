# Usage: Rscript scripts/fit_refs.R
# Fits the reference tables on prepared MLB reference seasons (cfg()$seasons_ref) and
# writes them to models/: arm-angle fill, movement-vs-slot medians, expected approach
# angle, BIP run value by xwOBA, the league count/platoon mix, and the league mean
# target by count and platoon.
source("R/load.R")
cols <- c("pitcher", "season", "level", "pitch_type", "pitch_group", "arm_angle", "rel_z", "rel_x",
          "hb", "ivb", "vaa", "plate_z", "ext", "balls", "strikes", "stand", "p_throws", "in_play",
          "delta_run_exp", "estimated_woba_using_speedangle")
d <- dplyr::bind_rows(lapply(cfg()$seasons_ref, function(s) {
  arrow::open_dataset(store_path(prepared_key("MLB", s))) %>% dplyr::select(dplyr::all_of(cols)) %>% dplyr::collect()
}))
d <- d[d$balls <= 3 & d$strikes <= 2, ]
write_ref(fit_arm_angle_ref(d[!is.na(d$arm_angle), ]), "arm")
write_ref(fit_slot_ref(d), "slot")
write_ref(fit_vaa_ref(d), "vaa")
bip <- fit_bip_xrv(d)
write_ref(bip, "bip")
d$target <- denoised_target(d, bip)
d$same_side <- as.integer(d$stand == d$p_throws)
mix <- d %>% dplyr::count(balls, strikes, same_side) %>% dplyr::mutate(share = n / sum(n)) %>% dplyr::select(-n)
write_ref(mix, "count_mix")
base <- d %>% dplyr::filter(!is.na(target)) %>%
  dplyr::group_by(balls, strikes, same_side) %>% dplyr::summarise(mean_target = mean(target), .groups = "drop")
write_ref(base, "count_baseline")
zone_cols <- c("batter", "sz_top", "sz_bot")
zpos <- arrow::open_dataset(store_path(prepared_key("MLB", 2025L))) %>% dplyr::select(dplyr::all_of(zone_cols)) %>% dplyr::collect()
zabs <- arrow::open_dataset(store_path(prepared_key("MLB", ABS_ERA$MLB[1]))) %>% dplyr::select(dplyr::all_of(zone_cols)) %>% dplyr::collect()
zm <- fit_zone_map(zpos, zabs)
write_ref(zm, "zone_map")
print(as.data.frame(zm))
print(as.data.frame(bip))
message("Mean target ", round(mean(d$target, na.rm = TRUE), 5), " vs mean delta_run_exp ",
        round(mean(d$delta_run_exp, na.rm = TRUE), 5))
