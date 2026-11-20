# Per-type pitch metrics for the Grades table, from the pitcher's display rows. Metrics
# follow the batter-side filter; usage splits vs L / vs R always use every pitch.

pitch_summary <- function(display, side = "All") {
  if (is.null(display) || nrow(display) == 0) return(NULL)
  splits <- display %>% dplyr::group_by(pitch_type) %>%
    dplyr::summarise(usage_l = sum(stand == "L"), usage_r = sum(stand == "R"), .groups = "drop") %>%
    dplyr::mutate(usage_l = usage_l / max(1, sum(display$stand == "L")),
                  usage_r = usage_r / max(1, sum(display$stand == "R")))
  d <- if (side %in% c("L", "R")) display[display$stand == side, ] else display
  if (nrow(d) == 0) return(NULL)
  d %>% dplyr::group_by(pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), velo = mean(velo, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
                     hb = mean(hb, na.rm = TRUE), spin = mean(spin_rate, na.rm = TRUE), ext = mean(ext, na.rm = TRUE),
                     vaa = mean(vaa, na.rm = TRUE), arm_angle = mean(arm_angle, na.rm = TRUE),
                     rel_z = mean(rel_z, na.rm = TRUE), rel_x = mean(rel_x, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(usage = n / sum(n)) %>%
    dplyr::left_join(splits, by = "pitch_type") %>%
    dplyr::arrange(dplyr::desc(usage))
}
