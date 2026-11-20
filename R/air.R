# Air density per game (README 6.1). The Magnus force that makes a pitch move scales
# with air density, so movement measured in thin air (Coors, hot days) is rescaled to
# a standard day: sea level, 70°F, 50% relative humidity.

STD_TEMP_C <- 21.111   # 70°F
STD_RH_PCT <- 50

# Density of moist air (kg/m^3) from temperature (°C), relative humidity (%) and
# station pressure (hPa). Saturation vapor pressure from the Tetens formula.
air_density <- function(temp_c, rh_pct, pressure_hpa) {
  es <- 6.1078 * 10^(7.5 * temp_c / (temp_c + 237.3))
  pv <- rh_pct / 100 * es
  tk <- temp_c + 273.15
  100 * (pressure_hpa - pv) / (287.058 * tk) + 100 * pv / (461.495 * tk)
}

# Standard-atmosphere station pressure (hPa) at an elevation in feet.
pressure_from_elevation <- function(elev_ft) {
  1013.25 * (1 - 2.25577e-5 * elev_ft * 0.3048)^5.25588
}

RHO_REF <- air_density(STD_TEMP_C, STD_RH_PCT, 1013.25)

INDOOR_CONDITIONS <- c("Dome", "Roof Closed")

# One row per game: density, whether it was played indoors, and where the inputs came
# from. Indoor games use 70°F / 50% with the outdoor pressure (elevation still
# matters indoors). Missing weather falls back to a standard day at the park's
# elevation, so density is never NA.
game_air <- function(schedule, weather, parks) {
  d <- schedule %>%
    dplyr::select(game_pk, venue_id, weather_condition) %>%
    dplyr::left_join(dplyr::select(weather, game_pk, temp_c, rh_pct, pressure_hpa), by = "game_pk") %>%
    dplyr::left_join(dplyr::select(parks, venue_id, elevation_ft, roof_type), by = "venue_id")
  elev_p <- pressure_from_elevation(dplyr::coalesce(d$elevation_ft, 0))
  indoor <- (d$weather_condition %in% INDOOR_CONDITIONS) | (d$roof_type %in% "Dome")
  have_wx <- !is.na(d$temp_c) & !is.na(d$rh_pct) & !is.na(d$pressure_hpa)
  p_indoor <- dplyr::coalesce(d$pressure_hpa, elev_p)
  rho <- dplyr::case_when(
    indoor  ~ air_density(STD_TEMP_C, STD_RH_PCT, p_indoor),
    have_wx ~ air_density(d$temp_c, d$rh_pct, d$pressure_hpa),
    TRUE    ~ air_density(STD_TEMP_C, STD_RH_PCT, elev_p)
  )
  tibble::tibble(
    game_pk = d$game_pk,
    rho = rho,
    indoor = indoor,
    air_source = ifelse(indoor, "indoor", ifelse(have_wx, "weather", "elevation_only"))
  )
}
