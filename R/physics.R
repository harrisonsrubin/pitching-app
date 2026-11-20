# Pitch physics (README 6, 7.1, 7.2). Coordinates are Statcast's: x toward the
# catcher's right, y from home plate toward the mound, z up; feet and seconds.
# Accelerations (ax, ay, az) are the constant accelerations of the 9-parameter fit.

GRAVITY_FT <- -32.174
BALL_MASS_KG <- 0.1455
BALL_RADIUS_M <- 0.0366
FT_TO_M <- 0.3048

# Left-handers mirrored onto the right-handed side so one model serves both hands.
mirror_x <- function(x, p_throws) x * ifelse(p_throws == "L", -1, 1)
mirror_axis <- function(axis, p_throws) ifelse(p_throws == "L", (360 - axis) %% 360, axis)

# Movement rescaled to standard air: Magnus force is proportional to air density.
# Release speed is measured at release, so it needs no drag correction.
neutral_movement <- function(pfx, rho) pfx * RHO_REF / rho

# Vertical and horizontal approach angles (degrees) at the front of home plate.
approach_angles <- function(vx0, vy0, vz0, ax, ay, az) {
  y_plate <- 17 / 12
  vy_f <- -sqrt(vy0^2 - 2 * ay * (50 - y_plate))
  t <- (vy_f - vy0) / ay
  vz_f <- vz0 + az * t
  vx_f <- vx0 + ax * t
  tibble::tibble(vaa = -atan(vz_f / vy_f) * 180 / pi,
                 haa = -atan(vx_f / vy_f) * 180 / pi)
}

# Share of the measured spin that is transverse (useful for movement), estimated by
# inverting the lift coefficient: Magnus acceleration is the part of (a - g) that is
# perpendicular to the velocity; C_L = S / (0.4 + 2.32 S) is inverted for the spin
# factor S = r * omega / v.
spin_efficiency <- function(vx0, vy0, vz0, ax, ay, az, spin_rate, rho) {
  # velocity at mid-flight, representative of the whole trip to the plate
  vm <- mid_flight_velocity(vx0, vy0, vz0, ax, ay, az)
  bx <- ax; by <- ay; bz <- az - GRAVITY_FT
  v <- sqrt(vm$vx^2 + vm$vy^2 + vm$vz^2)
  along <- (bx * vm$vx + by * vm$vy + bz * vm$vz) / v
  mx <- bx - along * vm$vx / v
  my <- by - along * vm$vy / v
  mz <- bz - along * vm$vz / v
  a_m <- sqrt(mx^2 + my^2 + mz^2) * FT_TO_M
  v_m <- v * FT_TO_M
  area <- pi * BALL_RADIUS_M^2
  cl <- pmin(pmax(2 * BALL_MASS_KG * a_m / (rho * area * v_m^2), 0), 0.42)
  s <- 0.4 * cl / (1 - 2.32 * cl)
  rpm_t <- s * v_m / BALL_RADIUS_M * 60 / (2 * pi)
  pmin(1, rpm_t / spin_rate)
}

# Velocity halfway (in time) between y = 50 ft and the front of the plate.
mid_flight_velocity <- function(vx0, vy0, vz0, ax, ay, az) {
  vy_f <- -sqrt(vy0^2 - 2 * ay * (50 - 17 / 12))
  tm <- (vy_f - vy0) / ay / 2
  list(vx = vx0 + ax * tm, vy = vy0 + ay * tm, vz = vz0 + az * tm)
}

# Spin axis implied by the movement direction, in Savant's convention
# (180 = pure backspin, movement straight up).
movement_axis <- function(pfx_x, pfx_z) (180 + atan2(-pfx_x, pfx_z) * 180 / pi) %% 360

# Signed angular difference a - b in (-180, 180].
circ_diff <- function(a, b) {
  d <- (a - b) %% 360
  ifelse(d > 180, d - 360, d)
}
