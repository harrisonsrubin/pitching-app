test_that("mirroring", {
  expect_equal(mirror_x(c(1, 1), c("R", "L")), c(1, -1))
  expect_equal(mirror_axis(c(210, 210), c("R", "L")), c(210, 150))
})

test_that("air-neutral movement scales by density ratio", {
  expect_equal(neutral_movement(1, RHO_REF / 1.2), 1.2)
})

test_that("approach angle sign and zero case", {
  flat <- approach_angles(0, -130, 0, 0, 28, 0)
  expect_equal(flat$vaa, 0); expect_equal(flat$haa, 0)
  down <- approach_angles(0, -130, -5, 0, 28, -15)
  expect_lt(down$vaa, -3); expect_gt(down$vaa, -9)
})

test_that("spin efficiency: no Magnus means zero, bounded, monotone", {
  # acceleration = gravity + drag along the mid-flight velocity only -> no Magnus.
  # Solve the fixed point: drag direction depends on the mid-flight velocity.
  v0 <- c(0, -130, -3); a <- c(0, 25, -32.174)
  for (i in 1:50) {
    vm <- mid_flight_velocity(v0[1], v0[2], v0[3], a[1], a[2], a[3])
    vv <- c(vm$vx, vm$vy, vm$vz); a <- 25 * -vv / sqrt(sum(vv^2)) + c(0, 0, -32.174)
  }
  e0 <- spin_efficiency(v0[1], v0[2], v0[3], a[1], a[2], a[3], 2300, RHO_REF)
  expect_equal(e0, 0, tolerance = 1e-6)
  e1 <- spin_efficiency(0, -130, -3, 5, 28, -15, 2300, RHO_REF)
  e2 <- spin_efficiency(0, -130, -3, 5, 28, -5, 2300, RHO_REF)
  expect_true(e1 >= 0 && e2 <= 1 && e2 > e1)
})

test_that("movement axis", {
  expect_equal(movement_axis(0, 1), 180)
  expect_equal(movement_axis(1, 0), 90)
  expect_equal(circ_diff(350, 10), -20)
})
