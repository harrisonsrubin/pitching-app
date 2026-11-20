test_that("savant codes map to app labels; unknowns are NA", {
  expect_equal(map_pitch_type(c("FF","SI","ST","KC","FS","FO","SC","EP","PO",NA)),
               c("4S","SNK","SW","CB","SP","FK","CH",NA,NA,NA))
})

test_that("labels map to pitch groups", {
  expect_equal(pitch_group(c("4S","CUT","SW","CS","CH","KN",NA)),
               c("FB","FB","BR","BR","OS","OS",NA))
})
