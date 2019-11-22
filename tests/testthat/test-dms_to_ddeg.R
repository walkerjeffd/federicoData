test_that("dms_to_ddeg() works", {
  expect_equal(dms_to_ddeg(800000), 80)
  expect_equal(dms_to_ddeg(803030), 80 + (30 + 30 / 60) / 60)
  expect_equal(dms_to_ddeg(803030.5), 80 + (30 + 30.5 / 60) / 60)
  expect_equal(dms_to_ddeg(c(803030, 800000)), c(80 + (30 + 30 / 60) / 60, 80))
  expect_equal(dms_to_ddeg(c(803030, 800000)), c(80 + (30 + 30 / 60) / 60, 80))
  expect_equal(dms_to_ddeg(NA_real_), NA_real_)
})
