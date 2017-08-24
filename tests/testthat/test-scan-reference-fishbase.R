context("Scan fishbase reference list for specific string")

out <- scan_reference_fishbase("Gadus morhua", chr = "Heteroplasmy")

test_that("Test with cod and heteroplasmy as search tag", {
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 6)
  expect_equal(out$ref_id, 46582)
  expect_equal(out$year, 1992)
})

