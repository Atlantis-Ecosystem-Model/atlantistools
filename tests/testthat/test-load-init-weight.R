context("load_init_weight check structure and values in output dataframe")

d <- system.file("extdata", "gns", package = "atlantistools")

iw <- load_init_weight(dir = d, nc = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv")

test_that("test output numbers", {iw
  expect_equal(dim(iw), c(24, 4))
  expect_equal(iw$rn[iw$species == "cod" & iw$agecl == 7], 61272.89)
  expect_equal(iw$sn[iw$species == "herring" & iw$agecl == 9], 718.66)
})
