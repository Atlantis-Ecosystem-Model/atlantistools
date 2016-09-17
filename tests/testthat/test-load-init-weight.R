context("load_init_weight check structure and values in output dataframe")

d <- system.file("extdata", "gns", package = "atlantistools")

iw <- load_init_weight(dir = d, nc = "init_NorthSea.nc", fgs = "functionalGroups.csv")

test_that("test output numbers", {
  expect_equal(dim(iw), c(226, 4))
  expect_equal(iw$rn[iw$species == "cod" & iw$agecl == 7], 61272.89)
  expect_equal(iw$sn[iw$species == "whiting" & iw$agecl == 3], 768.94)
  expect_equal(iw$sn[iw$species == "herring" & iw$agecl == 9], 718.66)
})
