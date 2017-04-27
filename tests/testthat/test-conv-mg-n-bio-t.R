context("get_conv_mgnbiot test extraction of conversion factor")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
dd <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")

test_that("test conversion factor and helper functions", {
  expect_error(extract_prm(dd, variable = "XYZABC"), "Variable XYZABC")
  expect_error(extract_prm(dd, variable = "FVD"), "found multiple times.")

  expect_equal(extract_prm(dd, variable = "X_CN"), 5.7)
  expect_equal(extract_prm(dd, variable = "mum_ZG_T15"), 0.02)

  expect_equal(get_conv_mgnbiot(dd), 1.14e-07)
})


