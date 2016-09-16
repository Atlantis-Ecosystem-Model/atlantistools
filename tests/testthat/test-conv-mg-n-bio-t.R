context("get_conv_mgnbiot test extraction of conversion factor")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
dd <- "VMPA_setas_biol_fishing_New.prm"

test_that("test conversion factor and helper functions", {
  expect_error(extract_prm(dir = d, prm_biol = dd, variable = "XYZABC"), "Variable XYZABC")
  expect_error(extract_prm(dir = d, prm_biol = dd, variable = "FVD"))

  expect_equal(extract_prm(dir = d, prm_biol = dd, variable = "X_CN"), 5.7)
  expect_equal(extract_prm(dir = d, prm_biol = dd, variable = "KMIGa_INVERT_ZG"), 1)

  expect_equal(get_conv_mgnbiot(dir = d, prm_biol = "VMPA_setas_biol_fishing_New.prm"), 1.14e-07)
})


