context("get_conv_mgnbiot test extraction of conversion factor")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
prm_biol <- readLines(con = file.path(d, "VMPA_setas_biol_fishing_New.prm"))

test_that("test conversion factor and helper functions", {
  expect_error(extract_prm(chars = c("XXX  15", "YYY   25"), variable = "X_CN"), "Variable X_CN not found.")
  expect_error(extract_prm(chars = c("X_CN  5.7", "X_CN   6.3"), variable = "X_CN"))

  expect_equal(extract_prm(chars = prm_biol, variable = "X_CN"), 5.7)
  expect_equal(extract_prm(chars = prm_biol, variable = "KMIGa_INVERT_ZG"), 1)

  expect_equal(get_conv_mgnbiot(dir = d, prm_biol = "VMPA_setas_biol_fishing_New.prm"), 1.14e-07)
})


