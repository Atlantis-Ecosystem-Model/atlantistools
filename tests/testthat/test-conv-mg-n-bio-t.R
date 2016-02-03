context("get_conv_mgnbiot test extraction of conversion factor")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
prm_biol <- readLines(con = file.path(d, "VMPA_setas_biol_fishing_New.prm"))

test_that("test conversion factor and helper functions", {
  expect_equal(str_split_twice(char = "test    20", min_only = TRUE), 20)
  expect_equal(str_split_twice(char = "test\t\t\t25", min_only = TRUE), 25)
  expect_equal(str_split_twice(char = "test\t\t    15", min_only = TRUE), 15)

  expect_error(str_split_twice(char = "dadsadadsadadsad", min_only = FALSE), "Neither space nor tab present.")
  expect_error(str_split_twice(char = "da ds\tdd", min_only = FALSE), "No numeric value present.")

  expect_error(extract_prm(chars = c("XXX  15", "YYY   25"), variable = "X_CN"), "Variable X_CN not found.")
  expect_error(extract_prm(chars = c("X_CN  5.7", "X_CN   6.3"), variable = "X_CN"))

  expect_equal(extract_prm(chars = prm_biol, variable = "X_CN"), 5.7)
  expect_equal(extract_prm(chars = prm_biol, variable = "KMIGa_INVERT_ZG"), 1)

  expect_equal(get_conv_mgnbiot(dir = d, prm_biol = "VMPA_setas_biol_fishing_New.prm"), 1.14e-07)
})


