context("conv_nmg_to_biom_t test extraction of conversion factor")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- readLines(con = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"))

test_that("test conversion factor and helper functions", {
  expect_equal(str_split_twice(char = "test    20", min_only = TRUE), 20)
  expect_equal(str_split_twice(char = "test\t\t\t25", min_only = TRUE), 25)
  expect_equal(str_split_twice(char = "test\t\t    15", min_only = TRUE), 15)

  expect_equal(extract_param(chars = prm_biol, variable = "X_CN"), 5.7)
  expect_equal(extract_param(chars = prm_biol, variable = "KMIGa_INVERT_ZG"), 1)

  expect_equal(get_conv_mgnbiot(dir = d, prm_biol = "VMPA_setas_biol_fishing_Trunk.prm"), 1.14e-07)
})


