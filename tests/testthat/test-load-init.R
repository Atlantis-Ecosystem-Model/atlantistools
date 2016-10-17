context("load_init check structure and values in output dataframe")

d <- system.file("extdata", "gns", package = "atlantistools")

dd <- load_init(dir = d, init = "init_simple_NorthSea.nc", vars = c("cod1_Nums", "cod1_ResN"))
df <- load_init(dir = system.file("extdata", "setas-model-new-trunk", package = "atlantistools"),
                init = "INIT_VMPA_Jan2015.nc", vars = "Planktiv_S_Fish1_ResN")
dg <- load_init(dir = system.file("extdata", "setas-model-new-trunk", package = "atlantistools"),
                init = "INIT_VMPA_Jan2015.nc", vars = "Megazoobenthos_N")

test_that("test output numbers", {
  expect_equal(dim(dd[[1]]), c(208, 3))
  expect_equal(dim(dd[[2]]), c(208, 3))
  expect_equal(dim(df[[1]]), c(77, 3))
  expect_equal(dim(dg[[1]]), c(11, 2))
})

layers <- get_layers(dir = d, init = "init_simple_NorthSea.nc")

test_that("test get_layers", {
  expect_equal(length(layers), 26)
  expect_equal(max(layers), 6)
})


