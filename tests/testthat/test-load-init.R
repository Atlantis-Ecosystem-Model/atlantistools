context("load_init check structure and values in output dataframe")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")

dd <- load_init(init, vars = c("Planktiv_S_Fish1_Nums", "Pisciv_S_Fish1_ResN"))
df <- load_init(init, vars = "Planktiv_S_Fish1_ResN")
dg <- load_init(init, vars = "Megazoobenthos_N")

test_that("test output numbers", {
  expect_equal(dim(dd[[1]]), c(77, 3))
  expect_equal(dim(dd[[2]]), c(77, 3))
  expect_equal(dim(df[[1]]), c(77, 3))
  expect_equal(dim(dg[[1]]), c(11, 2))
})

layers <- get_layers(init)

test_that("test get_layers", {
  expect_equal(length(layers), 11)
  expect_equal(max(layers), 6)
})


