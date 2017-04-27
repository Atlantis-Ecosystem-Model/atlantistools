context("get_boundary check for boundary boxes")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
boxinfo <- load_box(bgm = file.path(d, "VMPA_setas.bgm"))

test_that("test output of get_boundary", {
  expect_equal(get_boundary(boxinfo = boxinfo), c(0, 6, 7, 8, 9, 10))
})


# test for bec_dev models
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
boxinfo <- load_box(bgm = file.path(d, "VMPA_setas.bgm"))

test_that("test output of get_boundary", {
  expect_equal(get_boundary(boxinfo = boxinfo), c(0, 6, 7, 8, 9, 10))
})


