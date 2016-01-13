context("preprocess test data structure and format")

load(system.file("extdata", "setas-model-new-trunk", "preprocess.Rda", package = "atlantistools"))

test_that("test output numbers", {
  expect_equal(length(result), 12)
})


