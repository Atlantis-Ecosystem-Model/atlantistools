context("preprocess test data structure and format")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

test_that("test output numbers", {
  expect_equal(length(preprocess_setas), 14)
})


