context("load_dietcheck test datastructure")

library("dplyr")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

diet <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", fgs = "functionalGroups.csv")

test1 <- diet %>%
  group_by()

test_that("test output numbers", {
  expect_equal(dim(data), dim(ref_nums))
  expect_equal(sum(is.na(test$atoutput.x)) + sum(is.na(test$atoutput.y)), 0)
  expect_true(sd(test$check[!is.na(test$check)]) < 0.0000001)
})
