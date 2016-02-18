context("load_dietcheck test datastructure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

diet <- load_dietcheck(dir = d,
                       dietcheck = "outputSETASDietCheck.txt")

test1 <- diet %>%
  group_by(time, pred, habitat) %>%
  summarise(check = sum(atoutput))

test_that("test output numbers", {
  expect_true(all(abs(test1$check - 1) < 0.001))
  expect_equal(dim(diet), c(390, 6))
  expect_is(diet$pred, "character")
  expect_is(diet$prey, "character")
})
