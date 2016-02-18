context("load-rec test output structure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

test <- load_rec(dir = d,
                 yoy = "outputSETASYOY.txt",
                 ssb = "outputSETASSSB.txt",
                 prm_biol = "VMPA_setas_biol_fishing_New.prm")

test_that("test output numbers", {
  expect_equal(dim(test), c(8, 4))
  expect_equal(names(test), c("species", "time", "ssb", "rec"))
  expect_true(sum(test$ssb)/17810.83 > 0.999)
  expect_true(sum(test$rec)/1340230 > 0.999)
})
