context("load-rec test output structure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

test <- load_rec(dir = d,
                 yoy = "outputSETASYOY.txt",
                 ssb = "outputSETASSSB.txt",
                 fgs = "SETasGroups.csv",
                 prm_biol = "VMPA_setas_biol_fishing_New.prm",
                 prm_run = "VMPA_setas_run_fishing_F_New.prm",
                 modelstart = "1991-01-01")

test_that("test output numbers", {
  expect_equal(dim(test), c(6, 4))
  expect_equal(names(test), c("species", "time", "ssb", "rec"))
  expect_equal(sum(test$ssb), 809747.3)
  expect_equal(sum(test$rec), 274208183)
})
