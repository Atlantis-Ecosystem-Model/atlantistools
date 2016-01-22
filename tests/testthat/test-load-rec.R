context("load-rec test output structure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

test <- load_rec(dir = d,
                 yoy = "outputSETASYOY.txt",
                 ssb = "outputSETASSSB.txt",
                 fgs = "functionalGroups.csv",
                 prm_biol = "VMPA_setas_biol_fishing_Trunk.prm",
                 prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
                 modelstart = "1991-01-01")

test_that("test output numbers", {
  expect_equal(dim(test), c(6, 4))
  expect_equal(names(test), c("species", "time", "ssb", "rec"))
  expect_equal(sum(test$ssb), 809747.3)
  expect_equal(sum(test$rec), 274208183)
})
