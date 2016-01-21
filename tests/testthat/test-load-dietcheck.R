context("load_dietcheck test datastructure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

diet <- load_dietcheck(dir = d,
                       dietcheck = "outputSETASDietCheck.txt",
                       fgs = "functionalGroups.csv",
                       prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
                       modelstart = "1991-01-01")

test1 <- diet %>%
  group_by(time, pred, agecl) %>%
  summarise(check = sum(diet))

test_that("test output numbers", {
  expect_true(all(abs(test1$check - 1) < 0.001))
  expect_equal(dim(diet), c(6320, 5))
  expect_is(diet$pred, "factor")
  expect_is(diet$prey, "factor")
})
