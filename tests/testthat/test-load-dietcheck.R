context("load_dietcheck test datastructure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

diet <- load_dietcheck(dir = d,
                       dietcheck = "outputSETASDietCheck.txt",
                       fgs = "SETasGroups.csv",
                       prm_run = "VMPA_setas_run_fishing_F_New.prm",
                       modelstart = "1991-01-01",
                       combine_tresh = 0)


test1 <- diet %>%
  group_by(time, pred, agecl) %>%
  summarise(check = sum(diet))

test_that("test output numbers", {
  expect_true(all(abs(test1$check - 1) < 0.001))
  expect_equal(dim(diet), c(3605, 5))
  expect_is(diet$pred, "factor")
  expect_is(diet$prey, "factor")
})
