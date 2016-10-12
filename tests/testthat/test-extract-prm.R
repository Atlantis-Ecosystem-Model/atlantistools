context("extract-prm routines")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
prm <- "VMPA_setas_biol_fishing_New.prm"
fgs <- "SETasGroups.csv"

d1 <- prm_to_df(dir = d, prm_biol = prm, fgs = fgs, group = "FVD", parameter = "KWRR")
d2 <- prm_to_df(dir = d, prm_biol = prm, fgs = fgs, group = c("FVD", "FPO"), parameter = "KWRR")
d3 <- prm_to_df(dir = d, prm_biol = prm, fgs = fgs, group = c("FDE", "FBP"), parameter = c("BHbeta", "PP"))
d4 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = "FPO", parameter = "mum")
d5 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = "FPO", parameter = c("mum", "C"))
d6 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = c("FPO", "FBP"), parameter = "mum")
d7 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = c("FPO", "FBP"), parameter = c("mum", "C"))

test_that("test structure of fs", {
  expect_equal(d1$kwrr, 142)
  expect_equal(d2$kwrr, c(142, 110))
  expect_equal(names(d3), c("species", "bhbeta", "pp"))
  expect_equal(d3$bhbeta, c(1.80E+12, 1.80E+10))
  expect_equal(d3$pp, c(1.11E+08, 4.90E+07))

  expect_equal(dim(d4), c(10, 3))
  expect_equal(names(d4), c("species", "agecl", "mum"))

  expect_equal(dim(d7), c(20, 4))
})

d <- system.file("extdata", "gns", package = "atlantistools")
prm <- "NorthSea_biol_fishing.prm"
fgs <- "functionalGroups.csv"

d4 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = "COD", parameter = "mum")
d5 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = "COD", parameter = c("mum", "C"))
d6 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = c("HER", "COD"), parameter = "mum")
d7 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = c("COD", "HER"), parameter = c("mum", "C"))



