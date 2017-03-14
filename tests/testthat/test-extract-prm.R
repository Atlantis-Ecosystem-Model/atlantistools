context("extract-prm routines")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

d1 <- prm_to_df(prm_biol = prm, fgs = fgs, group = "FVS", parameter = "KWRR")
d2 <- prm_to_df(prm_biol = prm, fgs = fgs, group = c("FVS", "FPS"), parameter = "KWRR")
d3 <- prm_to_df(prm_biol = prm, fgs = fgs, group = c("FVS", "FPS"), parameter = c("KWRR", "KWSR"))
d4 <- prm_to_df_ages(prm_biol = prm, fgs = fgs, group = "FPS", parameter = "mum")
d5 <- prm_to_df_ages(prm_biol = prm, fgs = fgs, group = "FPS", parameter = c("mum", "C"))
d6 <- prm_to_df_ages(prm_biol = prm, fgs = fgs, group = c("FVS", "FPS"), parameter = "mum")
d7 <- prm_to_df_ages(prm_biol = prm, fgs = fgs, group = c("FVS", "FPS"), parameter = c("mum", "C"))

test_that("test structure of fs", {
  expect_equal(d1$kwrr, 7000)
  expect_equal(d2$kwrr, c(7000, 0.075))
  expect_equal(names(d3), c("species", "kwrr", "kwsr"))
  expect_equal(d3$kwrr, c(7000, 0.075))
  expect_equal(d3$kwsr, c(1500, 0.028))

  expect_equal(dim(d4), c(10, 3))
  expect_equal(names(d4), c("species", "agecl", "mum"))

  expect_equal(dim(d7), c(20, 4))
})

# d <- system.file("extdata", "gns", package = "atlantistools")
# prm <- "NorthSea_biol_fishing.prm"
# fgs <- "functionalGroups.csv"
#
# d4 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = "COD", parameter = "mum")
# d5 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = "COD", parameter = c("mum", "C"))
# d6 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = c("HER", "COD"), parameter = "mum")
# d7 <- prm_to_df_ages(dir = d, prm_biol = prm, fgs = fgs, group = c("COD", "HER"), parameter = c("mum", "C"))



