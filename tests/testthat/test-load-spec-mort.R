context("test load_spec_mort")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

df <- load_spec_mort(specmort, prm_run, fgs)

test_that("test specific values", {
  expect_equal(class(df)[1], "tbl_df")
  expect_equivalent(sapply(df, class), c("numeric", "character", "numeric", "character", "numeric" ))
  expect_true(all(df$time <= 5))
  expect_identical(df$atoutput[df$prey == "FPS" & df$pred == "DL" & df$time == 0.2], 6.801727e-009)
  expect_identical(df$atoutput[df$prey == "FPS" & df$pred == "FPS" & df$time == 1 & df$agecl == 1], 2.310769e-007)
  expect_identical(df$atoutput[df$prey == "CEP" & df$pred == "FPS" & df$time == 1 & df$agecl == 7], 4.309389e-002)
})

