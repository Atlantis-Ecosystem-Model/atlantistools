context("load-dietmatrix test output")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

dm <- load_dietmatrix(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
                      fgs = file.path(d, "SETasGroupsDem_NoCep.csv"))


test_that("test output numbers", {
  expect_equal(length(unique(dm$code)), 13)
  expect_equal(dm$avail[dm$pred == "FVS" & dm$prey_stanza == 1 & dm$pred_stanza == 2 & dm$prey == "PL"], 0.02)
  expect_equal(dm$avail[dm$pred == "CEP" & dm$prey_stanza == 2 & dm$pred_stanza == 2 & dm$prey == "FPS"], 4e-04)
  expect_equal(dm$avail[dm$pred == "BML" & dm$pred_stanza == 2 & dm$prey == "PL"], 0.01)
})

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

dm2 <- load_dietmatrix(prm_biol = file.path(d, "VMPA_setas_biol_fishing_New.prm"),
                       fgs = file.path(d, "SETasGroups.csv"),
                       version_flag = 1)

# Add some test for the baltic model.

