context("load-dietmatrix test output")

dm <- load_dietmatrix(dir = system.file("extdata", "setas-model-new-becdev", package = "atlantistools"),
                      prm_biol = "VMPA_setas_biol_fishing_New.prm",
                      fgs = "SETasGroups.csv")

test_that("test output numbers", {
  expect_equal(length(unique(dm$code)), 159)
  expect_equal(dm$avail[dm$pred == "FVS" & dm$prey_stanza == 1 & dm$pred_stanza == 2 & dm$prey == "FVV"], 0.02)
  expect_equal(dm$avail[dm$pred == "PWN" & dm$pred_stanza == 2 & dm$prey == "PL"], 0.18)
})
