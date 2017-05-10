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

dm <- load_dietmatrix(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
                      fgs = file.path(d, "SETasGroupsDem_NoCep.csv"), transform = FALSE)

dm_new <- write_diet(dietmatrix = dm, prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"), save_to_disc = FALSE)

test_that("test output numbers", {
  expect_equal(length(dm_new), 1630)
  # start of new diet matrix
  expect_equal(dm_new[118], "pPREY1FPS1\t11")
  # end of new diet matrix
  expect_equal(dm_new[143], paste(dm[nrow(dm), c(5:ncol(dm))], collapse = "\t"))
  # Rest of the file did not change
  expect_true(grepl(pattern = "# Clearance rates for consumers", x = dm_new[609]))
})


d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

dm2 <- load_dietmatrix(prm_biol = file.path(d, "VMPA_setas_biol_fishing_New.prm"),
                       fgs = file.path(d, "SETasGroups.csv"),
                       version_flag = 1)

