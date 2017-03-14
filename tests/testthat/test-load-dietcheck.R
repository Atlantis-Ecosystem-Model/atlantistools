context("load_dietcheck test datastructure")

diet <- ref_dm

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

diet2 <- load_dietcheck(dietcheck = file.path(d, "outputSETASDietCheck.txt"),
                        fgs = file.path(d, "SETasGroupsDem_NoCep.csv"),
                        prm_run = file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm"), report = FALSE, version_flag = 2)

# This is only used for code-coverage purposes.
diet3 <- suppressWarnings(load_dietcheck(dietcheck = file.path(d, "outputSETASDietCheck.txt"),
                        fgs = file.path(d, "SETasGroupsDem_NoCep.csv"),
                        prm_run = file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm"), report = TRUE, version_flag = 2))

test_that("test output numbers trunk", {
  # expect_true(all(abs(test1$check - 1) < 0.001))
  expect_equal(dim(diet), c(1121, 5))
  expect_is(diet$pred, "character")
  expect_is(diet$prey, "character")
  # expect_equal(diet$atoutput[diet$pred == "Shallow piscivorous fish" & diet$time == 570 & diet$agecl == 3 & diet$prey == "Shallow piscivorous fish"], 0.1192798)
  # expect_equal(diet$atoutput[diet$pred == "Small planktivorous fish" & diet$time == 720 & diet$agecl == 6 & diet$prey == "Refractory detritus"], 0.001334405)

  expect_equal(diet$atoutput[diet$pred == "Shallow piscivorous fish" & diet$time == 146/365 & diet$agecl == 2 & diet$prey == "Diatom"], 5.491577e-001)
  expect_equal(diet$atoutput[diet$pred == "Cephalopod" & diet$time == 584/365 & diet$agecl == 1 & diet$prey == "Cephalopod"], 8.268839e-001)

  expect_equal(diet2$atoutput[diet2$pred == "FVS" & diet2$time == 146/365 & diet2$agecl == 2 & diet2$prey == "PL"], 5.491577e-001)
  expect_equal(diet2$atoutput[diet2$pred == "CEP" & diet2$time == 584/365 & diet2$agecl == 1 & diet2$prey == "CEP"], 8.268839e-001)
})
