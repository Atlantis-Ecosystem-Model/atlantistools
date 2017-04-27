context("convert_time test")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")

ss <- load_dietcheck(dietcheck = file.path(d, "outputSETASDietCheck.txt"), fgs = file.path(d, "SETasGroupsDem_NoCep.csv"),
                     prm_run = prm_run, report = FALSE)

rando <- ss
rando$time <- rando$time * runif(n = nrow(ss), min = 0, max = 1)


test_that("test convert_time", {
  # expect_equal(min(test$time), as.Date.numeric(0, origin = ms))
  expect_error(convert_time(prm_run = prm_run, col = "this is silly..."), "Col is not numeric")
  expect_error(convert_time(prm_run = prm_run, col = rando$time), "Values are corrput")
  expect_equal(convert_time(prm_run = prm_run, col = 365*(0:4)), 0:4)
  expect_equal(convert_time(prm_run = prm_run, col = 0:4), seq(0, by = 0.2, length.out = 5))

})



