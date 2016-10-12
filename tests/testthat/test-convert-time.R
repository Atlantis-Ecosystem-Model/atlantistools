context("convert_time test")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
prm_run <- "VMPA_setas_run_fishing_F_New.prm"

ss <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", fgs = "SETasGroups.csv",
                     prm_run = "VMPA_setas_run_fishing_F_New.prm", report = FALSE)

rando <- ss
rando$time <- rando$time * runif(n = nrow(ss), min = 0, max = 1)


test_that("test convert_time", {
  # expect_equal(min(test$time), as.Date.numeric(0, origin = ms))
  expect_error(convert_time(dir = d, prm_run = prm_run, col = "this is silly..."), "Col is not numeric")
  expect_error(convert_time(dir = d, prm_run = prm_run, col = rando$time), "Values are corrput")
  expect_equal(convert_time(dir = d, prm_run = prm_run, col = 365*(0:4)), 0:4)
  expect_equal(convert_time(dir = d, prm_run = prm_run, col = 0:4), 0:4)

})



