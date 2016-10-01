context("Calculation of Schoner index")



d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

bps <- load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc")

bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))

non_ss <- load_nc(dir = d, nc = "outputSETAS.nc",
                  bps = bps,
                  fgs = "SETasGroups.csv",
                  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
                  select_variable = "Nums",
                  bboxes = bboxes,
                  check_acronyms = TRUE,
                  report = FALSE)

# test <- load_nc_physics(dir = d, nc = "outputSETAS.nc",
#   select_physics = c("salt", "NO3", "volume"),
#   aggregate_layers = FALSE,
#   bboxes = get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm")))

ss <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", fgs = "SETasGroups.csv", report = FALSE)

rando <- ss
rando$time <- rando$time * runif(n = nrow(ss), min = 0, max = 1)

wrong_input <- non_ss
wrong_input$time <- NULL

ms <- "1991-01-01"

# test <- convert_time(dir = d,
#                      prm_run = "VMPA_setas_run_fishing_F_New.prm",
#                      data = test,
#                      modelstart = ms)

test_that("test convert_time", {
  # expect_equal(min(test$time), as.Date.numeric(0, origin = ms))
  expect_warning(convert_time(dir = d, prm_run = "VMPA_setas_run_fishing_F_New.prm", data = wrong_input, as_date = TRUE, modelstart = ms),
                 "No column 'time' present in dataframe. No time conversion applied!")

  expect_error(convert_time(dir = d, prm_run = "VMPA_setas_run_fishing_F_New.prm", data = rando, as_date = TRUE, modelstart = ms),
               "Provided dataframe has column 'time' but values are corrput. PLease contact package development Team.")

  expect_is(convert_time(dir = d, prm_run = "VMPA_setas_run_fishing_F_New.prm", data = non_ss, as_date = TRUE, modelstart = ms)$time, "Date")

  expect_equal(unique(sort(convert_time(dir = d, prm_run = "VMPA_setas_run_fishing_F_New.prm", data = non_ss, as_date = TRUE, modelstart = ms)$time)),
               as.Date.numeric(0:3 * 365, origin = ms))

  expect_true(all(unique(convert_time(dir = d, prm_run = "VMPA_setas_run_fishing_F_New.prm", data = ss)$time) < 3))

})



