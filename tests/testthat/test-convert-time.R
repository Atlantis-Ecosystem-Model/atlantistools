context("convert_time test")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
bps <- load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")
bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))
test <- load_nc(dir = d, nc = "outputSETAS.nc",
  bps = bps,
  fgs = "functionalGroups.csv",
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "Nums",
  bboxes = bboxes,
  check_acronyms = TRUE,
  testmode = TRUE)

ms <- "1991-01-01"

test <- convert_time(dir = d,
  prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
  data = test,
  modelstart = ms)

test_that("test convert_time", {
  expect_equal(min(test$time), as.Date.numeric(0, origin = ms))
})
