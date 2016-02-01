context("convert_time test")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
bps <- load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc")
bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))
test <- load_nc(dir = d, nc = "outputSETAS.nc",
  bps = bps,
  fgs = "SETasGroups.csv",
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "Nums",
  bboxes = bboxes,
  check_acronyms = TRUE,
  report = FALSE)

ms <- "1991-01-01"

test <- convert_time(dir = d,
  prm_run = "VMPA_setas_run_fishing_F_New.prm",
  data = test,
  modelstart = ms)

test_that("test convert_time", {
  expect_equal(min(test$time), as.Date.numeric(0, origin = ms))
})
