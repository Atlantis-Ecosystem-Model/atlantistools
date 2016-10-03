context("preprocess test data structure and format")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

pset <- preprocess(dir = d, nc_gen = "outputSETAS.nc", nc_prod = "outputSETASPROD.nc",
                   dietcheck = "outputSETASDietCheck.txt", yoy = "outputSETASYOY.txt", ssb = "outputSETASSSB.txt",
                   specmort = "outputSETASSpecificMort.txt", specpredmort = "outputSETASSpecificPredMort.txt",
                   prm_biol = "VMPA_setas_biol_fishing_New.prm", prm_run = "VMPA_setas_run_fishing_F_New.prm",
                   select_groups = get_groups(dir = d, fgs = "SETasGroups.csv"),
                   bps = load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc"),
                   fgs = "SETasGroups.csv", bboxes = get_boundary(load_box(dir = d, bgm = "VMPA_setas.bgm")),
                   save_to_disc = FALSE, check_acronyms = FALSE, report = FALSE)

test_that("test output numbers", {
  expect_equal(length(preprocess_setas), 16)
})


