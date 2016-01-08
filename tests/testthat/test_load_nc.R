context("load_nc check structure and values in output dataframe")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
bps <- load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")
bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))

ref_nums <- load_nc(dir = d,
                    nc = "outputSETAS.nc",
                    bps = bps,
                    fgs = "functionalGroups.csv",
                    select_groups = get_groups(dir = d, fgs = "functionalGroups.csv"),
                    select_variable = "Nums",
                    bboxes = bboxes,
                    check_acronyms = TRUE)

ref_n <- load_nc(dir = d,
                    nc = "outputSETAS.nc",
                    bps = bps,
                    fgs = "functionalGroups.csv",
                    select_groups = get_groups(dir = d, fgs = "functionalGroups.csv"),
                    select_variable = "N",
                    bboxes = bboxes,
                    check_acronyms = TRUE)

ref_grazing <- load_nc(dir = d,
                 nc = "outputSETAS.nc",
                 bps = bps,
                 fgs = "functionalGroups.csv",
                 select_groups = get_groups(dir = d, fgs = "functionalGroups.csv"),
                 select_variable = "Grazing",
                 bboxes = bboxes,
                 check_acronyms = TRUE)

ref_eat <- load_nc(dir = d,
                       nc = "outputSETAS.nc",
                       bps = bps,
                       fgs = "functionalGroups.csv",
                       select_groups = get_groups(dir = d, fgs = "functionalGroups.csv"),
                       select_variable = "Eat",
                       bboxes = bboxes,
                       check_acronyms = TRUE)

test_that("test output numbers", {
  expect_equal(dim(text)[1], 826)
  expect_equal(dim(text)[2], 6)
})
