context("load_init_weight check structure and values in output dataframe")

d <- system.file("extdata", "gns", package = "atlantistools")

iw <- load_init_weight(dir = d, init = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv")
iw2 <- load_init_age(dir = d, init = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv", select_variable = "ResN", bboxes = get_boundary(load_box(dir = d, bgm = "NorthSea.bgm")))
load_init_nonage(dir = d, init = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv", bboxes = get_boundary(load_box(dir = d, bgm = "NorthSea.bgm")), bps = load_bps(d, fgs = "functionalGroups.csv", init = "init_simple_NorthSea.nc"))

test_that("test output numbers", {
  expect_equal(dim(iw), c(24, 4))
  expect_equal(iw$rn[iw$species == "cod" & iw$agecl == 7], 61272.89)
  expect_equal(iw$sn[iw$species == "herring" & iw$agecl == 9], 718.66)
})

num <- load_init_num(dir = d, init = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv")
n <- load_init_n(dir = d, init = "init_simple_NorthSea.nc", select_groups = "crangon")

layers <- get_layers(dir = d, init = "init_simple_NorthSea.nc")

test_that("test get_layers", {
  expect_equal(length(layers), 26)
  expect_equal(max(layers), 6)
  expect_equal(iw$sn[iw$species == "herring" & iw$agecl == 9], 718.66)
})



