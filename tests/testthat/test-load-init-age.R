context("load_init_age check different function calls")

d <- system.file("extdata", "gns", package = "atlantistools")

init <- "init_simple_NorthSea.nc"
fgs <- "functionalGroups.csv"
bboxes <- get_boundary(load_box(dir = d, bgm = "NorthSea.bgm"))
bps <- load_bps(dir = d, fgs = fgs, init = init)

# Test different calls to the loading functions!
dd <- load_init_age(dir = d, init = init, fgs = fgs, bboxes = bboxes, select_variable = "Nums")
de <- load_init_age(dir = d, init = init, fgs = fgs, bboxes = bboxes, select_variable = "ResN")
df <- load_init_age(dir = d, init = init, fgs = fgs, bboxes = bboxes, select_variable = "StructN")

dg <- load_init_nonage(dir = d, init = init, fgs = fgs, bboxes = bboxes, bps = bps, select_variable = "N")
dh <- load_init_nonage(dir = d, init = init, fgs = fgs, bboxes = bboxes, bps = bps, select_variable = "N",
                       select_groups = c("crangon", "cod"))
di <- load_init_nonage(dir = d, init = init, fgs = fgs, bboxes = bboxes, bps = bps, select_variable = "N",
                       select_groups = c("epifaunal_macrobenthos"))

dj <- load_init_physics(dir = d, init = init, bboxes = bboxes, select_variable = "NO3")
dj <- load_init_physics(dir = d, init = init, bboxes = bboxes, select_variable = "volume")

data <- de %>%
  dplyr::group_by(species, agecl) %>%
  dplyr::summarise(out = unique(atoutput))

test_that("test output numbers", {
  expect_equal(dim(data), c(24, 3))
  expect_equal(data$out[data$species == "Cod" & data$agecl == 7], 61272.89)
  expect_equal(unique(df$atoutput[df$species == "Herring" & df$agecl == 9]), 718.66)
})
