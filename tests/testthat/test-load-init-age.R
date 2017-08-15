context("load_init_age check different function calls")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

init <- file.path(d, "INIT_VMPA_Jan2015.nc")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))
bps <- load_bps(fgs = fgs, init = init)

# Test different calls to the loading functions!
dd <- load_init_age(init = init, fgs = fgs, bboxes = bboxes, select_variable = "Nums")
de <- load_init_age(init = init, fgs = fgs, bboxes = bboxes, select_variable = "ResN")
df <- load_init_age(init = init, fgs = fgs, bboxes = bboxes, select_variable = "StructN")

dg <- load_init_nonage(init = init, fgs = fgs, bboxes = bboxes, bps = bps, select_variable = "N")
dh <- load_init_nonage(init = init, fgs = fgs, bboxes = bboxes, bps = bps, select_variable = "N",
                       select_groups = c("Diatom", "Megazoobenthos"))
di <- load_init_nonage(init = init, fgs = fgs, bboxes = bboxes, bps = bps, select_variable = "N",
                       select_groups = c("Megazoobenthos"))

dj <- load_init_physics(init = init, bboxes = bboxes, select_variable = "NO3")
dk <- load_init_physics(init = init, bboxes = bboxes, select_variable = "volume")

dl <- load_init_stanza(init = init, fgs = fgs, bboxes = bboxes)

dm <- load_init_weight(init = init, fgs = fgs, bboxes = bboxes)

data <- de %>%
  dplyr::filter(!is.na(atoutput)) %>%
  dplyr::group_by(species, agecl) %>%
  dplyr::summarise(out = unique(atoutput))

test_that("test output numbers", {
  expect_equal(dim(data), c(20, 3))
  # expect_equal(data$out[data$species == "Cod" & data$agecl == 7], 61272.89)
  # expect_equal(unique(df$atoutput[df$species == "Herring" & df$agecl == 9]), 718.66)
  expect_equal(dim(dm), c(20, 4))

})

