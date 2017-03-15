context("sc-init example")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

init <- file.path(d, "INIT_VMPA_Jan2015.nc")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))

data1 <- sc_init(init, prm_biol, fgs, bboxes)
