context("sc-init example")

dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
init <- "INIT_VMPA_Jan2015.nc"
prm_biol <- "VMPA_setas_biol_fishing_Trunk.prm"
fgs <- "SETasGroupsDem_NoCep.csv"
bboxes <- get_boundary(load_box(dir = dir, bgm = "VMPA_setas.bgm"))

data1 <- sc_init(dir, init, prm_biol, fgs, bboxes)
