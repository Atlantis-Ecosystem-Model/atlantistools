context("calculate consumed biomass")

dir <- system.file("extdata", "gns", package = "atlantistools")
nc_prod <- "outputNorthSeaPROD.nc"
nc_gen <- "outputNorthSea.nc"
dietcheck <- "outputNorthSeaDietCheck.txt"
prm_biol <- "NorthSea_biol_fishing.prm"
prm_run <- "NorthSea_run_fishing_F.prm"
fgs <- "functionalGroups.csv"
bps <- load_bps(dir = dir, init = "init_simple_NorthSea.nc", fgs = fgs)
bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))

df <- calculate_consumed_biomass(dir, nc_prod, nc_gen, dietcheck,
                                 prm_biol, prm_run, bps, fgs, bboxes)
