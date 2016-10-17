# Set variable names ------------------------------------------------------------------------------
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

bgm <- "VMPA_setas.bgm"
fgs <- "SETasGroupsDem_NoCep.csv"
init <- "INIT_VMPA_Jan2015.nc"
nc_gen <- "outputSETAS.nc"
nc_prod <- "outputSETASPROD.nc"
dietcheck <- "outputSETASDietCheck.txt"
yoy <- "outputSETASYOY.txt"
ssb <- "outputSETASSSB.txt"
prm_biol <- "VMPA_setas_biol_fishing_Trunk.prm"
prm_run <- "VMPA_setas_run_fishing_F_Trunk.prm"

boundary_boxes <- get_boundary(boxinfo = load_box(dir = d, bgm = bgm))
epibenthic_groups <- load_bps(dir = d, fgs = fgs, init = init)
groups <- c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det")
groups_age <- groups[1:2]
groups_rest <- groups[3:length(groups)]

# Create reference dataframes ---------------------------------------------------------------------
vars <- list("Nums",     "StructN",  "ResN",     "Growth",   "Eat",      "Grazing",   "N")
ncs  <- list(nc_gen,     nc_gen,     nc_gen,     nc_prod,    nc_prod,    nc_prod,     nc_gen)
grps <- list(groups_age, groups_age, groups_age, groups_age, groups_age, groups_rest, groups_rest)
dfs <- Map(load_nc, nc = ncs, select_variable = vars, select_groups = grps,
           MoreArgs = list(dir = d, bps = epibenthic_groups, fgs = fgs, prm_run = prm_run, bboxes = boundary_boxes))

ref_nums    <- dfs[[1]]
ref_structn <- dfs[[2]]
ref_resn    <- dfs[[3]]
ref_growth  <- dfs[[4]]
ref_eat     <- dfs[[5]]
ref_grazing <- dfs[[6]]
ref_n       <- dfs[[7]]

ref_vol_dz <- load_nc_physics(dir = d, nc = "outputSETAS.nc", select_physics = c("volume", "dz"),
                              prm_run = "VMPA_setas_run_fishing_F_Trunk.prm", bboxes = boundary_boxes, aggregate_layers = F)

ref_vol <- load_nc_physics(dir = d, nc = "outputSETAS.nc", select_physics = "volume",
                           prm_run = "VMPA_setas_run_fishing_F_Trunk.prm", bboxes = boundary_boxes, aggregate_layers = F)

ref_physics <- load_nc_physics(dir = d, nc = "outputSETAS.nc",
                               select_physics = c("salt", "NO3", "NH3", "Temp", "Chl_a", "Denitrifiction"),
                               prm_run = "VMPA_setas_run_fishing_F_Trunk.prm", bboxes = boundary_boxes, aggregate_layers = F)

ref_dm <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt",
                         fgs = "SETasGroupsDem_NoCep.csv",
                         prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
                         version_flag = 2, convert_names = TRUE)

# Save to HDD and cleanup -------------------------------------------------------------------------
devtools::use_data(ref_eat, ref_grazing, ref_n, ref_nums, ref_structn, ref_resn,
                   ref_vol_dz, ref_vol, ref_dm, ref_growth, ref_physics, overwrite = TRUE)

rm(list = ls())


