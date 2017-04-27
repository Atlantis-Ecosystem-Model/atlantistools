# Set variable names ------------------------------------------------------------------------------
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

bgm       <- file.path(d, "VMPA_setas.bgm")
fgs       <- file.path(d, "SETasGroupsDem_NoCep.csv")
init      <- file.path(d, "INIT_VMPA_Jan2015.nc")
nc_gen    <- file.path(d, "outputSETAS.nc")
nc_prod   <- file.path(d, "outputSETASPROD.nc")
dietcheck <- file.path(d, "outputSETASDietCheck.txt")
yoy       <- file.path(d, "outputSETASYOY.txt")
ssb       <- file.path(d, "outputSETASSSB.txt")
prm_biol  <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
prm_run   <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")

boundary_boxes <- get_boundary(boxinfo = load_box(bgm = bgm))
epibenthic_groups <- load_bps(fgs = fgs, init = init)
groups <- c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det")
groups_age <- groups[1:2]
groups_rest <- groups[3:length(groups)]
bio_conv <- get_conv_mgnbiot(prm_biol = prm_biol)

# Create reference dataframes ---------------------------------------------------------------------
vars <- list("Nums",     "StructN",  "ResN",     "Growth",   "Eat",      "Grazing",   "N")
ncs  <- list(nc_gen,     nc_gen,     nc_gen,     nc_prod,    nc_prod,    nc_prod,     nc_gen)
grps <- list(groups_age, groups_age, groups_age, groups_age, groups_age, groups_rest, groups_rest)
dfs <- Map(load_nc, nc = ncs, select_variable = vars, select_groups = grps,
           MoreArgs = list(bps = epibenthic_groups, fgs = fgs, prm_run = prm_run, bboxes = boundary_boxes))

ref_nums    <- dfs[[1]]
ref_structn <- dfs[[2]]
ref_resn    <- dfs[[3]]
ref_growth  <- dfs[[4]]
ref_eat     <- dfs[[5]]
ref_grazing <- dfs[[6]]
ref_n       <- dfs[[7]]

ref_vol_dz <- load_nc_physics(nc = nc_gen, select_physics = c("volume", "dz"),
                              prm_run = prm_run, bboxes = boundary_boxes, aggregate_layers = F)

ref_vol <- load_nc_physics(nc = nc_gen, select_physics = "volume",
                           prm_run = prm_run, bboxes = boundary_boxes, aggregate_layers = F)

ref_physics <- load_nc_physics(nc = nc_gen,
                               select_physics = c("salt", "NO3", "NH3", "Temp", "Chl_a", "Denitrifiction"),
                               prm_run = prm_run, bboxes = boundary_boxes, aggregate_layers = F)

ref_dm <- load_dietcheck(dietcheck = dietcheck,
                         fgs = fgs,
                         prm_run = prm_run,
                         version_flag = 2, convert_names = TRUE)

ref_bio_sp <- calculate_biomass_spatial(nums = ref_nums, sn = ref_structn, rn = ref_resn, n = ref_n,
                                        vol_dz = ref_vol_dz, bio_conv = bio_conv, bps = epibenthic_groups)

ref_bio_cons <- calculate_consumed_biomass(eat = ref_eat, grazing = ref_grazing, dm = ref_dm,
                                           vol = ref_vol, bio_conv = bio_conv)

ref_dietmatrix <- load_dietmatrix(prm_biol, fgs, convert_names = TRUE)

ref_agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs,
                        group = get_age_acronyms(fgs = fgs), parameter = "age_mat")

# Save to HDD and cleanup -------------------------------------------------------------------------
devtools::use_data(ref_eat, ref_grazing, ref_n, ref_nums, ref_structn, ref_resn,
                   ref_vol_dz, ref_vol, ref_dm, ref_growth, ref_physics, ref_bio_sp,
                   ref_bio_cons, ref_dietmatrix, ref_agemat, overwrite = TRUE)

rm(list = ls())


