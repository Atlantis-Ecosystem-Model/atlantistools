context("plots")

# plot-consumed-biomass.R -------------------------------------------------------------------------
df1 <- expand.grid(pred = c("sp1", "sp2"), agecl = 1:3, polygon = 0:2,
                   time = 0:3, prey = c("sp1", "sp2"), stringsAsFactors = FALSE)
df1$atoutput <- runif(n = nrow(df1), min = 0, max = 1)

# plot_consumed_biomass(df1, select_time = 1, show = 0.95)


# plot-diet-bec-dev.R -----------------------------------------------------------------------------
plots <- plot_diet_bec_dev(preprocess_setas$diet_specmort, wrap_col = "agecl")

# plot-species.R ----------------------------------------------------------------------------------
plot <- plot_species(preprocess_setas, species = "Shallow piscivorous fish")


# context("calculate consumed biomass")

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


# context("preprocess test data structure and format")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

pset <- suppressMessages(preprocess(dir = d, nc_gen = "outputSETAS.nc", nc_prod = "outputSETASPROD.nc",
                                    dietcheck = "outputSETASDietCheck.txt", yoy = "outputSETASYOY.txt", ssb = "outputSETASSSB.txt",
                                    specmort = "outputSETASSpecificMort.txt", specpredmort = "outputSETASSpecificPredMort.txt",
                                    prm_biol = "VMPA_setas_biol_fishing_New.prm", prm_run = "VMPA_setas_run_fishing_F_New.prm",
                                    select_groups = get_groups(dir = d, fgs = "SETasGroups.csv"),
                                    bps = load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc"),
                                    fgs = "SETasGroups.csv", bboxes = get_boundary(load_box(dir = d, bgm = "VMPA_setas.bgm")),
                                    save_to_disc = FALSE, check_acronyms = FALSE, report = FALSE))


# context("sc_init applied to dummy dataframes")

dir <- system.file("extdata", "gns", package = "atlantistools")
fgs <- "functionalGroups.csv"
init <- "init_simple_NorthSea.nc"
prm_biol <- "NorthSea_biol_fishing.prm"
bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
mult_mum <- c(2:3)
mult_c <- c(2:3)
no_avail <- FALSE
save_to_disc <- FALSE
data1 <- sc_init(dir, init, prm_biol, fgs, bboxes, save_to_disc = FALSE)

p <- plot_sc_init(df = data1, mult_mum, mult_c)
p <- plot_sc_init(df = data1, mult_mum, mult_c, pred = "Cod")

data2 <- sc_init(dir, init, prm_biol, fgs, bboxes, pred = "Cod", save_to_disc = FALSE)
p <- plot_sc_init(df = data2, mult_mum, mult_c)


# context("plot_spatial")


dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
nc_gen <- "outputSETAS.nc"
prm_biol <- "VMPA_setas_biol_fishing_Trunk.prm"
prm_run = "VMPA_setas_run_fishing_F_Trunk.prm"
bps = load_bps(dir, fgs = "SETasGroupsDem_NoCep.csv", init = "INIT_VMPA_Jan2015.nc")
fgs = "SETasGroupsDem_NoCep.csv"
bboxes = get_boundary(boxinfo = load_box(dir, bgm = "VMPA_setas.bgm"))

df_agemat <- prm_to_df(dir = dir, prm_biol = prm_biol, fgs = fgs,
                       group = get_age_acronyms(dir = dir, fgs = fgs),
                       parameter = "age_mat")

df_sp <- suppressWarnings(calculate_biomass_spatial(dir, nc_gen, prm_biol, prm_run, bps, fgs, bboxes))
bio_spatial <- combine_ages(df_sp, grp_col = "species", agemat = df_agemat)
bgm_as_df <- convert_bgm(dir, bgm = "VMPA_setas.bgm")

grob <- plot_spatial(bio_spatial, bgm_as_df, select_species = "Cephalopod", timesteps = 3)


dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

nc_prod <- "outputSETASPROD.nc"
nc_gen <- "outputSETAS.nc"
dietcheck <- "outputSETASDietCheck.txt"
prm_biol <- "VMPA_setas_biol_fishing_Trunk.prm"
prm_run <- "VMPA_setas_run_fishing_F_Trunk.prm"
bps <- load_bps(dir = dir, init = "INIT_VMPA_Jan2015.nc", fgs = "SETasGroupsDem_NoCep.csv")
fgs <- "SETasGroupsDem_NoCep.csv"
bboxes <- get_boundary(load_box(dir = dir, bgm = "VMPA_setas.bgm")

bio_consumed <- calculate_consumed_biomass(dir, nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes)

plots <- plot_diet(bio_consumed, wrap_col = "agecl")
