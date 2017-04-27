context("plots")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")
bgm <- file.path(d, "VMPA_setas.bgm")

bps <- load_bps(fgs = fgs, init = init)
bboxes <- get_boundary(boxinfo = load_box(bgm = bgm))

# plot-consumed-biomass.R -------------------------------------------------------------------------
df1 <- expand.grid(pred = c("sp1", "sp2"), agecl = 1:3, polygon = 0:2,
                   time = 0:3, prey = c("sp1", "sp2"), stringsAsFactors = FALSE)
df1$atoutput <- runif(n = nrow(df1), min = 0, max = 1)

# plot_consumed_biomass(df1, select_time = 1, show = 0.95)


# plot-diet-bec-dev.R -----------------------------------------------------------------------------
# plots <- plot_diet_bec_dev(preprocess_setas$diet_specmort, wrap_col = "agecl")

# plot-species.R ----------------------------------------------------------------------------------
plot <- plot_species(preprocess, species = "Shallow piscivorous fish")


# calculate consumed biomass  ---------------------------------------------------------------------

bio_conv <- get_conv_mgnbiot(prm_biol)

test_that("biomass convertion constant", {
  expect_equal(bio_conv, 1.14e-07)
})

df_cons <- calculate_consumed_biomass(ref_eat, ref_grazing, ref_dm, ref_vol, bio_conv)

# sc_init applied to dummy dataframes  ------------------------------------------------------------
# dir <- system.file("extdata", "gns", package = "atlantistools")
# fgs <- "functionalGroups.csv"
# init <- "init_simple_NorthSea.nc"
# prm_biol <- "NorthSea_biol_fishing.prm"
# bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
# mult_mum <- c(2:3)
# mult_c <- c(2:3)
# no_avail <- FALSE
# save_to_disc <- FALSE
# data1 <- sc_init(dir, init, prm_biol, fgs, bboxes, save_to_disc = FALSE)
#
# p <- plot_sc_init(df = data1, mult_mum, mult_c)
# p <- plot_sc_init(df = data1, mult_mum, mult_c, pred = "Cod")
#
# data2 <- sc_init(dir, init, prm_biol, fgs, bboxes, pred = "Cod", save_to_disc = FALSE)
# p <- plot_sc_init(df = data2, mult_mum, mult_c)


# plot_spatial  -----------------------------------------------------------------------------------
df_agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs,
                       group = get_age_acronyms(fgs = fgs),
                       parameter = "age_mat")

df_sp <- calculate_biomass_spatial(ref_nums, ref_structn, ref_resn, ref_n, ref_vol_dz, bio_conv, bps)
bio_spatial <- combine_ages(df_sp, grp_col = "species", agemat = df_agemat)
bgm_as_df <- convert_bgm(bgm = bgm)

grob <- plot_spatial_box(bio_spatial, bgm_as_df, select_species = "Cephalopod", timesteps = 3)

vol <- agg_data(ref_vol, groups = c("time", "polygon"), fun = sum, out = "volume")
grob <- plot_spatial_ts(bio_spatial, bgm_as_df, vol, select_species = "Cephalopod")

# plot_diet  --------------------------------------------------------------------------------------
plots <- plot_diet(df_cons, wrap_col = "agecl")

# plot_calibrate  ---------------------------------------------------------------------------------
p <- plot_line(convert_relative_initial(preprocess$structn_age), col = "agecl")
p <- plot_add_box(p)

# plot_bench --------------------------------------------------------------------------------------
data_comp <- preprocess$biomass
data_comp$atoutput <- data_comp$atoutput * runif(n = nrow(data_comp), min = 0.8, max = 1.2)
data_comp$model <- "test_model"

df_plot <- preprocess$biomass
df_plot$model <- "atlantis"
df_plot <- rbind(df_plot, data_comp)

plot <- plot_line(df_plot, col = "model")

# plot_rec ----------------------------------------------------------------------------------------
# d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
# ex_data <- read.csv(file.path(d, "setas-ssb-rec.csv"), stringsAsFactors = FALSE)
# plot_rec(preprocess_setas$ssb_rec, ex_data)

# check new functionality of yexpand
p <- plot_line(preprocess$structn_age, col = "agecl", yexpand = T)


