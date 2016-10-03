context("plot-spatial")

df <- data.frame(time = seq(0, 5, length.out = 11))
df2 <- data.frame(time = round(seq(0, 5, length.out = 10), digits = 1))

test_that("test utility functions - select_time", {
  expect_equal(select_time(df)$time, c(0, 5))
  expect_equal(select_time(df, timesteps = 2)$time, c(0, 5))
  expect_equal(select_time(df, timesteps = 3)$time, c(0, 2.5, 5))
  expect_equal(select_time(df, timesteps = 4)$time, c(0, 1.5, 3, 5))
  expect_equal(select_time(df2)$time, c(0, 5))
  expect_equal(select_time(df2, timesteps = 2)$time, c(0, 5))
  expect_equal(select_time(df2, timesteps = 3)$time, c(0, 2.8, 5))
  expect_equal(select_time(df2, timesteps = 4)$time, c(0, 1.7, 3.3, 5))
})


df <- expand.grid(species = c("sp1", "sp2", "sp3"), layer = 0:3, polygon = 0:3, stringsAsFactors = FALSE)

test_that("test utility functions - split_dfs", {
  expect_error(split_dfs(df, cols = c("species", "atoutput")), "Column names in df do not match")
  expect_equal(length(split_dfs(df, cols = "species")), 3)
  expect_equal(unique(sapply(split_dfs(df, cols = c("layer", "polygon")), length)), 3)
})

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

plot_spatial(bio_spatial, bgm_as_df, select_species = "Cephalopod", timesteps = 3)


