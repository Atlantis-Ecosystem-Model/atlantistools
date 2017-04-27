#' Calculate 3d overlap of predator groups with their prey over time using Schoener Index.
#'
#' @param biomass_spatial Biomass timeseries of each group and ageclass per polygon
#' and layer. This dataframe should be generated with \code{\link{calculate_biomass_spatial}}.
#' @param dietmatrix Availability matrix given in the biological parameter file.
#' This dataframe should be generated with \code{\link{load_dietmatrix}}. Please
#' make sure to use \code{convert_names = TRUE} in \code{\link{load_dietmatrix}}.
#' @param agemat First mature age class for age structured groups. This dataframe should
#' be generated with \code{\link{prm_to_df}} using "age_mat" as parameter.
#' @return List of Schoener's similarity indices ranging from 1 (perfect overlap) to
#' 0 (zero overlap). Dataframe in first listentry gives pred, pred agecl, prey, preyageclass
#' specific index. Dataframe in second listentry gives index per pred, pred agecl. Si is calculated
#' as weighted mean with the availabilities as weights.
#'
#' @export
#'
#' @examples
#' # Using built in datasets.
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#'
#' dietmatrix <- load_dietmatrix(prm_biol, fgs, convert_names = TRUE)
#' agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs,
#'                     group = get_age_acronyms(fgs = fgs),
#'                     parameter = "age_mat")
#'
#' sp_overlap <- calculate_spatial_overlap(biomass_spatial = ref_bio_sp,
#'                                         dietmatrix = dietmatrix,
#'                                         agemat = agemat)

calculate_spatial_overlap <- function(biomass_spatial, dietmatrix, agemat) {
  # Check input dataframes!
  check_df_names(biomass_spatial, expect = c("species", "agecl", "polygon", "layer", "time", "atoutput"))
  check_df_names(dietmatrix, expect = c("pred", "pred_stanza", "prey_stanza","code", "prey", "avail", "prey_id"))
  check_df_names(agemat, expect = c("species", "age_mat"))

  # Step1: Calculate relative biomass per box and layer per group and agecl!
  # - Age based groups!
  # bio_stanza <- combine_ages(biomass_spatial, grp_col = "species", agemat = agemat)
  perc_biomass <- agg_perc(biomass_spatial, groups = c("time", "species", "agecl"), out = "perc_bio")
  # Add stanza information
  bio_stanza <- dplyr::left_join(perc_biomass, agemat, by = c("species"))
  bio_stanza$species_stanza <- ifelse(bio_stanza$agecl < bio_stanza$age_mat, 1, 2)
  bio_stanza$species_stanza[is.na(bio_stanza$species_stanza)] <- 2 # Not sure if this is correct!

  biomass <- bio_stanza

  # Fill data gaps to make sure that both combinations are present later:
  # - pred/stanza present in box/layer combination & prey/stanza absent
  # - prey/stanza present in box/layer combination $ pred/stanza absent
  ac_box_layer <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("polygon", "layer")))
  ac_pred_agecl <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("species", "agecl", "species_stanza")))
  ac_time <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("time")))

  data_bio <- merge(ac_box_layer, ac_pred_agecl) %>%
    merge(ac_time) %>%
    dplyr::left_join(biomass, by = c("time", "species", "agecl", "polygon", "layer", "species_stanza"))
  data_bio$perc_bio[is.na(data_bio$perc_bio)] <- 0

  # Remove sediment layer! Need to read in the sediment penetration depth (KDEP) to include sedimant layer.
  data_bio <- dplyr::filter_(data_bio, lazyeval::interp(~col != max(col), col = as.name("layer")))

  # Apply Schoener calculations to all predators!
  ps <- data_bio %>%
    dplyr::select_(.dots = c("species", "agecl")) %>%
    unique() %>%
    dplyr::filter_(~species %in% unique(dietmatrix$pred))

  sis <- Map(schoener, predgrp = ps$species, ageclass = ps$agecl,
             MoreArgs = list(biomass = data_bio, avail = dietmatrix))

  return(sis)
}

# 3rd step: Calculate schoener index per pred / prey combination (including stanzas)
# - pred: %biomass per predator ageclass per time, box, layer
# - avail: availability matrix
# - prey: overall preybiomass
# id <- 1
# predgrp <- ps$species[id]
# ageclass <- ps$agecl[id]
# biomass <- data_bio
# avail <- dietmatrix
schoener <- function(predgrp, ageclass, biomass, avail) {
  if (!(length(predgrp) == 1 & length(ageclass) == 1)) {
    stop("Only one predator/agecl combination allowed in dataframe 'pred'.")
  }

  # Select specific predator/ageclass combination! (columns still called species at this point)
  df_pred <- dplyr::filter_(biomass, ~species == predgrp & agecl == ageclass)
  pstanza <- unique(df_pred$species_stanza)
  df_avail <- dplyr::filter_(avail, ~pred == predgrp & pred_stanza == pstanza & avail != 0)
  # Remove predator/predstanza since overlap is 1 by default!
  biomass_clean <- dplyr::filter_(biomass, ~!(species == predgrp & agecl == ageclass))

  # Combine predator data with prey data!
  # WARNING: This may lead to a very huge dataframe... all (even non existing)
  # pred/prey combinations are combined! Therefore the calculations are applied per predator.
  # The 2nd join has to be an inner_join to make sure only available prey groups are
  # used to calculate the overlap index!
  si <- dplyr::left_join(df_avail, df_pred, by = c("pred" = "species", "pred_stanza" = "species_stanza")) %>%
    dplyr::inner_join(biomass_clean, by = c("prey" = "species", "prey_stanza" = "species_stanza", "time", "polygon", "layer"))

  si$si <- with(si, abs(perc_bio.x - perc_bio.y))

  # Schoner index per pred/agecl/prey/preystanza combination!
  si_spec <- si %>%
    agg_data(col = "si", groups = c("time", "pred", "agecl.x", "prey", "agecl.y", "pred_stanza", "prey_stanza", "avail"), out = "si", fun = sum) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~1 - si * 0.5), "si")) %>%
    dplyr::rename_(.dots = c("agecl_pred" = "agecl.x", "agecl_prey" = "agecl.y"))

  # 4th step: Aggregate schoner index based on availabilities
  # Combine to pred/predstanza index! Weight with present availabilities!
  si_overall <- si_spec %>%
    agg_perc(col = "avail", groups = c("time"), out = "avail") %>%
    dplyr::mutate_(.dots = stats::setNames(list(~si * avail), "si")) %>%
    agg_data(col = "si", groups = c("time", "pred", "agecl_pred"), out = "si", fun = sum)

  return(list(si_spec, si_overall))
}


# let's do some sicily debugging
# d <- file.path("C:", "Users", "alexanderke", "Dropbox", "Atlantis_SoS_Files_Alex")
#
# list.files(d)
#
# bgm <- "geometry.bgm"
# fgs <- "newFGHorMigr.csv"
# init <- "inSic03052016.nc"
# nc_gen <- "out_TS_TARGET4.nc"
# prm_biol <- "Sic_biol_fishing_TARGET4.prm"
# prm_run <- "Sic_run_fishing_F_gape100_65yr.prm"
#
# boundary_boxes <- get_boundary(boxinfo = load_box(dir = d, bgm = bgm))
# epibenthic_groups <- load_bps(dir = d, fgs = fgs, init = init)
#
# groups <- get_groups(dir = d, fgs = fgs)
# groups_age <- get_age_groups(dir = d, fgs = fgs)
# groups_rest <- groups[!groups %in% groups_age]
#
# bio_conv <- get_conv_mgnbiot(dir = d, prm_biol = prm_biol)
#
# # Read in data ------------------------------------------------------------------------------------
# vars <- list("Nums",     "StructN",  "ResN",     "N")
# ncs  <- list(nc_gen,     nc_gen,     nc_gen,     nc_gen)
# grps <- list(groups_age, groups_age, groups_age, groups_rest)
# dfs <- Map(load_nc, nc = ncs, select_variable = vars, select_groups = grps,
#            MoreArgs = list(dir = d, bps = epibenthic_groups, fgs = fgs, prm_run = prm_run, bboxes = boundary_boxes))
#
# df_vol_dz <- load_nc_physics(dir = d, nc = nc_gen, select_physics = c("volume", "dz"),
#                              prm_run = prm_run, bboxes = boundary_boxes, aggregate_layers = F)
#
#
# bio_sp <- calculate_biomass_spatial(nums = dfs[[1]], sn = dfs[[2]], rn = dfs[[3]], n = dfs[[4]],
#                                     vol_dz = df_vol_dz, bio_conv = bio_conv, bps = epibenthic_groups)
#
# biomass_spatial <- bio_sp
#
# dietmatrix <- load_dietmatrix(dir = d, prm_biol, fgs, convert_names = TRUE, version_flag = 1)
#
# agemat <- prm_to_df(dir = d, prm_biol = prm_biol, fgs = fgs,
#                     group = get_age_acronyms(dir = d, fgs = fgs),
#                     parameter = "age_mat")
#
# sp_overlap <- calculate_spatial_overlap(biomass_spatial = bio_sp,
#                                         dietmatrix = dietmatrix,
#                                         agemat = agemat)










