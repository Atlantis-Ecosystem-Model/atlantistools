#' Calculate spatiallly explicit biomass (in [t]) for each group and ageclass per timestep.
#'
#' Calculate spatially explicit biomass time series for each group and ageclass within
#' our model. Data is read in from 'output[...].nc'. Biomass for age based groups is calculated
#' as (StructN [mgN] + ResN [mgN]) * Numbers [individuals]. Biomass for non age based groups is
#' calculated as N [mgN] * volume [m^3] (sediment-dz [m] / volume [m^3] for epibenthic groups).
#' mgN is converted to t based on the stettings in the biol.prm file. Simulation time steps
#' are converted to time in years based on output timesteps given in run.prm.
#'
#' @param nums Dataframe with information about numbers for age-based groups.
#' Should be generated with \code{\link{load_nc}}.
#' @param sn Dataframe with information about structural nitrogen for age-based groups.
#' Should be generated with \code{\link{load_nc}}.
#' @param rn Dataframe with information about reserve nitrogen for age-based groups.
#' Should be generated with \code{\link{load_nc}}.
#' @param n Dataframe with information about nitrogen for non-ge-based groups.
#' Should be generated with \code{\link{load_nc}}.
#' @param vol_dz Dataframe with information about volume and layer height per polygon.
#' Should be generated with \code{\link{load_nc_physics}}.
#' @param bio_conv Numeric value to transform weight in mg N to tonnes.
#' Should be generated with \code{\link{get_conv_mgnbiot}}.
#' @param bps Vector of character strings giving the complete list of epibenthic
#' functional groups (Only present in the sediment layer). The names have to match
#' the column 'Name' in the 'functionalGroups.csv' file.
#' Should be generated with \code{\link{load_bps}}.
#' @return Dataframe with columns 'species', 'agecl', 'polygon', 'layer', 'time'.
#' Biomass in [t] is stored in column 'atoutput'.
#' @export
#'
#' @examples
#' # 1. Using built in datasets.
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' init <- file.path(d, "INIT_VMPA_Jan2015.nc")
#' bps <- load_bps(fgs = fgs, init = init)
#'
#' bio_conv <- get_conv_mgnbiot(prm_biol = prm_biol)
#'
#' df <- calculate_biomass_spatial(nums = ref_nums, sn = ref_structn, rn = ref_resn, n = ref_n,
#'                                 vol_dz = ref_vol_dz, bio_conv = bio_conv, bps = bps)
#'
#' # 2. Read in dataframes from existing Atlantis simulation.
#' bboxes <- get_boundary(boxinfo = load_box(file.path(d, "VMPA_setas.bgm")))
#' nc_gen <- file.path(d, "outputSETAS.nc")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
#'
#' groups_age <- c("Planktiv_S_Fish", "Pisciv_S_Fish")
#' groups_rest <- c("Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det")
#'
#' nums <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
#'                 select_groups = groups_age, select_variable = "Nums",
#'                 prm_run = prm_run, bboxes = bboxes)
#' sn <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
#'               select_groups = groups_age, select_variable = "StructN",
#'               prm_run = prm_run, bboxes = bboxes)
#' rn <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
#'               select_groups = groups_age, select_variable = "ResN",
#'               prm_run = prm_run, bboxes = bboxes)
#' n <- load_nc(nc = nc_gen, bps = bps, fgs = fgs,
#'              select_groups = groups_rest, select_variable = "N",
#'              prm_run = prm_run, bboxes = bboxes)
#' vol <- load_nc_physics(nc = nc_gen, select_physics = c("volume", "dz"),
#'                        prm_run = prm_run, bboxes = bboxes, aggregate_layers = FALSE)
#'
#' df <- calculate_biomass_spatial(nums = nums, sn = sn, rn = rn, n = n, vol_dz = vol,
#'                                 bio_conv = bio_conv, bps = bps)
#'
#' # 3. Read in dataframes from existing Atlantis simulation with Map().
#' vars <- list("Nums", "StructN", "ResN", "N")
#' grps <- list(groups_age, groups_age, groups_age, groups_rest)
#' dfs <- Map(load_nc, select_variable = vars, select_groups = grps,
#'            MoreArgs = list(nc = nc_gen, bps = bps, fgs = fgs,
#'                            prm_run = prm_run, bboxes = bboxes))
#'
#' df <- calculate_biomass_spatial(nums = dfs[[1]], sn = dfs[[2]], rn = dfs[[3]], n = dfs[[4]],
#'                                 vol_dz = vol, bio_conv = bio_conv, bps = bps)

calculate_biomass_spatial <- function(nums, sn, rn, n, vol_dz, bio_conv, bps) {
  vol <- tidyr::spread_(vol_dz, key_col = c("variable"), value_col = "atoutput")

  # Calculate biomass per time, box and layer per group and ageclass!
  # - Age based groups!
  names(sn)[names(sn) == "atoutput"] <- "sn"
  names(rn)[names(rn) == "atoutput"] <- "rn"
  biomass_age <- dplyr::inner_join(nums, sn, by = c("species", "agecl", "polygon", "layer", "time")) %>%
    dplyr::left_join(rn,  by = c("species", "agecl", "polygon", "layer", "time")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~(sn + rn) * atoutput * bio_conv), "atoutput")) %>%
    dplyr::select_(.dots = names(.)[!names(.) %in% c("sn", "rn")])

  # - Non age based groups!
  biomass_pools <- dplyr::left_join(n, vol, by = c("polygon", "layer", "time"))
  biomass_pools$atoutput <- with(biomass_pools, ifelse(species %in% bps, atoutput * volume / dz * bio_conv, atoutput * volume * bio_conv))
  biomass_pools <- dplyr::select_(biomass_pools, .dots = c("species", "time", "polygon", "layer", "atoutput"))
  biomass_pools$agecl <- 1

  # Combine both dataframes!
  biomass_spatial <- dplyr::bind_rows(biomass_age, biomass_pools)

  return(dplyr::ungroup(biomass_spatial))
}

