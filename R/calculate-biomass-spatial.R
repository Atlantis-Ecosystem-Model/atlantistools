#' Calculate spatiallly explicit biomass (in [t]) for each group and ageclass per timestep.
#'
#' Calculate spatially explicit biomass time series for each group and ageclass within
#' our model. Data is read in from 'output[...].nc'. Biomass for age based groups is calculated
#' as (StructN [mgN] + ResN [mgN]) * Numbers [individuals]. Biomass for non age based groups is
#' calculated as N [mgN] * volume [m^3] (sediment-dz [m] / volume [m^3] for epibenthic groups).
#' mgN is converted to t based on the stettings in the biol.prm file. Simulation time steps
#' are converted to time in years based on output timesteps given in run.prm.
#'
#' @inheritParams preprocess
#' @return Dataframe with columns 'species', 'agecl', 'polygon', 'layer', 'time'.
#' Biomass in [t] is stored in column 'atoutput'.
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' nc_gen <- "outputSETAS.nc"
#' prm_biol <- "VMPA_setas_biol_fishing_Trunk.prm"
#' prm_run = "VMPA_setas_run_fishing_F_Trunk.prm"
#' bps = load_bps(dir, fgs = "SETasGroupsDem_NoCep.csv", init = "INIT_VMPA_Jan2015.nc")
#' fgs = "SETasGroupsDem_NoCep.csv"
#' bboxes = get_boundary(boxinfo = load_box(dir, bgm = "VMPA_setas.bgm"))
#' df <- calculate_biomass_spatial(dir, nc_gen, prm_biol, prm_run, bps, fgs, bboxes)
#' }

calculate_biomass_spatial <- function(dir = getwd(), nc_gen, prm_biol, prm_run, bps, fgs, bboxes) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  # 1st step: Load in data!
  # - n per box and layer for each invert group
  # - nums, resn, structn for each vert group per box, layer and ageclass
  # - availability matrix
  # - Volume per layer and box, layerthickness
  # - convrsion factor mgN to bio t
  vars <- list("StructN", "ResN", "Nums", "N")
  grps <- list(groups_age, groups_age, groups_age, groups_rest)

  data_bio <- Map(load_nc, select_variable = vars, select_groups = grps,
                  MoreArgs = list(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes))
  names(data_bio) <- tolower(vars)

  vol <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = c("volume", "dz"),
                         prm_run = prm_run, bboxes = bboxes, aggregate_layers = F) %>%
    tidyr::spread_(key_col = c("variable"), value_col = "atoutput")

  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  # 2nd step: Calculate biomass per time, box and layer per group and ageclass!
  # - Age based groups!
  names(data_bio$structn)[names(data_bio$structn) == "atoutput"] <- "sn"
  names(data_bio$resn)[names(data_bio$resn) == "atoutput"] <- "rn"
  biomass_age <- dplyr::inner_join(data_bio$nums, data_bio$structn, by = c("species", "agecl", "polygon", "layer", "time")) %>%
    dplyr::left_join(data_bio$resn,  by = c("species", "agecl", "polygon", "layer", "time")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~(sn + rn) * atoutput * bio_conv), "atoutput")) %>%
    dplyr::select_(.dots = names(.)[!names(.) %in% c("sn", "rn")])

  # - Non age based groups!
  biomass_pools <- dplyr::left_join(data_bio$n, vol, by = c("polygon", "layer", "time"))
  biomass_pools$atoutput <- with(biomass_pools, ifelse(species %in% bps, atoutput * volume / dz * bio_conv, atoutput * volume * bio_conv))
  biomass_pools <- dplyr::select_(biomass_pools, .dots = c("species", "time", "polygon", "layer", "atoutput"))
  biomass_pools$agecl <- NA

  # Combine both dataframes!
  biomass <- dplyr::bind_rows(biomass_age, biomass_pools)

  return(biomass)
}

