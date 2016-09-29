#' Calculate the consumed biomass from predator x to prey Y.
#'
#' Consumption data is extracted from output[...]PROD.nc. Age based groups
#' are stored as "Eat_" non age based groups as "Grazing_". Units are mg N m^-3 d^-1.
#' Factors are species, time, box and agecl (if present). We will refer to species as
#' pred from heron to indicate the predator perspective.
#' Diet constribution data is extracted from DietCheck.txt. Currently this only works
#' with models based on the trunk code. Units are % diet contribution (Will check if
#' it sums to 1 per predator/agecl combination). Factors are pred, time, agecl, prey.
#' The biomass flows are calculated as follows:
#' - Calculate consumed biomass as Eat (or Grazing) * boxvolume per time, pred, agecl, box.
#' - Convert to biomass in [t].
#' - Sum consumed biomass over the model domain per pred, time, agecl.
#' - Combine with diet contributions and calculate consumed biomass of prey species.
#' - Sum up consumed biomass per pred, time, prey.
#'
#' @return df

#' @keywords gen
#' @export

dir <- "c:/backup_z/Atlantis_models/Runs/dummy_02_ATLANTIS_NS/"
nc_prod <- "outputNorthSeaPROD.nc"
nc_gen <- "outputNorthSea.nc"
dietcheck <- "outputNorthSeaDietCheck.txt"
prm_biol <- "NorthSea_biol_fishing.prm"
fgs <- "functionalGroups.csv"
bps <- load_bps(dir = dir, init = "init_NorthSea.nc", fgs = fgs)
bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))

biomass_flow <- function(dir = getwd(), nc_prod, nc_gen, dietcheck, prm_biol, bps, fgs, bboxes) {
  # Setup group variables
  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  # Extract data
  vars <- list("Eat", "Grazing")
  grps <- list(groups_age, groups_rest)

  data_eat <- Map(load_nc, select_variable = vars, select_groups = grps,
                  MoreArgs = list(dir, nc = nc_prod, bps = bps, fgs = fgs, bboxes = bboxes))
  data_eat <- do.call(rbind, data_eat)
  data_eat$species <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = data_eat$species)

  vol <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = "volume", bboxes = bboxes, aggregate_layers = F)

  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  # Step1: Calculate consumed biomass as Eat (or Grazing) * boxvolume per time, pred, agecl, box.
  # Weired stuff is happening here... Epibenthic groups consume the bulk of biomass!
  # Let's chceck this again with a different model!
  boxvol <- agg_data(vol, groups = c("polygon", "time"), out = "vol", fun = sum)
  consumed_bio <- dplyr::left_join(data_eat, boxvol, by = c("polygon", "time")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput * vol), "atoutput")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput * bio_conv), "atoutput"))

}

