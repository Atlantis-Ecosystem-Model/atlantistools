#' Calculate the consumed biomass in [t] of prey j by predator i.
#'
#' Consumption data is extracted from output[...]PROD.nc. Age based groups
#' are stored as "Eat_" non age based groups as "Grazing_". Units are mg N m^-3 d^-1.
#' Factors are species, time, box and agecl (if present). We will refer to species as
#' pred from here on to indicate the predator perspective.
#' Diet constribution data is extracted from DietCheck.txt. Currently this only works
#' for models based on the trunk code. Units are % diet contribution. Factors are pred, time, agecl, prey.
#' The consumed biomass is calculated as follows:
#' - Calculate consumed biomass as Eat (or Grazing) * boxvolume per time, pred, agecl, box.
#' - Convert to biomass in [t].
#' - Combine with diet contributions and calculate consumed biomass of prey species.
#' @inheritParams preprocess
#' @return Dataframe with columns 'pred', 'agecl', 'polygon', 'time', 'prey'.
#' Consumed biomass in [t] is stored in column 'atoutput'.

#' @export
#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' nc_prod <- "outputNorthSeaPROD.nc"
#' nc_gen <- "outputNorthSea.nc"
#' dietcheck <- "outputNorthSeaDietCheck.txt"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' prm_run <- "NorthSea_run_fishing_F.prm"
#' fgs <- "functionalGroups.csv"
#' bps <- load_bps(dir = dir, init = "init_simple_NorthSea.nc", fgs = fgs)
#' bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
#'
#' df <- calculate_consumed_biomass(dir, nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes)

calculate_consumed_biomass <- function(dir = getwd(), nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes) {
  # Setup group variables
  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  # Extract data
  vars <- list("Eat", "Grazing")
  grps <- list(groups_age, groups_rest)

  data_eat <- Map(load_nc, select_variable = vars, select_groups = grps,
                  MoreArgs = list(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes))
  data_eat <- do.call(rbind, data_eat)

  vol <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = "volume", prm_run = prm_run, bboxes = bboxes, aggregate_layers = F)

  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  data_dm <- load_dietcheck(dir = dir, dietcheck = dietcheck, fgs = fgs, prm_run = prm_run, version_flag = 2, convert_names = TRUE)

  # Check DietCheck.txt
  check <- agg_data(data_dm, groups = c("time", "pred", "agecl"), fun = sum)
  if (!all(abs(check$atoutput -1) < 0.001)) stop("DietCheck.txt does not sum to 1 for all predators.")

  # Check timesteps!
  ts_eat <- sort(unique(data_eat$time))
  ts_dm <- sort(unique(data_dm$time))
  matching <- sum(ts_eat %in% ts_dm) / length(ts_eat)
  message(paste0(100 * round(matching, digits = 2), "% matching timesteps between PROD.nc and DietCheck.txt"))

  # Step1: Calculate consumed biomass as Eat (or Grazing) * boxvolume per time, pred, agecl, box.
  # Weired stuff is happening here... Epibenthic groups consume the bulk of biomass!
  # Let's chceck this again with a different model!
  boxvol <- agg_data(vol, groups = c("polygon", "time"), out = "vol", fun = sum)
  consumed_bio <- dplyr::left_join(data_eat, boxvol, by = c("polygon", "time")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput * vol), "atoutput")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput * bio_conv), "atoutput")) %>%
  # Step2: Combine with diet contribution. We need a full join to make sure no data is lost!
    dplyr::full_join(data_dm, by = c("species" = "pred", "time", "agecl")) %>%
  # Restrict timesteps to netcdf data! Last timestep is weird in Dietcheck.txt.
    dplyr::filter_(~time %in% ts_eat) %>%
    dplyr::rename_(.dots = c("pred" = "species"))

  # Some detective work is needed here!
  det_eat <- consumed_bio[is.na(consumed_bio$atoutput.x), ]
  det_dm <- consumed_bio[is.na(consumed_bio$atoutput.y), ]

  # Remove NAs!
  if (nrow(det_eat) > 0) {
    message(paste0(100 * round(nrow(det_eat)/nrow(consumed_bio), digits = 4),
                   "% data is lost due to missing diet data despite available eat data."))
  }
  if (nrow(det_dm) > 0) {
    message(paste0(100 * round(nrow(det_dm)/nrow(consumed_bio), digits = 4),
                   "% data is lost due to missing eat data despite available diet data."))
  }

  # atoutput.x = eat, atoutput.y = diet
  # Setp4: Calculate consumed biomass of prey species.
  consumed_biomass <- consumed_bio %>%
    dplyr::filter_(~!is.na(atoutput.x)) %>%
    dplyr::filter_(~!is.na(atoutput.y)) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput.x * atoutput.y), "atoutput")) %>%
    dplyr::select_(.dots = names(.)[!names(.) %in% c("atoutput.x", "vol", "atoutput.y")])

  return(dplyr::ungroup(consumed_biomass))
}





