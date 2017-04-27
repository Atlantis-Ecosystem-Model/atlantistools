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
#' @param eat Dataframe with information about consumption for age-based groups.
#' Should be generated with \code{\link{load_nc}}.
#' @param grazing Dataframe with information about consumption for non-age-based groups.
#' Should be generated with \code{\link{load_nc}}.
#' @param dm Dataframe with information about diet contributions per predator.
#' Should be generated with \code{\link{load_dietcheck}} using \code{convert_names = TRUE}.
#' @param vol Dataframe with information about volume per polygon and layer.
#' Should be generated with \code{\link{load_nc_physics}}.
#' @param bio_conv Numeric value to transform weight in mg N to tonnes.
#' Should be generated with \code{\link{get_conv_mgnbiot}}.
#' @return Dataframe with columns 'pred', 'agecl', 'polygon', 'time', 'prey'.
#' Consumed biomass in [t] is stored in column 'atoutput'.

#' @export
#' @examples
#' # 1. Using built-in datasets.
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#'
#' bio_conv <- get_conv_mgnbiot(prm_biol = prm_biol)
#'
#' df <- calculate_consumed_biomass(eat = ref_eat, grazing = ref_grazing, dm = ref_dm,
#'                                  vol = ref_vol, bio_conv = bio_conv)
#'
#' # 2. Read in dataframes from existing Atlantis simulation.
#' bboxes <- get_boundary(boxinfo = load_box(file.path(d, "VMPA_setas.bgm")))
#' nc_gen <- file.path(d, "outputSETAS.nc")
#' nc_prod <- file.path(d, "outputSETASPROD.nc")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' init <- file.path(d, "INIT_VMPA_Jan2015.nc")
#'
#' bps <- load_bps(fgs = fgs, init = init)
#'
#' groups_age <- c("Planktiv_S_Fish", "Pisciv_S_Fish")
#' groups_rest <- c("Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det")
#'
#' df_eat <- load_nc(nc = nc_prod, bps = bps, fgs = fgs,
#'                select_groups = groups_age, select_variable = "Eat",
#'                prm_run = prm_run, bboxes = bboxes)
#' df_grz <- load_nc(nc = nc_prod, bps = bps, fgs = fgs,
#'                select_groups = groups_rest, select_variable = "Grazing",
#'                prm_run = prm_run, bboxes = bboxes)
#' df_dm <- load_dietcheck(dietcheck = file.path(d, "outputSETASDietCheck.txt"),
#'                         fgs = fgs, prm_run = prm_run, version_flag = 2, convert_names = TRUE)
#' vol <- load_nc_physics(nc = nc_gen, select_physics = "volume",
#'                        prm_run = prm_run, bboxes = bboxes, aggregate_layers = FALSE)
#'
#' df <- calculate_consumed_biomass(eat = df_eat, grazing = df_grz, dm = df_dm,
#'                                  vol = vol, bio_conv = bio_conv)

calculate_consumed_biomass <- function(eat, grazing, dm, vol, bio_conv) {
  # Combine grazing and eat dataframes!
  data_eat <- dplyr::bind_rows(eat, grazing)

  # Check DietCheck.txt
  check <- agg_data(dm, groups = c("time", "pred", "agecl"), fun = sum)
  if (!all(abs(check$atoutput - 1) < 0.001)) stop("DietCheck.txt does not sum to 1 for all predators.")

  # Check timesteps!
  ts_eat <- sort(unique(data_eat$time))
  ts_dm <- sort(unique(dm$time))
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
    dplyr::full_join(dm, by = c("species" = "pred", "time", "agecl")) %>%
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





