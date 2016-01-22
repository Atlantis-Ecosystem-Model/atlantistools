#' Preprocess Atlantis output (netcdf)
#'
#' This function loads data from Atlantis output (netcdf) files, performs various
#' calculations and saves the intermediate output as .Rda file. Information
#' about individual weight is extracted from the general netcdf output file via
#' "N", "ResN", "StructN" in additon information about numbers is extracted via
#' "Nums". Using the productivity outputfile information about feeding is extracted
#' ("Eat", "Grazing"). The raw-data is aggregated in various levels of
#' complexity. E.g. spatially, age-based, vertically.
#' Please see the details section for further information about the specific
#' calculations.
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param nc_gen Character string giving the filename of the general netcdf
#' file Usually "output[...].nc". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param nc_prod Character string giving the filename of the productivity netcdf
#' file. Usually "output[...]PROD.nc". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param bps Vector of character strings giving the complete list of epibenthic
#' functional groups (Only present in the sediment layer). The names have to match
#' the column 'Name' in the 'functionalGroups.csv' file.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param select_groups Character vector of funtional groups which shall be read in.
#' Names have to match the ones used in the ncdf file. Check column "Name" in
#' "functionalGroups.csv" for clarification.
#' @param bboxes Integer vector giving the box-id of the boundary boxes.
#' @param check_acronyms Logical testing if functional-groups in
#' select_groups are inactive in the current model run. The will be omitted
#' in the output.
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}.
#' @param out Character string giving the filename of the *.Rda output
#' file. In case you are using different folders for your model data
#' and output files please pass a complete folder/filename string and
#' set dir to 'NULL'.
#' @param report Logical indicating if additional information shall be printed
#' during function evaluation.
#' @param save_to_disc Logical indicating if the resulting list shall be stored
#' on the hard-disc (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return Named list with the dataframes as list entry saved as .Rda file.

#' @details This functions performs various calculations (details below) and saves the result as a
#' named list of dataframes.In addition results are saved to HDD as *.Rda file.
#'
#' 1. structn_age
#'    "StructN" for each age-structured group calculated as mean value
#'    per time, species and age over layer and polygon.
#' 2. resn_age
#'    "ResN" for each age-structured group calculated as mean value
#'    per time, species and age over layer and polygon.
#' 3. eat_age
#'    "Eat" for each age-structured group calculated as mean value
#'    per time, species and age over polygon.
#' 4. growth_age
#'    "Growth" for each age-structured group calculated as mean value
#'    per time, species and age over polygon.
#' 7. eat
#'    "Eat" for each age-structured group calculated as mean value
#'    per time and species over age and polygon.
#' 8. growth
#'    "Growth" for each age-structured group calculated as mean value
#'    per time and species over age and polygon.
#' 9. numbers
#'    "Nums" for each age-structured group calculated as summed value
#'    per time and species over age, polygon and layer.
#' 10. numbers_age
#'     "Nums" for each age-structured group calculated as summed value
#'     per time, species and age over polygon and layer.
#' 11. numbers_box
#'     "Nums" for each age-structured group calculated as summed value
#'     per time, species and polygon over age and layer.
#' 12. physics
#'     Given as mean value over all layers for each physical parameter and time.
#' 13. flux_layer
#'     Given as raw value for each layer and timestep.
#' 14. grazing
#'     "Grazing" for each biomasspool calculated as mean value
#'     per time and species over polygon.
#' 15. biomass_age
#'     Biomass as (StructN + ResN) * conversion_factor * Nums for each
#'     age-structured group (>= 10 cohorts) calculated as summed value
#'     per time, species and age over polygon and layer.
#' 16. biomass
#'     Biomass of non age-structured groups (< 10 cohorts) calculated as
#'     nitrogen * volume / dz * conversion_factor for epibenthic groups and
#'     nitrogen * volume * bio_conv for non epibenthic groups.
#'     Summed per time and species over polygon and layer. Combined with
#'     biomass_age Summed per time and species over ages.

#' @keywords gen
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' test <- preprocess(dir = d,
#'    nc_gen = "outputSETAS.nc",
#'    nc_prod = "outputSETASPROD.nc",
#'    prm_biol = "VMPA_setas_biol_fishing_Trunk.prm",
#'    prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
#'    bps = load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc"),
#'    fgs = "functionalGroups.csv",
#'    select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Diatom", "Zoo"),
#'    bboxes = get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm")),
#'    check_acronyms = TRUE,
#'    modelstart = "1991-01-01",
#'    out = "preprocess.Rda",
#'    report = TRUE,
#'    save_to_disc = FALSE)
#' @export

preprocess <- function(dir, nc_gen, nc_prod, prm_biol, prm_run, bps, fgs, select_groups, bboxes,
                       check_acronyms, modelstart, out, report = TRUE, save_to_disc = FALSE){

  age_groups <- get_age_groups(dir = dir, fgs = fgs)
  select_age_groups <- select_groups[is.element(select_groups, age_groups)]
  if (length(select_age_groups) == 0) stop("At least one age-structured group has to be selected!")

  select_other_groups <- select_groups[!is.element(select_groups, age_groups)]
  if (length(select_other_groups) == 0) stop("At least one non age-structured group has to be selected!")

  physic_var <- c("salt", "NO3", "NH3", "Temp", "Oxygen", "Si", "Det_Si", "DON", "Chl_a",
                  "Denitrifiction", "Nitrification", "Light")
  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  if (report) print("*** Start: reading in data! ***")
  # "load_atlantis_ncdf" and "load_atlantis_ncdf_physics" are independent functions in seperate R-files!
  # NOTE: Data for new plots has to be added here if not already available!
  if (report) print("*** Start: Reading in layerd data: structn")
  at_structn_l <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_age_groups,
                          select_variable = "StructN", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  if (report) print("*** Start: Reading in layerd data: resn")
  at_resn_l    <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_age_groups,
                          select_variable = "ResN", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  if (report) print("*** Start: Reading in layerd data: nums")
  at_nums_l    <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_age_groups,
                          select_variable = "Nums", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  if (report) print("*** Start: Reading in layerd data: n for invert groups")
  at_n_pools   <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_other_groups,
                          select_variable = "N", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  if (report) print("*** Start: Reading in productivity data")
  at_eat     <- load_nc(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, select_groups = select_age_groups,
                        select_variable = "Eat", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  at_growth  <- load_nc(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, select_groups = select_age_groups,
                        select_variable = "Growth", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  at_grazing <- load_nc(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, select_groups = select_other_groups,
                        select_variable = "Grazing", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  if (report) print("*** Start: Reading in physics data")
  flux       <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = c("eflux", "vflux"),
                                bboxes = bboxes, aggregate_layers = TRUE)

  physics    <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = physic_var,
                                bboxes = bboxes, aggregate_layers = FALSE)


  if (report) print("*** Start: data transformations! ***")
  # Aggregate Layers for N, Nums, ResN, StructN
#   at_n       <- agg_mean(data = at_n,         groups = c("species", "polygon", "time"))
#   at_resn    <- agg_mean(data = at_resn_l,    groups = c("species", "polygon", "agecl", "time"))
#   at_structn <- agg_mean(data = at_structn_l, groups = c("species", "polygon", "agecl", "time"))
#   at_nums    <- agg_sum(data = at_nums_l,     groups = c("species", "polygon", "agecl", "time"))

  # Calculate biomass for age-groups
  names(at_resn_l)[names(at_resn_l) == "atoutput"] <- "atresn"
  names(at_nums_l)[names(at_nums_l) == "atoutput"] <- "atnums"
  at_structn_l <- dplyr::inner_join(at_structn_l, at_nums_l)
  at_structn_l <- dplyr::left_join(at_structn_l, at_resn_l)
  at_structn_l$biomass_ind <- with(at_structn_l, (atoutput + atresn) * atnums * bio_conv)
  biomass_age <- agg_sum(data = at_structn_l, col = "biomass_ind", groups = c("species", "agecl", "time"))

  # Calculate biomass for non-age-groups
  vol <- load_nc_physics(dir = dir,
                         nc = nc_gen,
                         select_physics = c("volume", "dz"),
                         bboxes = bboxes,
                         aggregate_layers = F)

  vol <- tidyr::spread_(data = vol, key_col = c("variable"), value_col = "atoutput")

  at_n_pools <- dplyr::left_join(at_n_pools, vol)
  at_n_pools$biomass_ind <- with(at_n_pools, ifelse(species %in% bps, atoutput * volume / dz * bio_conv, atoutput * volume * bio_conv))
  biomass_pools <- agg_sum(data = at_n_pools, groups = c("species", "time"))

  # Combine with biomass from age-groups
  biomass <- biomass_age %>%
    dplyr::group_by_("species", "time") %>%
    dplyr::summarise_(atoutput = ~sum(atoutput)) %>%
    rbind(biomass_pools)

  # Aggregate Numbers! This is done seperately since numbers need to be summed!
  nums     <- agg_sum(data = at_nums_l, col = "atnums", groups = c("species", "time"))
  nums_age <- agg_sum(data = at_nums_l, col = "atnums", groups = c("species", "agecl", "time"))
  nums_box <- agg_sum(data = at_nums_l, col = "atnums", groups = c("species", "polygon", "time"))

  # Aggregate the rest of the dataframes by mean!
  structn_age <- agg_mean(data = at_structn_l, groups = c("species", "time", "agecl"))
  resn_age    <- agg_mean(data = at_resn_l,    groups = c("species", "time", "agecl"), col = "atresn")
  eat_age     <- agg_mean(data = at_eat,       groups = c("species", "time", "agecl"))
  growth_age  <- agg_mean(data = at_growth,    groups = c("species", "time", "agecl"))
  grazing     <- agg_mean(data = at_grazing,   groups = c("species", "time"))

  # WARNING: Newly created dataframes have to be added here!
  result <- list(
    "structn_age" = structn_age,
    "resn_age"    = resn_age,
    "eat_age"     = eat_age,
    "growth_age"  = growth_age,
    "nums"        = nums,
    "nums_age"    = nums_age,
    "nums_box"    = nums_box,
    "physics"     = physics,
    "flux"        = flux,
    "grazing"     = grazing,
    "biomass_age" = biomass_age,
    "biomass"     = biomass
  )

  # Convert timestep to actual time.
  result <- lapply(result, convert_time, dir = dir, prm_run = prm_run, modelstart = modelstart)

  # Convert Species names to Longnames!
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  for (i in seq_along(result)) {
    result[[i]]$species <- convert_factor(data_fgs = fgs_data, col = result[[i]]$species)
  }

  # Write rest to HDD
  if (report) print("*** Start: writing files! ***")
  if (save_to_disc) {
    if (!is.null(dir)) out <- file.path(dir, out)
    save(result, file = out)
    if (report) print("*** End: writing files!***")
  }

  if (report) print("*** End: Processing of data done. HURRAY!!!***")
  return(result)
}








