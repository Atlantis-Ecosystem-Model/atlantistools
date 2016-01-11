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

#' @param prm_biol Connection of the ATLANTIS biological-paramter file given as complete folder/filename string.
#' Usually "[...]_biol.prm".
#' @param prm_run Connection of the ATLANTIS run-parameter file given as complete folder/filename string.
#' Usually "[...]_run_fishing.prm".
#'
#' @param output_path Character string giving the output path the processed files are written into.
#' @return 19 csv files.

#' @details This functions performs various calculations (details below) and saves the result as a
#' named list of dataframes.In addition results are saved to HDD as *.Rda file.
#'
#' 1. structn_age
#'    "StructN" for each age-structured group calculated as mean value
#'    per time, species and age over layer and polygon.
#'
#' 2. resn_age
#'    "ResN" for each age-structured group calculated as mean value
#'    per time, species and age over layer and polygon.
#'
#' 3. eat_age
#'    "Eat" for each age-structured group calculated as mean value
#'    per time, species and age over polygon.
#'
#' 4. growth_age
#'    "Growth" for each age-structured group calculated as mean value per time, species and age over polygon.
#'
#' 5. nit_box
#'    "N" (nitroge) for each group calculated as mean value
#'    per time, species, polygon over layer.
#'
#' 6. nit
#'    "N" (nitrogen) for each group calculated as mean value
#'    per time and species over layer and polygon.
#'
#' 7. eat
#'    "Eat" for each age-structured group calculated as mean value
#'    per time and species over age and polygon.
#'
#' 8. growth
#'    "Growth" for each age-structured group calculated as mean value
#'    per time and species over age and polygon.
#'
#' 9. nums
#'    "Nums" for each age-structured group calculated as summed value
#'    per time and species over age, polygon and layer.
#'
#' 10. nums_age
#'     "Nums" for each age-structured group calculated as summed value
#'     per time, species and age over polygon and layer.
#'
#' 11. nums_box
#'     "Nums" for each age-structured group calculated as summed value
#'     per time, species and polygon over age and layer.
#'
#' 15. physics
#'     Given as mean value over all layers for each physical parameter and timestep.
#'
#' 16. flux
#'     Given as raw value for each layer and timestep.
#'
#' 19. grazing
#'     "Grazing" for each biomasspool calculated as mean value
#'     per time and species over polygon.

#' @keywords gen
#' @examples
#' preprocess_data(model_path = file.path("z:", "Atlantis", "ATLANTIS_NSmodel_base"), filename_output = "outputNorthSea.nc", filename_prod = "outputNorthSeaPROD.nc", select_groups = get_groups())
#' @export

preprocess <- function(nc_out,
                            file_fgs,
                            nc_prod,
                            prm_biol,
                            prm_run,
                            select_groups,
                            report,
                            check_acronyms,
                            output_path){

  age_groups <- get_age_groups(file_fgs = file_fgs)
  select_age_groups <- select_groups[is.element(select_groups, age_groups)]
  if (length(select_age_groups) == 0) stop("At least one age-structured group has to be selected!")

  select_other_groups <- select_groups[!is.element(select_groups, age_groups)]
  if (length(select_other_groups) == 0) stop("At least one non age-structured group has to be selected!")

  bps <- get_bps()
  physic_var <- get_physics()[-which(is.element(get_physics(), c("vflux", "eflux")))]
  bio_conv <- conv_nmg_to_biomt(prm = prm_biol)

  if (report) print("*** Start: reading in data! ***")
  # "load_atlantis_ncdf" and "load_atlantis_ncdf_physics" are independent functions in seperate R-files!
  # NOTE: Data for new plots has to be added here if not already available!
  if (report) print("*** Start: Reading in layerd data: n")
  at_n       <- load_atlantis_ncdf(nc_out = nc_out,
                                   file_fgs = file_fgs,
                                   select_groups = select_groups,
                                   select_variable = "N",
                                   remove_bboxes = T,
                                   check_acronyms = check_acronyms)

  if (report) print("*** Start: Reading in layerd data: structn")
  at_structn_l <- load_atlantis_ncdf(nc_out = nc_out,
                                     file_fgs = file_fgs,
                                     select_groups = select_age_groups,
                                     select_variable = "StructN",
                                     remove_bboxes = T,
                                     check_acronyms = check_acronyms)

  if (report) print("*** Start: Reading in layerd data: resn")
  at_resn_l    <- load_atlantis_ncdf(nc_out = nc_out,
                                     file_fgs = file_fgs,
                                     select_groups = select_age_groups,
                                     select_variable = "ResN",
                                     remove_bboxes = T,
                                     check_acronyms = check_acronyms)

  if (report) print("*** Start: Reading in layerd data: nums")
  at_nums_l    <- load_atlantis_ncdf(nc_out = nc_out,
                                     file_fgs = file_fgs,
                                     select_groups = select_age_groups,
                                     select_variable = "Nums",
                                     remove_bboxes = T,
                                     check_acronyms = check_acronyms)

  if (report) print("*** Start: Reading in layerd data: n for invert groups")
  at_n_pools   <- load_atlantis_ncdf(nc_out = nc_out,
                                     file_fgs = file_fgs,
                                     select_groups = select_other_groups,
                                     select_variable = "N",
                                     remove_bboxes = T,
                                     check_acronyms = check_acronyms)

  if (report) print("*** End: Reading in layerd data")
  at_eat     <- load_atlantis_ncdf(nc_out = nc_prod,
                                   file_fgs = file_fgs,
                                   select_groups = select_age_groups,
                                   select_variable = "Eat",
                                   remove_bboxes = T,
                                   check_acronyms = check_acronyms)

  at_growth  <- load_atlantis_ncdf(nc_out = nc_prod,
                                   file_fgs = file_fgs,
                                   select_groups = select_age_groups,
                                   select_variable = "Growth",
                                   remove_bboxes = T,
                                   check_acronyms = check_acronyms)

  flux       <- load_atlantis_ncdf_physics(nc_out = nc_out,
                                           physic_variables = c("eflux", "vflux"),
                                           aggregate_layers = F,
                                           remove_bboxes = T)

  physics    <- load_atlantis_ncdf_physics(nc_out = nc_out,
                                           physic_variables = physic_var,
                                           aggregate_layers = T,
                                           remove_bboxes = T)

  at_grazing <- load_atlantis_ncdf(nc_out = nc_prod,
                                   file_fgs = file_fgs,
                                   select_groups = select_other_groups,
                                   select_variable = "Grazing",
                                   remove_bboxes = T,
                                   check_acronyms = check_acronyms)

  if (report) print("*** Start: data transformations! ***")
  # Aggregate Layers for N, Nums, ResN, StructN
  at_n <- at_n %>%
    dplyr::group_by(species, polygon, time) %>%
    dplyr::summarise(atoutput = mean(atoutput))
  at_resn <- at_resn_l %>%
    dplyr::group_by(species, polygon, agecl, time) %>%
    dplyr::summarise(atoutput = mean(atoutput))
  at_structn <- at_structn_l %>%
    dplyr::group_by(species, polygon, agecl, time) %>%
    dplyr::summarise(atoutput = mean(atoutput))
  at_nums <- at_nums_l %>%
    dplyr::group_by(species, polygon, agecl, time) %>%
    dplyr::summarise(atoutput = sum(atoutput))

  # Calculate biomass for age-groups
  names(at_resn_l)[names(at_resn_l) == "atoutput"] <- "atresn"
  names(at_nums_l)[names(at_nums_l) == "atoutput"] <- "atnums"
  at_structn_l <- dplyr::inner_join(at_structn_l, at_nums_l)
  at_structn_l <- dplyr::left_join(at_structn_l, at_resn_l)
  at_structn_l$biomass_ind <- with(at_structn_l, (atoutput + atresn) * atnums * bio_conv)

  # At this point at_resn_l, at_nums_l are not needed anymore!
  rm(at_resn_l, at_nums_l)
  gc()

  biomass <- at_structn_l %>%
    dplyr::group_by(species, time) %>%
    dplyr::summarise(atoutput = sum(biomass_ind)) %>%
    dplyr::mutate(model = "atlantis")
  # Biomass per ageclass
  biomass_ages <- at_structn_l %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(atoutput = sum(biomass_ind))

  # At this point at_structn_l is not needed anymore!
  rm(at_structn_l)
  gc()

  # Aggregate Numbers! This is done seperately since numbers need to be summed!
  at_nums_age <- at_nums %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(atoutput = sum(atoutput))
  at_nums_polygon <- at_nums %>%
    dplyr::group_by(species, polygon, time) %>%
    dplyr::summarise(atoutput = sum(atoutput))
  at_nums_overview <- at_nums %>%
    dplyr::group_by(species, time) %>%
    dplyr::summarise(atoutput = sum(atoutput))

  # At this point at_nums is not needed anymore!
  rm(at_nums)
  gc()

  # Calculate biomass for non-age-groups
  vol <- load_atlantis_ncdf_physics(nc_out = nc_out,
                                    physic_variables = c("volume", "dz"),
                                    aggregate_layers = F,
                                    remove_bboxes = T)

  vol <- reshape2::dcast(vol, polygon + layer + time ~ variable, value.var = "atoutput")

  at_n_pools <- dplyr::left_join(at_n_pools, vol)
  at_n_pools$biomass_ind <- with(at_n_pools, ifelse(species %in% bps, atoutput * volume / dz * bio_conv, atoutput * volume * bio_conv))
  biomass_pools <- at_n_pools %>%
    dplyr::group_by(species, time) %>%
    dplyr::summarise(atoutput = sum(biomass_ind))

  # Put everyhing together
  biomass_pools_dummy <- biomass_pools
  biomass_pools_dummy$model <- "atlantis"
  biomass <- rbind(biomass, biomass_pools_dummy)

  # Correlation matrix of biomass time-series!
  biomass_cor <- biomass %>%
    reshape2::dcast(time ~ species, value.var = "atoutput", fill = 0) %>%
    dplyr::select(-time) %>%
    as.matrix() %>%
    cor() %>%
    as.data.frame() %>%
    stack()
  biomass_cor$x <- rep(unique(biomass_cor$ind), times = length(unique(biomass_cor$ind)))

  # NOTE: New dataframes also have to be added here depending on the calculations needed!
  agg_age      <- lapply(list(at_structn, at_resn, at_eat, at_growth), mean_over_ages)
  agg_polygon  <- lapply(list(at_n), mean_over_polygons)
  agg_overview <- lapply(list(at_n, at_eat, at_growth, at_grazing), mean_overview)

  # At this point at_structn, at_resn, at_eat and at_growth are not needed anymore!
  rm(at_structn, at_resn, at_eat, at_growth, at_n)
  gc()

  # Calculate agestructure per group and timestep!
  at_agestructure <- at_nums_age %>%
    dplyr::group_by(species, time) %>%
    dplyr::mutate(atoutput = atoutput / sum(atoutput))

  # WARNING: Newly created dataframes have to be added here!
  result <- list(
    "agg_age_at_structn"     = agg_age[[1]],
    "agg_age_at_resn"        = agg_age[[2]],
    "agg_age_at_eat"         = agg_age[[3]],
    "agg_age_at_growth"      = agg_age[[4]],
    "agg_polygon_at_n"       = agg_polygon[[1]],
    "agg_overview_at_n"      = agg_overview[[1]],
    "agg_overview_at_eat"    = agg_overview[[2]],
    "agg_overview_at_growth" = agg_overview[[3]],
    "at_nums_overview"       = at_nums_overview,
    "at_nums_age"            = at_nums_age,
    "at_nums_polygon"        = at_nums_polygon,
    "biomass_ages"           = biomass_ages,
    "at_agestructure"        = at_agestructure,
    "biomass"                = biomass,
    "physics"                = physics,
    "flux"                   = flux,
    "biomass_cor"            = biomass_cor,
    "biomass_pools"          = biomass_pools,
    "at_grazing"             = agg_overview[[4]]
  )

  # Write rest to HDD
  if (report) print("*** Start: writing files! ***")
  for (i in seq_along(result)) {
    write.csv(result[[i]], file.path(output_path, paste0("preprocessed_", names(result)[i], ".csv")), row.names = F, quote = F)
  }
  if (report) print("*** End: writing files! All Done! ***")

  return(result)
}








