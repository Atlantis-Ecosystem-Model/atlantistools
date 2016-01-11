#' Preprocess Atlantis output (netcdf)
#'

#' This function loads data from Atlantis output (netcdf) files. performs various calculations (biomass, eat, growth, physics...)
#' and saves files to csv!
#' @param nc_out Connection of the ATLANTIS general netcdf output file given as complete folder/filename string.
#' Usually "output[...].nc".
#' @param nc_init Connection of the ATLANTIS init file given as complete folder/filename string. Usually "init[...].nc".
#' @param file_fgs Connection of the ATLANTIS functional groups file given as complete folder/filename string.
#' Usually "functionalGroups.csv".
#' @param nc_prod Connection of the ATLANTIS netcdf productivity output file given as complete folder/filename string.
#' Usually "output[...]PROD.nc".
#' @param prm_biol Connection of the ATLANTIS biological-paramter file given as complete folder/filename string.
#' Usually "[...]_biol.prm".
#' @param prm_run Connection of the ATLANTIS run-parameter file given as complete folder/filename string.
#' Usually "[...]_run_fishing.prm".
#' @param select_groups Character vector of funtional groups which shall be plotted. Names have to match the ones
#' used in the ncdf file. Check column "Name" in "functionalGroups.csv" for clarification.
#' @param report Logical specifying if additional notifications shall be printed.
#' @param check_acronyms Logical specifying if selected groups are active in the model run. This is used in automated runs.
#' Since all groups are passed when plotting (plot_atlantis) is called via batch-file (which cannot be changed easily)
#' this will result in errors if some groups are not active in the model run. By default this is TRUE.
#' @param output_path Character string giving the output path the processed files are written into.
#' @return 19 csv files.

#' @details This functions performs various calculations (details below) and saves the result as a named list of dataframes.
#' In addition results are saved to HDD as *.csv for easier data handling lateron! Following gives a summary of every list
#' element starting with the name of the list entry the name of the *.csv and a description for each file.
#' 01. agg_age_at_structn --> "preprocessed_agg_age_at_structn.csv"
#' "StructN" for each age-structured group calculated as mean value per time, species and age over layer and polygon.
#'
#' 02. agg_age_at_resn --> "preprocessed_agg_age_at_resn.csv"
#' "ResN" for each age-structured group calculated as mean value per time, species and age over layer and polygon.
#'
#' 03. agg_age_at_eat --> "preprocessed_agg_age_at_eat.csv"
#' "Eat" for each age-structured group calculated as mean value per time, species and age over polygon.
#'
#' 04. agg_age_at_growth --> "preprocessed_agg_age_at_growth.csv"
#' "Growth" for each age-structured group calculated as mean value per time, species and age over polygon.
#'
#' 05. agg_polygon_at_n --> "preprocessed_agg_polygon_at_n.csv"
#' "N" for each group calculated as mean value per time, species, polygon over layer.
#'
#' 06. agg_overview_at_n --> "preprocessed_agg_overview_at_n.csv"
#' "N" for each group calculated as mean value per time and species over layer and polygon.
#'
#' 07. agg_overview_at_eat --> "preprocessed_agg_overview_at_eat.csv"
#' "Eat" for each age-structured group calculated as mean value per time and species over age and polygon.
#'
#' 08. agg_overview_at_growth --> "preprocessed_agg_overview_at_growth.csv"
#' "Growth" for each age-structured group calculated as mean value per time and species over age and polygon.
#'
#' 09. at_nums_overview --> "preprocessed_at_nums_overview.csv"
#' "Nums" for each age-structured group calculated as summed value per time and species over age, polygon and layer.
#'
#' 10. at_nums_age --> "preprocessed_at_nums_age.csv"
#' "Nums" for each age-structured group calculated as summed value per time, species and age over polygon and layer.
#'
#' 11. at_nums_polygon --> "preprocessed_at_nums_polygon.csv"
#' "Nums" for each age-structured group calculated as summed value per time, species and polygon over age and layer.
#'
#' 12. biomass_ages --> "preprocessed_biomass_ages.csv"
#' Biomass as (StructN + ResN) * conversion_factor * Nums for each age-structured group calculated as summed value
#' per time, species and age over polygon and layer.
#'
#' 13. at_agestructure --> "preprocessed_at_agestructure.csv"
#' Relative Contribution of each ageclass to total number for each age-structured group over time.
#'
#' 14. biomass --> "preprocessed_biomass.csv"
#' Biomass as (StructN + ResN) * conversion_factor * Nums for each age-structured group calculated as summed value
#' per time, species and age over polygon and layer. Combined with the biomass of non age-structured groups whose biomass
#' is calculated as N * volume / dz * conversion_factor for sediment groups and atoutput * volume * bio_conv for non
#' sediment groups.

#' 15. physics --> "preprocessed_physics.csv"
#' Given as mean value over all layers for each physical parameter and timestep.
#'
#' 16. flux --> "preprocessed_flux.csv"
#' Given as raw value for each layer and timestep.
#'
#' 17. biomass_cor --> "preprocessed_biomass_cor.csv"
#' Correlation of biomass timeseries for each group with each other group. See "biomass" for details of biomass calculation.
#'
#' 18. biomass_pools --> "preprocessed_biomass_pools.csv"
#' Biomass of non age-structured groups. See "biomass" for details of biomass calculation.
#'
#' 19. at_grazing --> "preprocessed_at_grazing.csv"
#' "Grazing" for each biomasspool calculated as mean value per time and species over polygon.

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








