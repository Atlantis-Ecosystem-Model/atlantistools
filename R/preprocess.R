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
#' folder/filename string as nc_gen. In addition set dir to 'NULL' in this
#' case.
#' @param nc_prod Character string giving the filename of the productivity netcdf
#' file. Usually "output[...]PROD.nc". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc_prod. In addition set dir to 'NULL' in this
#' case.
#' @param dietcheck Character string of the DietCheck.txt file. Usually
#' 'output[...]DietCheck.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as dietcheck. In addition set dir to 'NULL' in this
#' case.
#' @param yoy Character string of the YOY.txt file. Usually
#' 'output[...]YOY.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as yoy. In addition set dir to 'NULL' in this
#' case.
#' @param ssb Character string of the SSB.txt file. Usually
#' 'output[...]SSB.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as ssb. In addition set dir to 'NULL' in this
#' case.
#' @param specmort Character string of the SpecMort.txt file. Usually
#' 'output[...]SpecificPredMort.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as specmort. In addition set dir to 'NULL' in this
#' case.
#' @param specpredmort Character string of the SpecPredMort.txt file. Usually
#' 'output[...]SpecificPredMort.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as specmort. In addition set dir to 'NULL' in this
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
#' @param as_date Convert the time into a date format (yyyy-mm-dd) \code{TRUE} or
#' not \code{FALSE}. Default is \code{FALSE}.
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}. Only needed in case \code{as_date} is set to \code{TRUE}.
#' @param out Character string giving the filename of the *.Rda output
#' file. In case you are using different folders for your model data
#' and output files please pass a complete folder/filename string and
#' set dir to 'NULL'.
#' @param report Logical indicating if additional information shall be printed
#' during function evaluation.
#' @param save_to_disc Logical indicating if the resulting list shall be stored
#' on the hard-disc (\code{TRUE}) or not (\code{FALSE}).
#' @param version_flag The version of atlantis that created the output files. 1 for bec_dev, 2 for trunk.
#'
#' @return Named list with the dataframes as list entry saved as .Rda file.

#' @details This functions performs various calculations (use \code{?preprocess_setas} for details)
#' and saves the result as a named list of dataframes. In addition results are saved to HDD as *.Rda file.
#'

#' @keywords gen
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' test <- preprocess(dir = d,
#'    nc_gen = "outputSETAS.nc",
#'    nc_prod = "outputSETASPROD.nc",
#'    dietcheck = "outputSETASDietCheck.txt",
#'    yoy = "outputSETASYOY.txt",
#'    ssb = "outputSETASSSB.txt",
#'    specmort = "outputSETASSpecificMort.txt",
#'    specpredmort = "outputSETASSpecificPredMort.txt",
#'    prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    bps = load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc"),
#'    fgs = "SETasGroups.csv",
#'    select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Diatom", "Zoo"),
#'    bboxes = get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm")),
#'    check_acronyms = TRUE,
#'    modelstart = "1991-01-01",
#'    out = "preprocess.Rda",
#'    save_to_disc = FALSE,
#'    version_flag = 1)
#' @export

#BJS 7/6/16 change to be compatible with trunk version; added version_flag
preprocess <- function(dir = getwd(), nc_gen, nc_prod, dietcheck, yoy, ssb, specmort, specpredmort, prm_biol, prm_run, bps, fgs, select_groups, bboxes,
                       check_acronyms, as_date = FALSE, modelstart = NULL, out, report = TRUE, save_to_disc = FALSE, version_flag = 1){

  age_groups <- get_age_groups(dir = dir, fgs = fgs)
  select_age_groups <- select_groups[is.element(select_groups, age_groups)]
  if (length(select_age_groups) == 0) stop("At least one age-structured group has to be selected!")

  select_other_groups <- select_groups[!is.element(select_groups, age_groups)]
  if (length(select_other_groups) == 0) stop("At least one non age-structured group has to be selected!")

  physic_var <- c("salt", "NO3", "NH3", "Temp", "Oxygen", "Si", "Det_Si", "DON", "Chl_a",
                  "Denitrifiction", "Nitrification", "Light")
  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  message("Start reading in data!")
  # "load_atlantis_ncdf" and "load_atlantis_ncdf_physics" are independent functions in seperate R-files!
  # NOTE: Data for new plots has to be added here if not already available!
  message("01 layerd data\nstructn")
  at_structn_l <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_age_groups,
                          select_variable = "StructN", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("resn")
  at_resn_l    <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_age_groups,
                          select_variable = "ResN", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("nums")
  at_nums_l    <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_age_groups,
                          select_variable = "Nums", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("02 non-layerd data\nn for invert groups")
  at_n_pools   <- load_nc(dir = dir, nc = nc_gen, bps = bps, fgs = fgs, select_groups = select_other_groups,
                          select_variable = "N", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("eat")
  at_eat     <- load_nc(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, select_groups = select_age_groups,
                        select_variable = "Eat", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("growth")
  at_growth  <- load_nc(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, select_groups = select_age_groups,
                        select_variable = "Growth", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("grazing")
  at_grazing <- load_nc(dir = dir, nc = nc_prod, bps = bps, fgs = fgs, select_groups = select_other_groups,
                        select_variable = "Grazing", bboxes = bboxes, check_acronyms = check_acronyms, report = report)

  message("03 physics data\nfluxes")
  flux       <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = c("eflux", "vflux"),
                                bboxes = bboxes, aggregate_layers = FALSE)

  message("physical variables")
  physics    <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = physic_var,
                                bboxes = bboxes, aggregate_layers = TRUE)


  if (report) message("Start data transformations!")
  # Aggregate Layers for N, Nums, ResN, StructN
#   at_n       <- agg_data(data = at_n,         groups = c("species", "polygon", "time"), fun = mean)
#   at_resn    <- agg_data(data = at_resn_l,    groups = c("species", "polygon", "agecl", "time"), fun = mean)
#   at_structn <- agg_data(data = at_structn_l, groups = c("species", "polygon", "agecl", "time"), fun = mean)
#   at_nums    <- agg_data(data = at_nums_l,     groups = c("species", "polygon", "agecl", "time"), fun = sum)

  # Calculate biomass for age-groups
  names(at_resn_l)[names(at_resn_l) == "atoutput"] <- "atresn"
  names(at_nums_l)[names(at_nums_l) == "atoutput"] <- "atnums"
  at_structn_l <- dplyr::inner_join(at_structn_l, at_nums_l)
  at_structn_l <- dplyr::left_join(at_structn_l, at_resn_l)
  at_structn_l$biomass_ind <- with(at_structn_l, (atoutput + atresn) * atnums * bio_conv)
  biomass_age <- agg_data(data = at_structn_l, col = "biomass_ind", groups = c("species", "agecl", "time"), fun = sum)

  # Calculate biomass for non-age-groups
  vol <- load_nc_physics(dir = dir,
                         nc = nc_gen,
                         select_physics = c("volume", "dz"),
                         bboxes = bboxes,
                         aggregate_layers = F)

  vol <- tidyr::spread_(data = vol, key_col = c("variable"), value_col = "atoutput")

  at_n_pools <- dplyr::left_join(at_n_pools, vol)
  at_n_pools$atoutput <- with(at_n_pools, ifelse(species %in% bps, atoutput * volume / dz * bio_conv, atoutput * volume * bio_conv))
  biomass_pools <- agg_data(data = at_n_pools, groups = c("species", "time"),  fun = sum)

  # Combine with biomass from age-groups
  biomass <- biomass_age %>%
    dplyr::group_by_("species", "time") %>%
    dplyr::summarise_(atoutput = ~sum(atoutput)) %>%
    rbind(biomass_pools)

  # Aggregate Numbers! This is done seperately since numbers need to be summed!
  nums     <- agg_data(data = at_nums_l, col = "atnums", groups = c("species", "time"), fun = sum)
  nums_age <- agg_data(data = at_nums_l, col = "atnums", groups = c("species", "agecl", "time"), fun = sum)
  nums_box <- agg_data(data = at_nums_l, col = "atnums", groups = c("species", "polygon", "time"), fun = sum)

  # Aggregate the rest of the dataframes by mean!
  structn_age <- agg_data(data = at_structn_l, groups = c("species", "time", "agecl"), fun = mean)
  resn_age    <- agg_data(data = at_resn_l,    groups = c("species", "time", "agecl"), col = "atresn", fun = mean)
  eat_age     <- agg_data(data = at_eat,       groups = c("species", "time", "agecl"), fun = mean)
  growth_age  <- agg_data(data = at_growth,    groups = c("species", "time", "agecl"), fun = mean)
  grazing     <- agg_data(data = at_grazing,   groups = c("species", "time"), fun = mean)

  # Load in diet-data!
  message("Read in DietCheck.txt!")
  diet <- load_dietcheck(dir = dir, dietcheck = dietcheck, fgs = fgs, report = report, version_flag = version_flag) #bjs pass the report flag so dietcheck doesnt always report no matter on the setting; add version_flag)

  # load in recruitment data!
  message("Read in SSB/REC data!")
  ssb_rec <- load_rec(dir = dir, yoy = yoy, ssb = ssb, prm_biol = prm_biol)

  # load in specific mortality data!
  message("Read in SpecPredMort data!")
  spec_pred_mort <- load_spec_mort(dir = dir, specmort = specpredmort, version_flag = version_flag) #bjs add version_flag

  message("Read in SpecMort data!")
  spec_mort <- load_txt(dir = dir, file = specmort)
  spec_mort <- preprocess_txt(df_txt = spec_mort, into = c("species", "agecl", "empty_col", "mort"))

  # WARNING: Newly created dataframes have to be added here!
  result <- list(
    "biomass"        = biomass,       #1
    "biomass_age"    = biomass_age,
    "diet_dietcheck" = diet,
    "diet_specmort"  = spec_pred_mort,
    "eat_age"        = eat_age,       #5
    "flux"           = flux,
    "grazing"        = grazing,
    "growth_age"     = growth_age,
    "nums"           = nums,
    "nums_age"       = nums_age,      #10
    "nums_box"       = nums_box,
    "physics"        = physics,
    "resn_age"       = resn_age,
    "spec_mort"      = spec_mort,
    "ssb_rec"        = ssb_rec,
    "structn_age"    = structn_age    #15
  )

  # Convert timestep to actual time.
  result <- lapply(result, convert_time, dir = dir, prm_run = prm_run, as_date = as_date, modelstart = modelstart)

  # According to the Atlantis wiki DietCheck.txt gives an indication of how much a group is eating
  # If the prey is an inverebrate value units are mgN/m^3 per second
  # --> Multiply value with volume of model domain!
  # If the prey is a vertebrate value units are nums per second
  # --> Multiply with individual weight!
  # Currently it is not possible to merge the dataframes vol with diet because there are different
  # timesteps used (Diet is based on tsumout while vol is based on toutinc) which may differ for
  # most simulations. I checked our model and tested if the volume does change significantly over
  # time... Which it does not. Therefore we use the mean volume over the whole simulation as proxy!
  # This is not 100% correct! In addition individual weight changes drastically over time and merging
  # with diet data is necessary (but impossbiel due to different time steps) here! This will only
  # work if toutinc and tsumout are equal!
  # vol_mult <- mean(agg_sum(vol, col = "volume", groups = "time")$atoutput)
  # vol_epi <- mean(agg_sum(subset(vol, layer == max(vol$layer)), col = "volume", groups = "time")$atoutput)
  #
  # result$diet$time <- result$diet$time / min(result$diet$time)

  # Convert Species names to Longnames!
  for (i in seq_along(result)) {
    # Do not convert acronym names in dietdataframes! This will increase readybility of diet plots ALOT!
    # Otherwise lots of space is lost due to long species names.
    # if (grepl(pattern = "diet", x = names(result)[i])) {
    #   result[[i]]$pred <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = result[[i]]$pred)
    #   result[[i]]$prey <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = result[[i]]$prey)
    # }
    # exlude physics dataframes!
    if (is.element("species", names(result[[i]])) && !grepl(pattern = "diet", x = names(result)[i])) {
      result[[i]]$species <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = result[[i]]$species)
    }
  }

  # Ungroup dataframes!
  result <- lapply(result, dplyr::ungroup)

  # Write rest to HDD
  if (save_to_disc) {
    message("Write preprocessed list as *.rda")
    if (!is.null(dir)) out <- file.path(dir, out)
    save(result, file = out)
  }

  message("Finished preprocessing of data HURRAY!!!")
  return(result)
}



# wawa <- list()
# for (i in seq_along(result)) {
#   wawa[[i]] <- convert_time(dir = dir, prm_run = prm_run, data = result[[i]], modelstart = modelstart)
#   print(i)
# }



