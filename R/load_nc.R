#' Load Atlantis outputfiles (netcdf)
#'
#' This function loads Atlantis outputfiles (any netcdf file) and
#' converts them to a \code{data.frame}.
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param nc Character string giving the filename of netcdf file which
#' shall be read in. Usually "output[...].nc". Currently the general-
#' production- and catch.nc files can be loaded in. In case you are using
#' multiple folder for your model files and outputfiles pass the complete
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
#' @param select_variable Character value spefifying which variable to load.
#' Only one variable of the options available (i.e., \code{c(
#' "N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing")
#' }) can be loaded at a time.
#' @param bboxes Integer vector giving the box-id of the boundary boxes.
#' @param check_acronyms Logical testing if functional-groups in
#' select_groups are inactive in the current model run. The will be omitted
#' in the output.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   species, timestep, polygon, agecl, and atoutput (i.e., variable).
#'
#' @keywords gen
#' @author Alexander Keth
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' bps <- load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")
#' bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))
#' test <- load_nc(dir = d, nc = "outputSETAS.nc",
#'   bps = bps,
#'   fgs = "functionalGroups.csv",
#'   select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
#'   select_variable = "ResN",
#'   bboxes = bboxes,
#'   check_acronyms = TRUE)
#' str(test)

# # Import '%>%' operator from magrittr
# #' @importFrom magrittr %>%
# #' @export
# magrittr::`%>%`

load_nc <- function(dir = getwd(), nc, bps, fgs, select_groups,
                    select_variable, bboxes = c(0), check_acronyms = TRUE) {
  # NOTE: The extraction procedure may look a bit complex... A different approach would be to
  # create a dataframe for each variable (e.g. GroupAge_Nums) and combine all dataframes
  # at the end. However, this requires alot more storage and the code wouldn't be highly
  # vectorised (which it is at the moment...)!
  supported_variables <- c("N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing")
  if (length(select_groups) == 0) stop("No functional groups selected.")
  if (length(select_variable) == 0) stop("No variable selected.")
  if (length(select_variable) > 1) stop("Only one variable allowed per function call.")
  if (any(!is.element(select_variable, supported_variables))) {
    stop(paste("Only", paste(supported_variables, collapse = ", "), "can be selected as 'select_variable'"))
  }

  fgs <- load_fgs(dir = dir, fgs = fgs)

  # Check input of the nc file
  if (tail(strsplit(nc, "\\.")[[1]], 1) != "nc") {
    stop("The argument for nc,", nc, "does not end in nc")
  }
  if (!is.null(dir)) nc <- file.path(dir, nc)

  # Check input structure!
  if (check_acronyms) {
    active_groups <- as.vector(subset(fgs, IsTurnedOn == 1)$Name)
    inactive_groups <- select_groups[which(!is.element(select_groups, active_groups))]
    if (length(inactive_groups) >= 1) {
      select_groups <- select_groups[!is.element(select_groups, inactive_groups)]
      warning(paste(paste("Some selected groups are not active in the model run. Check 'IsTurnedOn' in fgs\n"),
                    paste(inactive_groups, collapse = "\n")))
    }
    if (all(!is.element(select_groups, active_groups))) {
      stop(paste("None of the species selected are active in the model run.",
                 "Check spelling and Check 'IsTurnedOn' in fgs"))
    }
  }

  # Deal with file structures

  # Load ATLANTIS output!
  at_out <- RNetCDF::open.nc(con = nc)
  on.exit(RNetCDF::close.nc(at_out))

  if (select_variable != "N" & all(is.element(select_groups, bps))) stop("The only output for Biomasspools is N.")

  # Get info from netcdf file! (Filestructure and all variable names)
  var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_out)$nvars - 1),
                           function(x) RNetCDF::var.inq.nc(at_out, x)$name)
  n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
  n_boxes     <- RNetCDF::dim.inq.nc(at_out, 1)$length
  n_layers    <- RNetCDF::dim.inq.nc(at_out, 2)$length

  # Extract data from the ncdf file! Create a vector of all potential variable names first! Only use names which
  # are available in the ncdf-file as an extraction of missing variables is not possible! Unfortunately variable
  # names for Prodn and Garzing use a "" instead of "_" as seperator... :)
  # Create vecotr of available species at the end using search_clean! This is needed to create species-names
  # lateron! This approach may seem complicate but it turns out that this approach is very robust since no
  # user input is needed as the variable names are basically extracted from the available names in the ncdf file!
  # WARNING: Only 10 cohorts are supported at the moment!
  # In order to make the creation of variables as robust as possible we introduce differtent combinations
  # of groups, variable and cohort! Only combinations present in the ncdf are used lateron! This makes the
  # code both robust and fast!
  # Loop over select_groups to use the same ordering! This is essential otherwise species names
  # are not assigned correctly lateron!
  cohorts <- 1:10
  search <- list()
  for (i in seq_along(select_groups)) {
    search[[i]] <- c(unlist(lapply(paste0(select_groups[i], cohorts),                   paste0, select_variable)),           # GroupCohortVariable
                     unlist(lapply(paste0(select_groups[i], select_variable),           paste0, cohorts)),                   # GroupVariableCohort
                     unlist(lapply(paste0(select_groups[i], cohorts),                   paste, select_variable, sep = "_")), # GroupCohort_Variable
                     unlist(lapply(paste(select_groups[i], select_variable, sep = "_"), paste0, cohorts)),                   # Group_VariableCohort
                     unlist(lapply(paste(select_groups[i], cohorts, sep = "_"),         paste, select_variable, sep = "_")), # Group_Cohort_Variable
                     unlist(lapply(paste(select_groups[i], select_variable, sep = "_"), paste, cohorts, sep = "_")),         # Group_Variable_Cohort
                     unlist(paste0(select_groups[i], select_variable)),                                                      # GroupVariable
                     unlist(paste(select_groups[i], select_variable, sep = "_")))                                            # Group_Variable
    search[[i]] <- search[[i]][is.element(search[[i]], var_names_ncdf)]
    search[[i]] <- unique(search[[i]])
  }
  search_clean <- do.call(c, search)
  # If the combination of select_groups and select_variable ends up not being found.
  if (length(search_clean) == 0) return(0)

  at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = at_out)

  # Get final species and number of ageclasses per species
  final_species <- select_groups[sapply(lapply(select_groups, grepl, x = search_clean), any)]
  final_agecl <- fgs$NumCohorts[sapply(final_species, function(x) which(x == fgs$Name))]

  num_layers <- RNetCDF::var.get.nc(ncfile = at_out, variable = "numlayers")[,1]
  # add sediment layer!
  num_layers <- num_layers + ifelse(num_layers == 0, 0, 1)

  # Create an array of layerids.
  # Every entry in the array indicates if a layer is present (= 1) or not (= 0).
  # Boxes without layers (= islands) have only 0s as id,
  # used later on to remove data from non-existent layers!
  # By default output should be 0 in those layers.
  # Layers in boundary boxes are set to 0 if bboxes is anything other than NULL!
  # Applying a boolean array to an array results in a vector!
  for (i in seq_along(num_layers)) {
    if (i == 1) layerid <- array(dim = c(n_layers, n_boxes))
    if (num_layers[i] == 0) {
      layerid[, i] <- 0
    } else {
      if (!is.null(bboxes) & is.element((i - 1), bboxes)) {
        layerid[, i] <- 0
      } else {
        layerid[, i] <- c(rep(0, times = n_layers - num_layers[i]), rep(1, times = num_layers[i]))
      }
    }
  }

  # Create vectors for polygons and layers! Each vector has the length equal to one timestep!
  # All data from islands and non-existent layers is removed! Therefore the length of these
  # vectors is equal for each extracted variable!
  boxes <- 0:(n_boxes - 1)
  # Remove islands! and boundary boxes!
  island_ids <- num_layers == 0
  if (!is.null(bboxes)) {
    boundary_ids <- is.element(boxes, bboxes)
    island_ids <- island_ids | boundary_ids
  }
  boxes <- boxes[!island_ids]
  num_layers <- num_layers[!island_ids]

  polygons <- rep(boxes, times = num_layers)
  layers <- sapply(num_layers[num_layers != 0] - 2, function(x) c(seq(x, from = 0, by = 1), n_layers - 1))
  if (any(sapply(layers, length) != num_layers[num_layers != 0])) {
    stop("Number of layers incorrect. Contact package development team.")
  }
  layers <- do.call(c, layers)
  if (length(polygons) != length(layers)) stop("Number of polygons and layers do not match. Contact package development team.")

  # In the following section the data is transformed to a long dataframe! The code is written for speed!
  # I haven't found any solution to vectorise the creation of the dataframe columns (species, age, polygons,...)
  # when data from 2d and 3d arrays (e.g. select_variable = "N" all biomasspools are only present in the
  # sediment layer.) are read in simultaneausly. Therefore the current "messy" solution splits tha data
  # in 2 subpopulations: 2d-data and 3d-data!
  at_data3d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 3)]
  at_data2d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 2)]

  int_fs <- final_species
  int_fa <- final_agecl

  if (length(at_data3d) >= 1) {
    # Remove biomasspools if selected variable is "N"!
    if (select_variable == "N") {
      int_fs <- final_species[!is.element(final_species, bps)]
      int_fa <- final_agecl[!is.element(final_species, bps)]
      # Note this only works if age-structured vertebrates have 10 ageclasses!
      int_fa[int_fa == 10] <- 1
    }
    for (i in seq_along(at_data3d)) {# for loop over all variables!
      if (i == 1) result3d <- list()
      for (j in 1:n_timesteps) {# loop over timesteps
        if (j == 1) values <- array(dim = c(length(layers), n_timesteps))
        values[, j] <- at_data3d[[i]][,, j][layerid == 1]
      }
      result3d[[i]] <- as.vector(values)
    }
    result3d <- data.frame(species = unlist(sapply(X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = F, USE.NAMES = F),
                                                   FUN = rep, each = length(layers) * n_timesteps, simplify = F)),
                           agecl = unlist(sapply(X = sapply(X = int_fa, FUN = seq, from = 1, by = 1, simplify = F, USE.NAMES = F),
                                                 FUN = rep, each = length(layers) * n_timesteps, simplify = F)),
                           polygon = unlist(sapply(X = n_timesteps * int_fa, FUN = rep, x = polygons, simplify = F, USE.NAMES = F)),
                           layer = unlist(sapply(X = n_timesteps * int_fa, FUN = rep, x = layers, simplify = F, USE.NAMES = F)),
                           time = unlist(sapply(X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1), each = length(layers)), simplify = F, USE.NAMES = F)),
                           atoutput = do.call(c, result3d),
                           stringsAsFactors = F)
  }

  if (length(at_data2d) >= 1) {
    # Only select biomasspools if selected variable is "N"!
    if (select_variable == "N") {
      int_fs <- final_species[is.element(final_species, bps)]
      int_fa <- final_agecl[is.element(final_species, bps)]
    }
    if (select_variable == "Grazing") int_fa <- 1 # age-structured invert groups are combined in ncdf file!
    for (i in seq_along(at_data2d)) {# for loop over all variables!
      if (i == 1) result2d <- list()
      for (j in 1:n_timesteps) {# loop over timesteps
        if (j == 1) values <- array(dim = c(length(boxes), n_timesteps))
        values[, j] <- at_data2d[[i]][, j][boxes + 1]
      }
      result2d[[i]] <- as.vector(values)
    }

    # Order of the data in value column = "atoutput".
    # 1. species  --> rep each with the number of ageclasses and n_timesteps * boxes
    # 2. age      --> rep each (1:10 for each species) with n_timesteps * boxes
    # 3. timestep --> rep each timestep (1:n_timesteps) with the number of boxes and final_agecl
    #                 (num cohorts per species)
    # 4. polygon  --> rep boxes times n_timesteps * final_agecl (num cohorts per species)
    # The code is highly vectorized and therefore quite effective!
    result2d <- data.frame(species = unlist(sapply(X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = F, USE.NAMES = F),
                                                   FUN = rep, each = length(boxes) * n_timesteps, simplify = F)),
                           agecl = unlist(sapply(X = sapply(X = int_fa, FUN = seq, from = 1, by = 1, simplify = F, USE.NAMES = F),
                                                 FUN = rep, each = length(boxes) * n_timesteps, simplify = F)),
                           polygon = unlist(sapply(X = n_timesteps * int_fa, FUN = rep, x = boxes, simplify = F, USE.NAMES = F)),
                           time = unlist(sapply(X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1), each = length(boxes)), simplify = F, USE.NAMES = F)),
                           atoutput = do.call(c, result2d),
                           stringsAsFactors = F)
    if (select_variable == "N") result2d$layer <- n_layers - 1
  }

  # Combine dataframes if necessary!
  if (all(sapply(lapply(at_data, dim), length) == 3) & select_variable != "N") result <- result3d
  if (all(sapply(lapply(at_data, dim), length) == 2) & select_variable != "N") result <- result2d
  if (select_variable == "N") {
    if (length(at_data2d) >= 1 & length(at_data3d) == 0) result <- result2d
    if (length(at_data2d) == 0 & length(at_data3d) >= 1) result <- result3d
    if (length(at_data2d) >= 1 & length(at_data3d) >= 1) result <- rbind(result2d, result3d)
  }

  # Remove min_pools if existent (well, there always are min pools... ;)).
  min_pools <- is.element(result$atoutput, c(0, 1e-08, 1e-16))
  if (length(min_pools) > 0) {
    # exclude 1st timestep and sediment layer from calculation! They behave differently...
    print_min_pools <- sum(min_pools) - length(result[min_pools & result$time == 1, 1]) - length(result[min_pools & result$time > 1 & result$layer == 7, 1])
    if (print_min_pools > 0) warning(paste0(round(print_min_pools/dim(result)[1] * 100), "% of ", select_variable, " are true min-pools (0, 1e-08, 1e-16)"))
    result <- result[!min_pools, ]
  }

  # Remove non-existent layers.
  # WARNING: Biomass is build up (very few) in sediment layer for NON sediment groups (e.g. baleen whales)
  # Therefore, I subset all data from that layer for non biomass groups and groups which cannot penetrate into the sediment!
  # UPDATE: Doesn't work with layers as species are not distributed through the whole water column and do not appear in
  # every polygon. Therefore we subset zeros!
  #   if (all(is.element(c("layer", "polygon"), names(result)))) {
  #     result <- dplyr::filter(result, !(layer == n_layers & !is.element(species, union(biomasspools, get_sediment_digger()))))
  #   }

  # Sum up N for invert cohorts if invert cohorts are present!
  # NOTE: invert cohorts of size 10 are not considered!
  if (select_variable == "N" & any(final_agecl != 10 & final_agecl > 1)) {
    result <- result %>%
      dplyr::group_by(polygon, layer, species, time) %>%
      dplyr::summarise(atoutput = sum(atoutput))
  }

  return(result)
}
