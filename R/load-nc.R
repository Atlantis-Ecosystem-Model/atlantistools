#' Load Atlantis outputfiles (netcdf)
#'
#' This function loads Atlantis outputfiles (any netcdf file) and
#' converts them to a \code{data.frame}.
#'
#' @inheritParams load_fgs
#' @param nc Character string giving the connection of the netcdf file to read in.
#' The filename usually contains \code{output} and ends in \code{.nc}".
#' @param bps Vector of character strings giving the complete list of epibenthic
#' functional groups (Only present in the sediment layer). The names have to match
#' the column 'Name' in the 'functionalGroups.csv' file. Can be created with
#' \code{load_bps}.#'
#' @param select_groups Character vector of funtional groups which shall be read in.
#' Names have to match the ones used in the netcdf file. Check column "Name" in
#' "functionalGroups.csv" for clarification.
#' @param select_variable Character value specifying which variable to load.
#' Only one variable of the options available (i.e., \code{c(
#' "N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing")
#' }) can be loaded at a time.
#' @param prm_run Character string giving the connection of the run parameterfile.
#' The filename usually contains \code{run_fishing} and ends in \code{.prm}".
#' @param bboxes Integer vector giving the box-id of the boundary boxes.
#' Can be created with \code{get_boundary}.
#' @param check_acronyms Logical testing if functional-groups in
#' select_groups are inactive in the current model run. They will be omitted
#' in the output.
#' @param warn_zeros Logical indicating if check for actual zeros in the
#' data shall be printed or not. Default is \code{FALSE}.
#' @param report Logical indicating if progress bars shall be printed (\code{TRUE}) or
#' not (\code{FALSE}). Default is \code{TRUE}.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following column names:
#'   species, timestep, polygon, agecl, and atoutput (i.e., variable).
#'
#' @keywords gen
#' @author Alexander Keth
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#'
#' nc <- file.path(d, "outputSETAS.nc")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' bps <- load_bps(init = file.path(d, "INIT_VMPA_Jan2015.nc"), fgs = fgs)
#' bboxes <- get_boundary(boxinfo = load_box(bgm = file.path(d, "VMPA_setas.bgm")))
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
#'
#' test <- load_nc(nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
#'   select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
#'   select_variable = "ResN")
#'
#' test <- load_nc(nc = nc, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes,
#'   select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
#'   select_variable = "Nums")

load_nc <- function(nc, fgs, bps, select_groups,
                    select_variable, prm_run, bboxes, check_acronyms = TRUE,
                    warn_zeros = FALSE, report = TRUE) {
  # NOTE: The extraction procedure may look a bit complex... A different approach would be to
  # create a dataframe for each variable (e.g. GroupAge_Nums) and combine all dataframes
  # at the end. However, this requires alot more storage and the code wouldn't be highly
  # vectorised (which it is at the moment...)!
  supported_variables <- c("N", "Nums", "ResN", "StructN", "Eat", "Growth", "Prodn", "Grazing", "Catch")
  if (length(select_groups) == 0) stop("No functional groups selected.")
  if (length(select_variable) == 0) stop("No variable selected.")
  if (length(select_variable) > 1) stop("Only one variable allowed per function call.")
  if (any(!is.element(select_variable, supported_variables))) {
    stop(paste("Only", paste(supported_variables, collapse = ", "), "can be selected as 'select_variable'"))
  }

  fgs <- load_fgs(fgs = fgs)

  # Check input structure!
  if (check_acronyms) {
    active_groups <- fgs[fgs$IsTurnedOn == 1, "Name"]
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
  # UPDATE: Different numbers of cohorts per group are supported.
  # In order to make the creation of variables as robust as possible we introduce differtent combinations
  # of groups, variable and cohort! Only combinations present in the ncdf are used lateron! This makes the
  # code both robust and fast!
  # Loop over select_groups to use the same ordering! This is essential otherwise species names
  # are not assigned correctly lateron!
  cohorts <- 1:max(fgs$NumCohorts)
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

  at_data <- list()
  # Initialise progress par!
  # if (report) pb <- txtProgressBar(min = 0, max = length(search_clean), style = 3)
  if (report) pb <- dplyr::progress_estimated(length(search_clean))
    for (i in seq_along(search_clean)) {
      at_data[[i]] <- RNetCDF::var.get.nc(ncfile = at_out, variable = search_clean[i])
      # update progress bar
      # if (report) setTxtProgressBar(pb, i)
      if (report) pb$tick()$print()
    }
  # if (report) close(pb)
  if (report) pb$stop()

  # at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = at_out)

  # Get final species and number of ageclasses per species
  final_species <- select_groups[sapply(lapply(select_groups, grepl, x = search_clean), any)]
  id <- sapply(final_species, function(x) which(x == fgs$Name))
  final_agecl <- fgs$NumCohorts[id] * fgs$NumGeneTypes[id]

  # This may allow init files to be loaded as well! Unfortunately "num_layers" is missing in
  # the init file. Therefore we also load in the general file to extract the layers!
  # nc = init-file, init = nc-file (very confusing....)
  # if (length(init) >= 1 & is.character(init)) {
  #   init <- convert_path(dir = dir, file = init)
  #   at_out <- RNetCDF::open.nc(con = init)
  # }
  num_layers <- RNetCDF::var.get.nc(ncfile = at_out, variable = "numlayers")
  if (length(dim(num_layers)) == 2) {
    if (all(apply(num_layers, MARGIN = 1, FUN = function(x) length(unique)) == 1)) {
      num_layers <- num_layers[, 1]
    } else {
      stop("Different numbers of layers per Box. This nc-structure is not supported.")
    }
  }
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
      # Note this only works if age-structured invertebrates have 2 ageclasses!
      int_fa[int_fa != 2] <- 1
    }
    for (i in seq_along(at_data3d)) {# for loop over all variables!
      if (i == 1) result3d <- list()
      for (j in 1:n_timesteps) {# loop over timesteps
        if (j == 1) values <- array(dim = c(length(layers), n_timesteps))
        values[, j] <- at_data3d[[i]][,, j][layerid == 1]
      }
      result3d[[i]] <- as.vector(values)
    }
    result3d <- data.frame(species = unlist(lapply(X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = F, USE.NAMES = F),
                                                   FUN = rep, each = length(layers) * n_timesteps)),
                           agecl = unlist(lapply(X = lapply(X = int_fa, FUN = seq, from = 1, by = 1),
                                                 FUN = rep, each = length(layers) * n_timesteps)),
                           polygon = unlist(lapply(X = n_timesteps * int_fa, FUN = rep, x = polygons)),
                           layer = unlist(lapply(X = n_timesteps * int_fa, FUN = rep, x = layers)),
                           time = unlist(lapply(X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1), each = length(layers)))),
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
    result2d <- data.frame(species = unlist(lapply(X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = F, USE.NAMES = F),
                                                   FUN = rep, each = length(boxes) * n_timesteps)),
                           agecl = unlist(lapply(X = lapply(X = int_fa, FUN = seq, from = 1, by = 1),
                                                 FUN = rep, each = length(boxes) * n_timesteps)),
                           polygon = unlist(lapply(X = n_timesteps * int_fa, FUN = rep, x = boxes)),
                           time = unlist(lapply(X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1), each = length(boxes)))),
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
    if (print_min_pools > 0 & warn_zeros) {
      warning(paste0(round(print_min_pools/dim(result)[1] * 100), "% of ", select_variable, " are true min-pools (0, 1e-08, 1e-16)"))
    }
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
  # NOTE: only invert cohorts of size 2 are considered!
  if (select_variable == "N" & any(final_agecl == 2)) {
    result <- result %>%
      dplyr::group_by_("species", "polygon", "layer", "time") %>%
      dplyr::summarise_(atoutput = ~sum(atoutput)) %>%
      dplyr::ungroup()
  }

  # convert names to longnames
  result$species <- convert_factor(data_fgs = fgs, col = result$species)

  # Convert timestep to time in years!
  result$time <- convert_time(prm_run = prm_run, col = result$time)

  return(result)
}

# Add some debugging
# dir <- "z:/R_codes/Heidi/GABout_v1_converwrong/"
# bgm <- "GAB_xy.bgm"
# nc <- "GAB.nc"
# fgs <- "GAB_Groups.csv"
# init <- "inGAB.nc"
# bps <- load_bps(dir, fgs, init)
# groups <- get_groups(dir, fgs)
# groups_age <- get_age_groups(dir, fgs)
# groups_age <- c(groups_age, "Aquacult_Tuna")
# select_groups <- groups[!groups %in% groups_age]
# select_variable <- "Nums"
# prm_run <- "GAB_run.prm"
# bboxes <- get_boundary(boxinfo = load_box(dir, bgm))
# check_acronyms <- TRUE
# warn_zeros <- FALSE
# report <- TRUE
#
# test <- load_nc(dir, nc, fgs, bps, groups_age, select_variable, prm_run, bboxes)

# dir <- "z:/R_codes/Thiebaut/"
# bgm <- "SEAP_extended_shelf.bgm"
# nc <- "CEP_outputPROD.nc"
# fgs <- "CEP_Groups_onespawn.csv"
# init <- "CEP_ic.nc"
# bps <- load_bps(dir, fgs, init)
# groups <- get_groups(dir, fgs)
# groups_age <- get_age_groups(dir, fgs)
# select_groups <- groups_age
# select_variable <- "Eat"
# prm_run <- "CEP_run.prm"
# bboxes <- get_boundary(boxinfo = load_box(dir, bgm))
# check_acronyms <- TRUE
# warn_zeros <- FALSE
# report <- TRUE
#
# test <- load_nc(dir, nc, fgs, bps, groups_age, select_variable, prm_run, bboxes)


