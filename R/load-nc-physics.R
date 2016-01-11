#' Load Atlantis outputfiles (netcdf)
#'
#'
#' This function loads Atlantis outputfiles (netcdf) and converts them to a dataframe.
#' @param model_path Character string of the ATLANTIS folder.
#' @param filename Character string of the general ATLANTIS output file. Usually "outputNorthSea.nc".
#' @param physic_variables Character value spefifying which variables shall be loaded. Only one variable can be loaded at a time. Currently, only "salt", "NO3", "NH3", "Temp", "Oxygen", "Si", "Det_Si", "DON", "Chl_a", "Denitrifiction", "Nitrification" can be selected.
#' @param aggregate_layers Logical specifying if layers shall be aggregated or not.
#' @param load_init Logical specifying if an initial file shall be read in. Default is F wich means output files are loaded.
#' @param remove_bboxes Logical specifying if an boundary boxes  shall be excluded. Default is T.
#' @return Dataframe in long format with the following informations: variable, timestep, polygon, and value (= "atoutput").

#' @details This functions converts the ATLANTIS output to a dataframe which can be processed in R.
#' @keywords gen
#' @examples
#' load_atlantis_ncdf_physics(model_path = file.path("z:", "Atlantis", "ATLANTIS NSmodel base"), filename = "outputNorthSea.nc", physic_variables = c("salt", "Temp"))
#' @export

load_atlantis_ncdf_physics <- function(nc_out,
                                       physic_variables,
                                       aggregate_layers,
                                       remove_bboxes){
  if (is.null(physic_variables)) stop("No physical variables selected.")
  supported_variables <- get_physics()

  wrong_input <- physic_variables[which(!is.element(physic_variables, supported_variables))]

  if (length(wrong_input) >= 1) {
    stop(paste(wrong_input, "not part of", paste(supported_variables, collapse = ", ")))
  }

  # Load ATLANTIS output!
  at_out <- ncdf4::nc_open(filename = nc_out)

  # Extract number of timesteps, polygons and layers from netcdf
  n_timesteps <- at_out$dim[[1]]$len
  n_boxes     <- at_out$dim[[2]]$len
  n_layers    <- at_out$dim[[3]]$len

  if (n_timesteps == 1) stop("Timestep is one.")

  num_layers <- ncdf4::ncvar_get(nc = at_out, varid = "numlayers")[,1]
  # add sediment layer!
  num_layers <- num_layers + ifelse(num_layers == 0, 0, 1)

  # Create an array of layerids. Every entry in the array indicates if a layer is present (= 1)
  # or not (= 0). Boxes without layers (= islands) have only 0s as id! This is used lateron to remove
  # data from non-existent layers! By default output should be 0 in those layers. However, this approach is
  # much more robust as true zeros are kept!!! In addition all layers in boundary boxes are also set
  # to 0 if remove_bboxes is TRUE! This will speed up the code ALOT! In addition is helps to vectorise
  # the dataframe creation. Applying a boolean array to an array results in a vector!
  for (i in seq_along(num_layers)) {
    if (i == 1) layerid <- array(dim = c(n_layers, n_boxes))
    if (num_layers[i] == 0) {
      layerid[, i] <- 0
    } else {
      if (remove_bboxes & is.element((i - 1), get_bboxes())) {
        layerid[, i] <- 0
      } else {
        layerid[, i] <- c(rep(0, times = n_layers - num_layers[i]), rep(1, times = num_layers[i]))
      }
    }
  }

  # Perform ncdf extraction! This is the main time consuming step!
  physic_variables <- sort(physic_variables)
  physic_output <- lapply(physic_variables, ncdf4::ncvar_get, nc = at_out)

  # Create vectors for polygons and layers! Each vector has the length equal to one timestep!
  # All data from islands and non-existent layers is removed! Therefore the length of these
  # vectors is equal for each extracted variable!
  boxes <- 0:(n_boxes - 1)
  # Remove islands! and boundary boxes!
  island_ids <- num_layers == 0
  if (remove_bboxes) {
    boundary_ids <- is.element(boxes, get_bboxes())
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

  # Actual data extraction is performed!
  for (i in seq_along(physic_output)) {# for loop over physical variables
    if (i == 1) result <- list()
    for (j in 1:n_timesteps) {# loop over timesteps
      if (j == 1) values <- array(dim = c(length(layers), n_timesteps))
      values[, j] <- physic_output[[i]][,, j][which(layerid == 1)]
    }
    result[[i]] <- as.vector(values)
  }

  # Order of the data in value column = "atoutput".
  # 1. physic_variables --> rep with num_existing_layers * n_timesteps
  # 2. timestep         --> rep each with num_existing_layers then times physic_variables
  # 3. polygon          --> rep polygons times timesteps * physic_variables
  # 4. layer            --> rep layers times timesteps * physic_variables
  # The code is highly vectorized and therefore quite effective!
  result <- data.frame(variable = rep(physic_variables, each = length(layers) * n_timesteps),
                       polygon = rep(polygons, times = n_timesteps * length(physic_variables)),
                       layer = rep(layers, times = n_timesteps * length(physic_variables)),
                       time = rep(rep(0:(n_timesteps - 1), each = length(layers)), times = length(physic_variables)),
                       atoutput = do.call(c, result),
                       stringsAsFactors = F)

  # Remove min_pools if existent (well, there always are min pools... ;)).

  # Remove min_pools if existent (well, there always are min pools... ;)).
  min_pools <- is.element(result$atoutput, c(0, 1e-08, 1e-16))
  if (length(min_pools) > 0) {
    print_min_pools <- sum(min_pools)
    if (print_min_pools > 0) warning(paste0(round(print_min_pools/dim(result)[1] * 100), "% of entries are min-pools (0, 1e-08, 1e-16)"))
    result <- result[!min_pools, ]
  }

  if (aggregate_layers) {
    result <- result %>%
      dplyr::group_by(variable, polygon, time) %>%
      dplyr::summarise(atoutput = mean(atoutput))
  }

  return(result)
}






