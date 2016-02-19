#' Load Atlantis outputfiles (netcdf)
#'
#'
#' This function loads Atlantis outputfiles (netcdf) and converts them to a dataframe.
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param nc Character string giving the filename of netcdf file which
#' shall be read in. Usually "output[...].nc". Currently the general-
#' production- and catch.nc files can be loaded in. In case you are using
#' multiple folder for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param select_physics Character vector of physical variables which shall be read in.
#' Names have to match the ones used in the ncdf file.
#' @param aggregate_layers Logical indicating if values for layers should be
#' aggregated (\code{TRUE}) or not (\code{FALSE}).
#' @param bboxes Integer vector giving the box-id of the boundary boxes.
#' @param warn_zeros Logical indicating if check for actual zeros in the
#' data shall be printed or not.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   variable, time, polygon, layer, and atoutput (i.e., variable).


#' @details This functions converts the ATLANTIS output to a dataframe which can be processed in R.
#' @keywords gen
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' test <- load_nc_physics(dir = d, nc = "outputSETAS.nc",
#'   select_physics = c("salt", "NO3", "volume"),
#'   aggregate_layers = FALSE,
#'   bboxes = get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm")))
#' str(test)
#' @export

load_nc_physics <- function(dir = getwd(),
                            nc,
                            select_physics,
                            aggregate_layers,
                            bboxes,
                            warn_zeros = FALSE){
  if (is.null(select_physics)) stop("No physical variables selected.")
  supported_variables <- c("salt", "NO3", "NH3", "Temp", "Oxygen", "Si", "Det_Si", "DON", "Chl_a",
                           "Denitrifiction", "Nitrification", "eflux", "vflux", "volume", "Light", "dz",
                           "salinity", "temperature", "exchange", "dest_b", "dest_k")

  wrong_input <- select_physics[which(!is.element(select_physics, supported_variables))]

  if (length(wrong_input) >= 1) {
    stop(paste(wrong_input, "not part of", paste(supported_variables, collapse = ", ")))
  }

  # Check input of the nc file
  if (tail(strsplit(nc, "\\.")[[1]], 1) != "nc") {
    stop("The argument for nc,", nc, "does not end in nc")
  }
  if (!is.null(dir)) nc <- file.path(dir, nc)

  # Load ATLANTIS output!
  at_out <- RNetCDF::open.nc(con = nc)
  on.exit(RNetCDF::close.nc(at_out))

  # Extract number of timesteps, polygons and layers from netcdf
  n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
  n_boxes     <- RNetCDF::dim.inq.nc(at_out, 1)$length
  n_layers    <- RNetCDF::dim.inq.nc(at_out, 2)$length

  if (n_timesteps == 1) stop("Timestep is one.")

  num_layers <- RNetCDF::var.get.nc(ncfile = at_out, variable = "numlayers")[,1]
  # add sediment layer!
  num_layers <- num_layers + ifelse(num_layers == 0, 0, 1)

  # Create an array of layerids. Every entry in the array indicates if a layer is present (= 1)
  # or not (= 0). Boxes without layers (= islands) have only 0s as id! This is used lateron to remove
  # data from non-existent layers! By default output should be 0 in those layers. However, this approach is
  # much more robust as true zeros are kept!!! In addition all layers in boundary boxes are also set
  # to 0! This will speed up the code ALOT! In addition is helps to vectorise
  # the dataframe creation. Applying a boolean array to an array results in a vector!
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

  # Perform ncdf extraction! This is the main time consuming step!
  select_physics <- sort(select_physics)
  physic_output <- lapply(select_physics, RNetCDF::var.get.nc, ncfile = at_out)

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
  # 1. select_physics   --> rep with num_existing_layers * n_timesteps
  # 2. timestep         --> rep each with num_existing_layers then times select_physics
  # 3. polygon          --> rep polygons times timesteps * select_physics
  # 4. layer            --> rep layers times timesteps * select_physics
  # The code is highly vectorized and therefore quite effective!
  result <- data.frame(variable = rep(select_physics, each = length(layers) * n_timesteps),
                       polygon = rep(polygons, times = n_timesteps * length(select_physics)),
                       layer = rep(layers, times = n_timesteps * length(select_physics)),
                       time = rep(rep(0:(n_timesteps - 1), each = length(layers)), times = length(select_physics)),
                       atoutput = do.call(c, result),
                       stringsAsFactors = F)

  # Remove min_pools if existent (well, there always are min pools... ;)).
  min_pools <- is.element(result$atoutput, c(0, 1e-08, 1e-16))
  if (length(min_pools) > 0) {
    print_min_pools <- sum(min_pools)
    if (print_min_pools > 0 & warn_zeros){
      warning(paste0(round(print_min_pools/dim(result)[1] * 100), "% of entries are min-pools (0, 1e-08, 1e-16)"))
    }
    result <- result[!min_pools, ]
  }

  if (aggregate_layers) {
    result <- result %>%
      dplyr::group_by_("variable", "polygon", "time") %>%
      dplyr::summarise_(atoutput = ~mean(atoutput))
  }

  return(result)
}






