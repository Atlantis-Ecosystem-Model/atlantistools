#' This function loads weight at age data (in mgN) from the initial conditions file.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param init Character string giving the filename of the initial conditions netcdf file.
#' Usually "init[...].nc".
#' @param vars Vector of character strings giving the variables to extract from the
#' netcdf file.
#'
#' @family load functions
#' @export
#' @return A list of dataframes with columns atoutput, polygon and layer (if present).
#'
#' @author Alexander Keth

#' @examples
#' dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' load_init(dir = dir, init = "INIT_VMPA_Jan2015.nc", vars = "Planktiv_S_Fish1_Nums")
#' load_init(dir = dir, init = "INIT_VMPA_Jan2015.nc", vars = c("Planktiv_S_Fish2_ResN",
#'                                                              "Planktiv_S_Fish3_ResN"))
#' load_init(dir = dir, init = "INIT_VMPA_Jan2015.nc", vars = "Megazoobenthos_N")

load_init <- function(dir = getwd(), init, vars) {
  # dummy
  read_nc <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(read_nc))

  # Extract ncdf dimensions!
  n_timesteps <- RNetCDF::dim.inq.nc(read_nc, 0)$length
  if (n_timesteps != 1) stop("More than 1 timestep! init was not an initial conditions file.")
  n_boxes     <- RNetCDF::dim.inq.nc(read_nc, 1)$length
  n_layers    <- RNetCDF::dim.inq.nc(read_nc, 2)$length
  num_layers <- get_layers(dir = dir, init = init)
  layerid <- get_layerid(num_layers = num_layers, max_layer = n_layers, n_boxes = n_boxes)
  var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(read_nc)$nvars - 1),
                           function(x) RNetCDF::var.inq.nc(read_nc, x)$name)

  wrong_var <- vars[!vars %in% var_names_ncdf]
  if (length(wrong_var) >= 1) stop(paste("Variable", paste(wrong_var, collapse = " "), "not found in init file."))

  at_data <- lapply(vars, RNetCDF::var.get.nc, ncfile = read_nc)

  # Box and layer!
  convert2d <- function(mat, layerid, n_boxes) {
    if (!(is.matrix(mat) & length(dim(mat)) == 2)) {
      stop("Wrong data format. Variable is not stored as 2d data in initial file.")
    }
    data.frame(atoutput = as.vector(mat),
               polygon = rep(0:(n_boxes - 1), each = length(layerid) / n_boxes),
               layer = layerid, stringsAsFactors = FALSE)
  }

  # Only Box data!
  convert1d <- function(vec, n_boxes) {
    if (!(is.array(vec) & length(vec) == n_boxes)) {
      stop("Wrong data format. Variable is not stored as 1d vector in initial file.")
    }
    data.frame(atoutput = as.vector(vec),
               polygon = 0:(n_boxes - 1), stringsAsFactors = FALSE)
  }

  at_dim <- vapply(at_data, function(x) length(dim(x)), integer(1))

  # Check cases and apply formulas!
  if (all(at_dim == 2)) df_list <- lapply(at_data, convert2d, layerid, n_boxes)
  if (all(at_dim == 1)) df_list <- lapply(at_data, convert1d, n_boxes)
  if (length(unique(at_dim)) > 1) stop("Vars are stored in different dimensions. Please, either pick only 2d or 1d data.")

  # Data extracted for every variable?
  if (length(vars) != length(df_list)) stop("Starnge ncdf extraction. Please contact package development Team.")

  return(df_list)
}

get_layers <- function(dir = getwd(), init) {
  read_nc <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(read_nc))

  num_layers <- RNetCDF::var.get.nc(ncfile = read_nc, variable = "numlayers")
  if (length(dim(num_layers)) == 2) {
    if (all(apply(num_layers, MARGIN = 1, FUN = function(x) length(unique)) == 1)) {
      num_layers <- num_layers[, 1]
    } else {
      stop("Different numbers of layers per Box. This nc-structure is not supported.")
    }
  }

  return(num_layers)
}

# Utility function used to extract the layerids from the number of layers per box.
# Layer ids are given in 0:num_layers-1 with 0 being the layer closest to the sediment.
# The largest id per box is the surface layer. Non existing layers (per box) are NA.
# The sediment layer id is the maximum number of layers. E.g. in case there are 5 water
# column layers the sediment layer has the id 6.
get_layerid <- function(num_layers, max_layer, n_boxes) {
  wc_id <- lapply(num_layers, function(x) rep(1, times = x))
  wc_id <- lapply(wc_id, function(x) rev(cumsum(x) - 1)) # ids are in reverse order in the nc file
  wc_fill <- lapply(num_layers, function(x) rep(NA, times = max_layer - x - 1))
  wc <- Map(f = c, wc_id, wc_fill)
  if (length(unique(sapply(wc, length))) != 1) stop("Layers inconsistent. Contact package development Team.")
  wc <- lapply(wc, function(x) c(x, max_layer - 1)) # add sediment layer
  unlist(wc)
}

# Remove min pools (0 and almost 0) from a datframe.
remove_min_pools <- function(df, col = "atoutput", min_pools = c(0, 1e-08, 1e-16)) {
  expr <- lazyeval::interp(quote(!(x %in% y)), x = as.name(col), y = min_pools)
  df %>% dplyr::filter_(expr)
}

# Remove boundary boxes from a dataframe. bboxes is the vector of box ids (starting with 0)
remove_bboxes <- function(df, bboxes) {
  if (!any(names(df) == "polygon")) stop("No column polygon in df. Cannot remove boundary boxes.")
  df %>% dplyr::filter_(~!(polygon %in% bboxes))
}

