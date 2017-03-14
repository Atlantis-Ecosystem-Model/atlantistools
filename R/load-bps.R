#' Extracts the names of the epibenthic biomasspools from the initial conditions file.
#'
#' Use \code{fgs} \code{data.frame} as read in by \code{\link{load_fgs}}
#' to get the biomass pool information.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param init Character string giving the filename of netcdf init-file which
#' shall be read in. Usually "init[...].nc". In case you are using
#' multiple folder for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#'
#' @export
#' @family load functions
#' @seealso \code{\link{load_fgs}}
#'
#' @return Character \code{vector} of epibenthic biomass pools.
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' load_bps(dir = d, fgs = "SETasGroupsDem_NoCep.csv", init = "INIT_VMPA_Jan2015.nc")

load_bps <- function(dir = getwd(), fgs, init){
  file_ending(filename = init)
  if (!is.null(dir)) {
    init <- file.path(dir, init)
  }

  init <- RNetCDF::open.nc(con = init)
  on.exit(RNetCDF::close.nc(init))

  fgs <- load_fgs(dir = dir, fgs = fgs)

  all_groups <- fgs$Name
  init_vars <- sapply(seq_len(RNetCDF::file.inq.nc(init)$nvars - 1),
                      function(x) RNetCDF::var.inq.nc(init, x)$name)

  search_string <- paste(all_groups, "N", sep = "_")
  search_string <- search_string[is.element(search_string, init_vars)]

  groups <- substr(x = search_string, start = 1, stop = nchar(search_string) - 2)

  bps_id <- vector()
  for (i in seq_along(search_string)) {
    bps_id[i] <- RNetCDF::var.inq.nc(ncfile = init, variable = search_string[i])$ndims
  }
  bps_id <- which(bps_id == 2)

  bps <- groups[bps_id]
  return(bps)
}


