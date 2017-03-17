#' Extracts the names of the epibenthic biomasspools from the initial conditions file.
#'
#' Use \code{fgs} \code{data.frame} as read in by \code{\link{load_fgs}}
#' to get the biomass pool information.
#'
#' @inheritParams load_fgs
#' @inheritParams load_init
#'
#' @export
#' @family load functions
#' @seealso \code{\link{load_fgs}}
#'
#' @return Character \code{vector} of epibenthic biomass pools.
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#'
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' init <- file.path(d, "INIT_VMPA_Jan2015.nc")
#'
#' load_bps(fgs, init)

load_bps <- function(fgs, init){
  if (utils::tail(unlist(strsplit(init, "\\.")), 1) != "nc") {
    stop(paste("The init argument", init, "does not end in .nc"))
  }

  init_read <- RNetCDF::open.nc(con = init)
  on.exit(RNetCDF::close.nc(init_read))

  fgs <- load_fgs(fgs = fgs)

  all_groups <- fgs$Name
  init_vars <- sapply(seq_len(RNetCDF::file.inq.nc(init_read)$nvars - 1),
                      function(x) RNetCDF::var.inq.nc(init_read, x)$name)

  search_string <- paste(all_groups, "N", sep = "_")
  search_string <- search_string[is.element(search_string, init_vars)]

  groups <- substr(x = search_string, start = 1, stop = nchar(search_string) - 2)

  bps_id <- vector()
  for (i in seq_along(search_string)) {
    bps_id[i] <- RNetCDF::var.inq.nc(ncfile = init_read, variable = search_string[i])$ndims
  }
  bps_id <- which(bps_id == 2)

  bps <- groups[bps_id]
  return(bps)
}
