#' This function loads weight at age data (in mgN) from the initial conditions file.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param nc Character string giving the filename of netcdf file which
#' shall be read in. Usually "init[...].nc".
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following column names:
#'   species, agecl, rn and sn.
#'
#' @keywords gen
#' @author Alexander Keth

#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' load_init_weight(dir = d, nc = "init_vmpa_setas_25032013.nc", fgs = "SETasGroups.csv")

load_init_weight <- function(dir = getwd(), nc, fgs) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  at_out <- RNetCDF::open.nc(con = convert_path(dir = dir, file = nc))
  on.exit(RNetCDF::close.nc(at_out))

  # Construct vector of variable names to search!
  species <- get_age_groups(dir = dir, fgs = fgs)
  cohorts <- lapply(fgs_data$NumCohorts[is.element(fgs_data$Name, species)], seq, from = 1)
  search_clean <- Map(f = paste0, species, cohorts)
  search_clean <- unlist(lapply(search_clean, function(x) outer(X = x, Y = c("_ResN", "_StructN"), FUN = paste0)))

  # Extract data from init-file
  at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = at_out)

  sapply(at_data, dim)

}
