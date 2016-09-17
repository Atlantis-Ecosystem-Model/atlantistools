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
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' load_init_weight(dir = dir, nc = "init_NorthSea.nc", fgs = "functionalGroups.csv")

load_init_weight <- function(dir = getwd(), nc, fgs) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  init <- RNetCDF::open.nc(con = convert_path(dir = dir, file = nc))
  on.exit(RNetCDF::close.nc(init))

  # Construct vector of variable names to search!
  species <- get_age_groups(dir = dir, fgs = fgs)
  numcohorts <- fgs_data$NumCohorts[is.element(fgs_data$Name, species)]
  cohorts <- lapply(numcohorts, seq, from = 1)
  search_clean <- unlist(Map(f = paste0, species, cohorts, USE.NAMES = FALSE))

  # Extract data from init-file remove duplicated values and zeros!
  extract_data <- function(tags, nc) {
    at_data <- lapply(tags, RNetCDF::var.get.nc, ncfile = nc)
    vapply(at_data, function(x) unique(x[x != 0]), numeric(1))
  }

  # Store in df
  df <- data.frame(species = rep(species, times = numcohorts),
                   agecl = unlist(cohorts),
                   rn = extract_data(paste0(search_clean, "_ResN"), init),
                   sn = extract_data(paste0(search_clean, "_StructN"), init))

  return(df)
}
