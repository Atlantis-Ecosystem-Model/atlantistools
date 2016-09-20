#' This function loads weight at age data (in mgN) from the initial conditions file.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param init Character string giving the filename of the initial conditions netcdf file.
#' Usually "init[...].nc".
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following column names:
#'   species, agecl, rn and sn.
#'
#' @author Alexander Keth

#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' load_init_weight(dir = dir, init = "init_simple_NorthSea.nc", fgs = "functionalGroups.csv")

load_init_weight <- function(dir = getwd(), init, fgs) {
  init <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init))

  # Construct vector of variable names to search!
  search_clean <- get_tags(dir = dir, fgs = fgs)

  # Extract data from init-file remove duplicated values and zeros!
  extract_data <- function(tags, nc) {
    at_data <- lapply(tags, RNetCDF::var.get.nc, ncfile = nc)
    at_data <- lapply(at_data, function(x) unique(x[!x %in% c(0, 1e-08, 1e-16)]))
    dims <- vapply(at_data, length, integer(1))
    if (all(dims == 1)) {
      unlist(at_data)
    } else {
      warning("Multiple weight at age values in initial file. Only the first value is used per group/age.")
      vapply(at_data, function(x) x[1], numeric(1))
    }
  }

  # Store in df
  df <- data.frame(species = rep(search_clean$species, times = sapply(search_clean$cohorts, length)),
                   agecl = unlist(search_clean$cohorts),
                   rn = extract_data(paste0(search_clean$tags, "_ResN"), init),
                   sn = extract_data(paste0(search_clean$tags, "_StructN"), init), stringsAsFactors = FALSE)

  return(df)
}

#' @export
#' @rdname load_init_weight
load_init_num <- function(dir = getwd(), init, fgs) {
  init <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init))

  # Construct vector of variable names to search!
  search_clean <- get_tags(dir = dir, fgs = fgs)

  # Extract data from init-file remove duplicated values and zeros!
  extract_data <- function(tags, nc) {
    at_data <- lapply(tags, RNetCDF::var.get.nc, ncfile = nc)
    at_data <- lapply(at_data, function(x) x[1, ]) # only use 1st layer
    at_data <- lapply(at_data, function(x) data.frame(atoutput = x, polygon = 0:(length(x) - 1), stringsAsFactors = FALSE))
    at_data <- lapply(at_data, function(x) x[!is.element(x$atoutput, c(0, 1e-08, 1e-16)), ])
    return(at_data)
  }

  # Store in df
  wuwu <- extract_data(paste0(search_clean$tags, "_Nums"), nc = init)
  for (i in seq_along(search_clean[[2]])) {
    for (j in 1:length(search_clean[[3]][[i]])) {
      if (i == 1 & j == 1) k <- 1
      wuwu[[k]]$species <- search_clean[[2]][i]
      wuwu[[k]]$agecl <- search_clean[[3]][[i]][j]
      k <- k + 1
    }
  }
  df <- do.call(rbind, wuwu)

  return(df)
}

#' @export
#' @rdname load_init_weight
load_init_n <- function(dir = getwd(), init, fgs) {
  init <- RNetCDF::open.nc(con = convert_path(dir = dir, file = init))
  on.exit(RNetCDF::close.nc(init))

  # Construct vector of variable names to search!
  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  search_clean <- paste0(groups_rest, "_N")
  at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = init)

  sed_id <- sapply(at_data, function(x) length(dim(x))) == 1

  at_data <- at_data[!sed_id]
  at_data <- lapply(at_data, function(x) x[1, ]) # only use 1st layer
  at_data <- lapply(at_data, function(x) data.frame(atoutput = x, polygon = 0:(length(x) - 1), stringsAsFactors = FALSE))
  at_data <- lapply(at_data, function(x) x[!is.element(x$atoutput, c(0, 1e-08, 1e-16)), ])

  species <- groups_rest[!sed_id]
  # Store in df
  for (i in seq_along(at_data)) {
    at_data[[i]]$species <- species[i]
  }
  df <- do.call(rbind, at_data)

  return(df)
}

get_tags <- function(dir = getwd(), fgs) {
  # Construct vector of variable names to search!
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  species <- get_age_groups(dir = dir, fgs = fgs)
  numcohorts <- fgs_data$NumCohorts[is.element(fgs_data$Name, species)]
  cohorts <- lapply(numcohorts, seq, from = 1)
  search_clean <- unlist(Map(f = paste0, species, cohorts, USE.NAMES = FALSE))
  return(list(tags = search_clean, species = species, cohorts = cohorts))
}


