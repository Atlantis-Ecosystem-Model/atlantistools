#' Extract values for Atlantis parameters from the biological parameter file.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param variables Character string giving the flag to search for. This should be
#' a combination of the parameter name and the group-Code.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.

#' @export
extract_prm <- function(dir = getwd(), prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- convert_path(dir = dir, file = prm_biol)
  prm_biol_new <- readLines(con = prm_biol_new)

  pos <- vapply(variables, scan_prm, FUN.VALUE = integer(1), chars = prm_biol_new)
  result <- prm_biol_new[pos]
  result <- vapply(result, str_split_twice, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  return(result)
}


#' @export
#' @rdname extract_prm
# Extract value for a specific cohort parameter from a Vector of character strings.
extract_prm_cohort <- function(dir = getwd(), prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- convert_path(dir = dir, file = prm_biol)
  prm_biol_new <- readLines(con = prm_biol_new)

  slice <- function(prm, variable) {
    pos <- scan_prm(chars = prm, variable = variable)
    pos <- pos + 1
    while (substr(prm[pos], 1, 1) == "#") pos <- pos + 1

    # Keep all numeric values
    value <- str_split_twice(char = prm[pos], min_only = FALSE)
    return(value)
  }

  values <- lapply(variables, slice, prm = prm_biol_new)
  # rbind to matrix in case all groups have the same number of cohorts!
  if (length(unique(sapply(values, length))) == 1) {
    values <- do.call(rbind, values)
    rownames(values) <- variables
  } else {
    names(values) <- variables
  }
  return(values)
}


# dir <- "c:/backup_z/Atlantis_models/baserun/"
# prm_biol <- "NorthSea_biol_fishing.prm"
# group <- c("COD", "WHG")
# parameter <- c("mum", "C")

prm_to_df <- function(dir = getwd(), prm_biol, fgs, group, parameter) {
  # Extract data!
  prms <- lapply(parameter, set_single_prm, group = group)
  values <- lapply(prms, extract_prm, dir = dir, prm_biol = prm_biol)

  # Combine to df!
  df <- as.data.frame(do.call(cbind, values))
  names(df) <- tolower(parameter)
  df$species <- group
  df$species <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = df$species)
  df <- dplyr::select_(df, .dots = c("species", sort(names(df)[-ncol(df)])))

  return(df)
}


prm_to_df_ages <- function(dir = getwd(), prm_biol, fgs, group, parameter) {
  # Extract data!
  prms <- lapply(parameter, set_single_prm, group = group)
  values <- lapply(prms, extract_prm_cohort, dir = dir, prm_biol = prm_biol)

  # Combine to df!
  nc <- sapply(values, function(x) sapply(x, length), USE.NAMES = FALSE)[, 1]
  df <- data.frame(values = unlist(values))
  df$species <- rep(unlist(Map(rep, group, nc)), times = length(parameter))
  df$agecl <- rep(unlist(sapply(nc, seq, from = 1)), times = length(parameter))
  df$prm <- rep(tolower(parameter), each = sum(nc))

  result <- tidyr::spread_(data = df, key_col = "prm", value_col = "values")

  result$species <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = result$species)

  return(result)
}


# helper function to create an Atlantis parameter Flag given a vector of groups and
# a vector of parameters. You can pass different numbers of groups and parameters.
# In addition
set_single_prm <- function(group, parameter) {
  if (parameter %in% c("AgeClassSize", "age_mat")) {
    paste(group, parameter, sep = "_")
  } else {
    paste(parameter, group, sep = "_")
  }
}



