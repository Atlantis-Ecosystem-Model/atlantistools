#' Extract parameters from the biological parameter file and transform them to a dataframe.
#'
#' @param group Character vector giving the functional Groups to extract.
#' @param parameter Character vector giving the parameters to extract.
#' @inheritParams preprocess
#' @return Dataframe with columns 'species' and as many columns as parameters.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' fgs <- "functionalGroups.csv"
#' group <- c("COD", "HER")
#'
#' prm_to_df_ages(dir, prm_biol, fgs, group, parameter = c("mum", "C"))
#' prm_to_df(dir, prm_biol, fgs, group, parameter = c("age_mat", "KWRR"))

dir <- "c:/backup_z/Atlantis_models/baserun/"
prm_biol <- "NorthSea_biol_fishing.prm"
group <- c("COD", "WHG")
parameter <- c("mum", "C")

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

#' @export
#' @rdname prm_to_df
prm_to_df_ages <- function(dir = getwd(), prm_biol, fgs, group, parameter) {
  # Extract data!
  prms <- lapply(parameter, set_single_prm, group = group)
  values <- lapply(prms, extract_prm_cohort, dir = dir, prm_biol = prm_biol)

  # Combine to df!
  nc <- sapply(values, function(x) sapply(x, length), USE.NAMES = FALSE)
  if (length(group) > 1) {
    nc <- nc[, 1]
  } else {
    nc <- nc[1]
  }

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



