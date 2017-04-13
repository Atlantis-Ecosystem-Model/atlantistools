#' Extract parameters from the biological parameter file and transform them to a dataframe.
#'
#' @inheritParams extract_prm
#' @inheritParams load_fgs
#' @param group Character vector giving the functional Groups to extract.
#' @param parameter Character vector giving the parameters to extract.
#' @return Dataframe with columns 'species' and as many columns as parameters.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' group <- c("FPS", "FVS")
#' parameter <- c("mum", "C")
#'
#' prm_to_df_ages(prm_biol, fgs, group, parameter)
#' prm_to_df(prm_biol, fgs, group, parameter)

prm_to_df <- function(prm_biol, fgs, group, parameter) {
  # Extract data!
  prms <- lapply(parameter, set_single_prm, group = group)
  prm_biol_new <- readLines(con = prm_biol, warn = FALSE)

  prm_t  <- do.call(rbind, prms)[,1]
  no_prm <- which(is.na(charmatch(prm_t, prm_biol_new)))
  if (sum(no_prm) > 1 && unlist(strsplit(prm_t[no_prm], '_'))[2] == 'AgeClassSize') {
    prms2  <- prms[-no_prm]
    values <- lapply(prms2, extract_prm, prm_biol = prm_biol)
    sps    <- which(load_fgs(fgs = fgs)$Code %in% group)
    extr   <- load_fgs(fgs = fgs)$NumAgeClassSize[sps]
    values[[length(values) + 1]] <- extr
    parameter                    <- c(parameter[-no_prm], parameter[no_prm])
  } else {
    values <- lapply(prms, extract_prm, prm_biol = prm_biol)
  }
  # Combine to df!
  df         <- as.data.frame(do.call(cbind, values))
  names(df)  <- tolower(parameter)
  df$species <- group
  df$species <- convert_factor(data_fgs = load_fgs(fgs = fgs), col = df$species)
  df <- dplyr::select_(df, .dots = c("species", sort(names(df)[-ncol(df)])))

  return(df)
}

#' @export
#' @rdname prm_to_df
prm_to_df_ages <- function(prm_biol, fgs, group, parameter) {
  # Extract data!
  prms <- lapply(parameter, set_single_prm, group = group)
  values <- lapply(prms, extract_prm_cohort, prm_biol = prm_biol)

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

  result$species <- convert_factor(data_fgs = load_fgs(fgs = fgs), col = result$species)

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
