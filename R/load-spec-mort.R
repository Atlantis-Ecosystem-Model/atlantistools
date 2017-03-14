#' Load mortality information from outputSpecificPredMort.txt
#'
#' @inheritParams load_nc
#' @inheritParams load_fgs
#' @inheritParams load_dietmatrix
#' @param specmort Character string giving the connection of the specific mortality file.
#' The filename usually contains \code{SpecificPredMort} and ends in \code{.txt}".
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
#' fgs <- file.path(d, "SETasGroups.csv")
#'
#' df <- load_spec_mort(specmort, prm_run, fgs, version_flag = 1)
#' head(df)
#'
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#'
#' df <- load_spec_mort(specmort, prm_run, fgs)
#' head(df)

#BJS 7/15/16 add version_flag and make compatible with trunk output
load_spec_mort <- function(specmort, prm_run, fgs, convert_names = FALSE, version_flag = 2) {
  if (version_flag == 1) {
    mort <- load_txt(file = specmort)
    mort <- tidyr::separate_(mort, col = "code", into = c("prey", "agecl", "stock", "pred", "mort"), convert = TRUE)
    # check uniqueness of column notsure and mort
    if (any(sapply(mort[, c("stock", "mort")], function(x) length(unique(x))) != 1)) {
      stop("Multiple stocks present. This is not covered by the current version of atlantistools. Please contact the package development team.")
    }
  } else if (version_flag == 2) {
    mort <- load_txt(file = specmort, id_col = c("Time", "Group", "Cohort", "Stock"))
    mort <- dplyr::rename_(mort, prey = ~group, agecl = ~cohort, pred = ~code)
    if (any(sapply(mort[, "stock"], function(x) length(unique(x))) != 1)) {
      stop("Multiple stocks present. This is not covered by the current version of atlantistools. Please contact the package development team.")
    }
  }
  mort$agecl <- mort$agecl + 1

  # Remove unnecessary columns
  mort <- mort[, !is.element(names(mort), c("stock", "mort"))]

  # First time step appears twice and only has 0s as entry!
  # ***This will cause a potentially unnoticed bug when this issue in the output file gets fixed
  mort <- mort[mort$time != 0, ]

  # Check number of empty entries per predator!
  # BJS: is this needed? it doesnt appear to be used anywhere
  nr_prey <- length(unique(mort$prey))
  count_zero <- mort %>%
    dplyr::group_by_(~time, ~pred, ~agecl) %>%
    dplyr::summarise_(count_zero = ~sum(atoutput == 0) / nr_prey) %>%
    dplyr::filter(count_zero == 1)

  # Remove zeros
  mort <- mort[mort$atoutput != 0, ]

  # Convert species codes to longnames!
  if (convert_names) {
    mort <- dplyr::mutate_at(mort, .cols = c("pred", "prey"), .funs = convert_factor, data_fgs = load_fgs(fgs = fgs))
  }

  # Convert time
  mort$time <- convert_time(prm_run = prm_run, col = mort$time)

  return(mort)
}


# ggplot2::ggplot(subset(mort, pred == "COD" & prey == "COD"), ggplot2::aes(x = factor(time), y = atoutput, fill = stanza)) +
#   ggplot2::geom_boxplot(position = "dodge") +
#   # ggplot2::geom_point()
#   ggplot2::facet_wrap(~prey, scale = "free")
#
# dir <- "z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/"
# specmort = "outputNorthSeaSpecificPredMort.txt"
# prm_biol = "NorthSea_biol_fishing.prm"






