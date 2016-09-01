#' Load mortality information from outputSpecificPredMort.txt
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param specmort Character string of the outputSpecificPredMort.txt file. Usually
#' 'output[...]SpecificPredMort.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param version_flag The version of atlantis that created the output files.
#' 1 for bec_dev, 2 for trunk.
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' df <- load_spec_mort(dir = d,
#'    specmort = "outputSETASSpecificPredMort.txt")
#' head(df)
#' df <- load_spec_mort(dir = system.file("extdata", "setas-model-new-trunk", package = "atlantistools"),
#'                      specmort = "outputSETASSpecificPredMort.txt", version_flag = 2)
#' head(df)

#BJS 7/15/16 add version_flag and make compatible with trunk output
load_spec_mort <- function(dir = getwd(), specmort, version_flag = 1) {
  if (version_flag == 1) {
    mort <- load_txt(dir = dir, file = specmort)
    mort <- tidyr::separate_(mort, col = "code", into = c("prey", "agecl", "stock", "pred", "mort"), convert = TRUE)
    # check uniqueness of column notsure and mort
    if (any(sapply(mort[, c("stock", "mort")], function(x) length(unique(x))) != 1)) {
      stop("Multiple stocks present. This is not covered by the current version of atlantistools. Please contact the package development team.")
    }
  } else if (version_flag == 2) {
    mort <- load_txt(dir = dir, file = specmort, id_col = c("Time", "Group", "Cohort", "Stock"))
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






