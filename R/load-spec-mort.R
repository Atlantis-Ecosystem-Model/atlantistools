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
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' df <- load_spec_mort(dir = d,
#'    specmort = "outputSETASSpecificPredMort.txt")
#' head(df)

load_spec_mort <- function(dir = getwd(), specmort) {
  mort <- load_txt(dir = dir, file = specmort)
  mort <- tidyr::separate_(mort, col = "code", into = c("prey", "agecl", "notsure", "pred", "mort"), convert = TRUE)
  mort$agecl <- mort$agecl + 1

  # check uniqueness of column notsure and mort
  if (any(sapply(mort[, c("notsure", "mort")], function(x) length(unique(x))) != 1)) {
    stop("Insufficient grouping columns!")
  }

  # Remove unnecessary columns
  mort <- mort[, !is.element(names(mort), c("notsure", "mort"))]

  # First time step appears twice and only has 0s as entry!
  mort <- mort[mort$time != 0, ]

  # Check number of empty entries per predator!
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






