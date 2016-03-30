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
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' df <- load_spec_mort(dir = d,
#'    specmort = "outputSETASSpecificPredMort.txt",
#'    prm_biol = "VMPA_setas_biol_fishing_New.prm")
#' head(df)

load_spec_mort <- function(dir = getwd(), specmort, prm_biol) {
  mort <- load_txt(dir = dir, file = specmort)
  mort <- tidyr::separate_(mort, col = "code", into = c("pred", "agecl", "notsure", "prey", "mort"), convert = TRUE)
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

  # Combine ageclasses to stanzas based on maturity!
  # Get agebased predators. We could extract the data from the functional
  # groups file. However, doing so would add an additional parameter to the function call...
  # Stanzas are introcued during the file loading procedure to reduce the final file size on the HDD.
  preds <- mort %>%
    dplyr::group_by_("pred") %>%
    dplyr::summarise_(count = ~length(unique(agecl))) %>%
    dplyr::filter(count == 10)
  preds <- preds$pred

  age_mat <- convert_path(dir = dir, file = prm_biol)
  age_mat <- readLines(con = age_mat)

  age_mat <- data.frame(pred = preds,
                        age_mat = vapply(paste0(preds, "_age_mat"), extract_prm, chars = age_mat, FUN.VALUE = numeric(1)),
                        stringsAsFactors = FALSE)

  # Finally set stanzas!
  mort <- dplyr::left_join(mort, age_mat)
  mort$stanza <- ifelse(mort$agecl < mort$age_mat, "juvenile", "adult")
  mort$stanza[is.na(mort$age_mat)] <- "none"

  # Mean per stanza!
  mort_stanza <- agg_data(data = mort, groups = c("time", "pred", "prey", "stanza"), fun = mean)

  return(mort_stanza)
}


# ggplot2::ggplot(subset(mort, pred == "COD" & prey == "COD"), ggplot2::aes(x = factor(time), y = atoutput, fill = stanza)) +
#   ggplot2::geom_boxplot(position = "dodge") +
#   # ggplot2::geom_point()
#   ggplot2::facet_wrap(~prey, scale = "free")
#
# dir <- "z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/"
# specmort = "outputNorthSeaSpecificPredMort.txt"
# prm_biol = "NorthSea_biol_fishing.prm"






