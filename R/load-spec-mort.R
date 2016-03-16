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
#'    specmort = "outputSETASSpecificPredMort.txt")
#' head(df)

load_spec_mort <- function(dir = getwd(), specmort) {
  mort <- load_txt(dir = dir, file = specmort)
  mort <- tidyr::separate_(mort, col = "code", into = c("pred", "agecl", "notsure", "prey", "mort"), convert = TRUE)
  mort$agecl <- mort$agecl + 1

  # check uniqueness of column notsure and mort
  if (any(sapply(mort[, c("notsure", "mort")], function(x) length(unique(x))) != 1)) {
    stop("Insufficient grouping columns!")
  }

  # First time step

  # Check number of empty entries per predator!
  nr_prey <- length(unique(mort$prey))
  count_zero <- mort %>%
    dplyr::group_by_(~time, ~pred, ~agecl) %>%
    dplyr::summarise_(count_zero = ~sum(atoutput == 0) / nr_prey)

  wuwu <- mort %>%
    dplyr::group_by_(~time, ~pred, ~agecl) %>%
    dplyr::summarise(check = length(time))

  return(mort)
}


