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
#' load_spec_mort(dir = d,
#'    specmort = "outputSETASSpecificPredMort.txt")


load_spec_mort <- function(dir = getwd(), specmort) {
  specmort <- load_txt(dir = dir, file = specmort)
  return(specmort)
}


