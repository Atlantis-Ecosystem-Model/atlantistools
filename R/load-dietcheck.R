#' Read in the atlantis dietcheck.txt file and perform some basic data transformations.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param dietcheck Character string of the DietCheck.txt file. Usually
#' 'output[...]DietCheck.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   species, time, agecl, and atoutput (i.e., variable).
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' test <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", fgs = "functionalGroups.csv")

load_dietcheck <- function(dir, dietcheck, fgs) {
  dietcheck <- convert_path(dir = dir, file = dietcheck)
  if (!file.exists(dietcheck)) {
    stop(paste("File", dietcheck, "not found. Plase make sure to use "))
  }

  # read in diet information
  diet <- read.table(file = dietcheck, header = TRUE, sep = " ", stringsAsFactors = FALSE)

  # remove entries without diet-information!
  diet <- diet[rowSums(x = diet[, 5:ncol(diet)]) != 0,]


}
