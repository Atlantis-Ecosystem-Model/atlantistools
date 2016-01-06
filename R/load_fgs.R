#' Load the functional group file
#'
#' Read in the functional group file as dataframe.
#'
#' @prm dir Path of the Atlantis model folder.
#' @prm file_fgs Name of the functional groups file which is typically saved as
#' \code{functionalGroups.csv}.
#'
#' @export
#' @family load functions
#'
#' @return A \code{data.frame} of functional group information.
#'
#' @examples
#' d <- system.file("extdata", testscenario, package = "atlantisom")
#' file <- "functionalGroups.csv"
#' fgs <- load_fgs(d, file)

load_fgs <- function(dir = getwd(), file_fgs) {
  if (is.null(dir)) {
    file.fgs <- file_fgs
  } else {
    file.fgs <- file.path(dir, file_fgs)
  }
  result <- read.table(file = file.fgs, sep = ",",
                       header = TRUE, stringsAsFactors = FALSE)
  return(result)
}
