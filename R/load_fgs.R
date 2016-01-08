#' Load the functional group file
#'
#' Read in the functional group file as dataframe.
#'
#' @param  dir Path of the Atlantis model folder.
#' @param file_fgs Name of the functional groups file which is typically saved as
#' \code{functionalGroups.csv}.
#'
#' @export
#' @family load functions
#'
#' @return A \code{data.frame} of functional group information.
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' file <- "functionalGroups.csv"
#' fgs <- load_fgs(d, file)
#' fgs

load_fgs <- function(dir = getwd(), file_fgs) {
  if (!is.null(dir)) file_fgs <- file.path(dir, file_fgs)
  result <- read.table(file = file_fgs, sep = ",",
                       header = TRUE, stringsAsFactors = FALSE)
  return(result)
}
