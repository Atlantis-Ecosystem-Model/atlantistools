#' Load the functional group file
#'
#' Read in the functional group file as dataframe.
#'
#' @param dir Path of the Atlantis model folder.
#' @param fgs Name of the functional groups file which is typically saved as
#' \code{functionalGroups.csv}.
#'
#' @export
#' @family load functions
#'
#' @return A \code{data.frame} of functional group information.
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' file <- "SETasGroups.csv"
#' fgs <- load_fgs(d, file)
#' head(fgs)

load_fgs <- function(dir = getwd(), fgs) {
  if (!is.null(dir)) fgs <- file.path(dir, fgs)
  result <- utils::read.table(file = fgs, sep = ",",
                       header = TRUE, stringsAsFactors = FALSE)
  # Rename column longname if space is present in raw file.
  names(result)[names(result) == "Long.Name"] <- "LongName"
  return(result)
}
