#' Load the functional group file
#'
#' Read in the functional group file as dataframe.
#'
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
#' fgs <- load_fgs(file.path(d, file))
#' head(fgs)
#'
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' file <- "SETasGroupsDem_NoCep.csv"
#' fgs <- load_fgs(file.path(d, file))
#' head(fgs)

load_fgs <- function(fgs) {
  result <- utils::read.table(file = fgs, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  # Rename column longname if space is present in raw file.
  names(result)[names(result) == "Long.Name"] <- "LongName"
  return(result)
}
