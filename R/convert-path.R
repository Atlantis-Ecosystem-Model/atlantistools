#' Function to allow for flexible file/filepath usage in the package.
#'
#' Users can either store all their data (modelfiles and outputfiles)
#' in one directory or use different directories for model data and
#' output. In case you are using multiple folder please set the argument
#' \code{dir} to \code{NULL} in all your function calls. In addition
#' use the complete character string composed of path/file for all
#' filename-parameters.
#'
#' @param dir Character string giving the directory the file.
#' @param file Character string giving the filename.
#' @return Character string of the complete dir/filename string.
#'
#' @export
#' @family convert functions
#'
#' @examples
#' convert_path(dir = file.path("c:", "atlantis-model"), file = "functionalGroups.csv")
#' convert_path(dir = NULL, file = file.path("c:", "atlantis-model", "functionalGroups.csv"))

convert_path <- function(dir, file) {
  if (!is.null(dir)) file <- file.path(dir, file)
  return(file)
}
