#' Function to load various txt files from Atlantis simulations
#'
#' @param file Character string giving the connection of the output file.
#' The filename usually contains \code{output} and ends in \code{.txt}".
#' @param id_col Character strings giving the names of the columns which are not variables.
#' Data from all other columns will be gathered with tidyr. Default is \code{"Time"}.
#' @return Dataframe in tidy format!
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' file <- file.path(d, "outputSETASSSB.txt")
#' load_txt(file)
#'
#' file <- file.path(d, "outputSETASYOY.txt")
#' load_txt(file)

load_txt <- function(file, id_col = "Time") {
  # This is the main time consuming step. Checked it with Rprof. E.g. reading in outputNorthSeaSpecificPredMort.txt
  # showed that reading in consumes 99% of the total time. Using read_delim from "readr" results in
  # a minor (~30%) increase in speed (not work implementing though...).
  data <- utils::read.table(file, header = TRUE, stringsAsFactors = FALSE)

  #BJS: If the file has a Unicode UTF-8 BOM (hidden character) at the beginning of the file then remove it
  #
  #This is a bit of a hack, but I'm not sure how else to work around as the BOM has only appeared in one test file
  # if(identical(colnames(data)[1], "ï..Time")) { #couldn't use this because building the package converted ï so it would not match
  #   data <- dplyr::rename(data, Time = "ï..Time")
  # }
  #This assumes that Time will always be the first column and if there is ever anything besides just Time (such as a hidden character to start the file) it will revert the column name back to Time.
  #If this changes in the future, then this will be a functional error that might be hard to detect as it won't cause an error to be thrown
  if (colnames(data)[1] != "Time") {
    data <- dplyr::rename_(data, Time = colnames(data)[1])
  }

  data <- tidyr::gather_(data, key_col = "code", value_col = "atoutput", gather_cols = names(data)[!is.element(names(data), id_col)])
  data$code <- as.character(data$code)
  names(data) <- tolower(names(data))
  return(data)
}


