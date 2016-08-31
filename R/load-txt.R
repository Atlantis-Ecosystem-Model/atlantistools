#' Function to load various txt files from Atlantis simulations
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param file Character string of the file. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as to file. In addition set dir to 'NULL' in this
#' case.
#' @param id_col Character strings giving the names of the columns which are not variables.
#' Data from all other columns will be gathered with tidyr.
#' @return Dataframe in tidy format!
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' load_txt(dir = d,
#'    file = "outputSETASSSB.txt",
#'    id_col = "Time")

load_txt <- function(dir, file, id_col = "Time") {
  file <- convert_path(dir = dir, file = file)
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
  if(colnames(data)[1] != "Time") {
    data <- dplyr::rename_(data, Time = colnames(data)[1])
  }

  data <- tidyr::gather_(data, key_col = "code", value_col = "atoutput", gather_cols = names(data)[!is.element(names(data), id_col)])
  data$code <- as.character(data$code)
  names(data) <- tolower(names(data))
  return(data)
}


