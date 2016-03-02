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
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' load_txt(dir = d,
#'    file = "outputSETASSSB.txt",
#'    id_col = "Time")

load_txt <- function(dir, file, id_col = "Time") {
  file <- convert_path(dir = dir, file = file)
  data <- read.table(file, header = TRUE, stringsAsFactors = FALSE)
  data <- tidyr::gather_(data, key_col = "code", value_col = "atoutput", gather_cols = names(data)[!is.element(names(data), id_col)])
  data$code <- as.character(data$code)
  names(data) <- tolower(names(data))
  return(data)
}


