#' Function to convert any column with information about functional groups
#' to a factor whose levels use the LongName of the functional groups file.
#'
#' This function is used to match the labels of the plots!
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param col Column of the dataframe which is converted to a factor.
#' @return Column of a dataframe in factor format.
#' @export
#'

convert_factor <- function(dir, fgs, col) {
  fgs <- convert_path(dir = dir, file = fgs)
  fgs <- load_fgs(dir = dir, fgs = fgs)

  # Create vector of sorted labels!
  avail <- unique(col)
  if (any(is.element(avail, fgs$Code))) {
    labels <- sort(fgs$LongName[is.element(fgs$Code, unique(col))])
    col <- factor(col, levels = fgs$Code[sapply(labels, function(x) which(fgs$LongName == x))], labels = labels)
  } else {
    if (any(is.element(avail, fgs$Name))) {
       labels <- sort(fgs$LongName[is.element(fgs$Name, unique(col))])
       col <- factor(col, levels = fgs$Name[sapply(labels, function(x) which(fgs$LongName == x))], labels = labels)
    } else {
      stop("Neither entries in column Code nor Name match the entries in parameter col!")
    }
  }

  return(col)
}

