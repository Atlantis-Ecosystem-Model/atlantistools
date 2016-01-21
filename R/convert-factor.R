#' Function to convert any column with information about functional groups
#' to a factor whose levels use the LongName of the functional groups file.
#'
#' This function is used to match the labels of the plots!
#'
#' @param data_fgs Dataframe with information about functionalGroups.
#' Usually loaded with load_fgs().
#' @param col Column of the dataframe which is converted to a factor.
#' @return Column of a dataframe in factor format.
#' @export

convert_factor <- function(data_fgs, col) {
  # Create vector of sorted labels!
  avail <- unique(col)
  if (all(is.element(avail, data_fgs$Code))) {
    labels <- sort(data_fgs$LongName[is.element(data_fgs$Code, unique(col))])
    col <- factor(col, levels = data_fgs$Code[sapply(labels, function(x) which(data_fgs$LongName == x))], labels = labels)
  } else {
    if (all(is.element(avail, data_fgs$Name))) {
       labels <- sort(data_fgs$LongName[is.element(data_fgs$Name, unique(col))])
       col <- factor(col, levels = data_fgs$Name[sapply(labels, function(x) which(data_fgs$LongName == x))], labels = labels)
    } else {
      stop("Not all entries in column Code (or Name) match the entries in parameter col!")
    }
  }

  return(col)
}

