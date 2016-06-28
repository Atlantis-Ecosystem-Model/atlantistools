#' Create discrete color palette used in plots.
#'
#' The colors are derived from http://colorbrewer2.org/ using the palette
#' 12-class Paired. In addition 9 different grey tones were added in
#' case more than 12 categories are needed (This happens quite often
#' with feeding data).
#' @return Vector of colors in hexadecimal code.
#' @family get functions
#'
#' @export


get_colpal <- function(){
  r <- c(166, 31, 178, 51, 251, 227, 253, 255, 202, 106, 255, 177, 51, 128, 204, 71, 148, 224, 91, 168, 244)
  g <- c(206, 120, 223, 160, 154, 26, 191, 127, 178, 61, 255, 89, 51, 128, 204, 71, 148, 224, 91, 168, 244)
  b <- c(227, 180, 138, 44, 153, 28, 111, 0, 214, 154, 153, 40, 51, 128, 204, 71, 148, 224, 91, 168, 244)
  col_pal <- grDevices::rgb(cbind(r, g, b), maxColorValue = 255)
  return(col_pal)
}
