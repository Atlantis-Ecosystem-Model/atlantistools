#' Create discrete color palette used in plots.
#'
#' The colors are derived from http://colorbrewer2.org/ using the palette
#' 12-class Paired. In addition 9 different gray tones were added in
#' case more than 12 categories are needed (This happens quite often
#' with feeding data).
#' @return Vector of colors in hexadecimal code.
#' @family get functions
#'
#' @export


get_colpal <- function(){
  greys <- c(51, 128, 204, 71, 148, 224, 91, 168, 244)
  greys <- grDevices::rgb(cbind(greys, greys, greys), maxColorValue = 255)
  col_pal <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"), greys)
  return(col_pal)
}
