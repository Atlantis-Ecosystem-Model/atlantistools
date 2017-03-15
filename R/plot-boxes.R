#' Plot layout of boxes!
#'
#' @param data Dataframe to be plotted.
#' @param color_boxes logical indicating if polygons should be color coded or not. Default is \code{TRUE}.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' bgm_data <- convert_bgm(file.path(d, "VMPA_setas.bgm"))
#'
#' # Use color coding for polygons.
#' plot_boxes(bgm_data)
#'
#' # Only use text to indicate polygons.
#' plot_boxes(bgm_data, color_boxes = FALSE)

plot_boxes <- function(data, color_boxes = TRUE) {
  check_df_names(data = data, expect = c("long", "lat", "polygon", "inside_lat", "inside_long"))

  inside <- unique(subset(data, select = c("inside_lat", "inside_long", "polygon")))
  names(inside)[1:2] <- c("lat", "long")

  plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~long, y = ~lat, group = ~factor(polygon), label = ~polygon))

  if (color_boxes) {
    plot <- plot + ggplot2::geom_polygon(ggplot2::aes_(fill = ~factor(polygon)), colour = "black")
  } else {
    plot <- plot + ggplot2::geom_polygon(colour = "black", fill = "white")
  }

  plot <- plot + ggplot2::geom_text(data = inside)
  plot <- plot + ggplot2::theme_void()
  plot <- plot + ggplot2::theme(legend.position = "none")

  return(plot)
}
