#' Plot layout of boxes!
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' bgm_data <- convert_bgm(dir = d, bgm = "VMPA_setas.bgm")
#' plot_boxes(bgm_data)

plot_boxes <- function(data) {
  check_df_names(data = data, expect = c("long", "lat", "polygon", "inside_lat", "inside_long"))

  inside <- unique(subset(data, select = c("inside_lat", "inside_long", "polygon")))
  names(inside)[1:2] <- c("lat", "long")
  plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~long, y = ~lat, fill = ~factor(polygon), group = ~factor(polygon), label = ~polygon)) +
    ggplot2::geom_polygon(colour = "black") +
    ggplot2::geom_text(data = inside) +
    theme_atlantis() +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3)) +
    ggplot2::theme(legend.position = "right")

  return(plot)
}
