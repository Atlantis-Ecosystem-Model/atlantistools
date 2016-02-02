#' Plot layout of boxes!
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' bgm_data <- convert_bgm(dir = d, bgm = "VMPA_setas.bgm")
#' plot_boxes(bgm_data)

plot_boxes <- function(data) {
  inside <- unique(subset(data, select = c("inside_lat", "inside_long", "box")))
  names(inside)[1:2] <- c("lat", "long")
  plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~long, y = ~lat, fill = ~factor(box), group = ~factor(box), label = ~box)) +
    ggplot2::geom_polygon(colour = "black") +
    ggplot2::geom_text(data = inside) +
    theme_atlantis() +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::theme(legend.position = "right")

  return(plot)
}
