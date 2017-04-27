#' Add spatial representation of polygon layout to a ggplot2 object.
#'
#' @param plot ggplot2 object. Can be a ggplot grob.
#' @param bgm_as_df *.bgm file converted to a dataframe. Please use \code{\link{convert_bgm}}
#' to convert your bgm-file to a dataframe with columns 'lat', 'long', 'inside_lat',
#' 'inside_long' and 'polygon'.
#' @param polygon_overview numeric value between 0 and 1 indicating the size used to plot the polygon overview in the
#' upper right corner of the plot. Default is \code{0.2}.
#' @return ggplot grob
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' bgm_as_df <- convert_bgm(bgm = file.path(d, "VMPA_setas.bgm"))
#'
#' p <- plot_line(preprocess$physics, wrap = NULL)
#' p <- custom_grid(p, grid_x = "polygon", grid_y = "variable")
#'
#' grob <- plot_add_polygon_overview(p, bgm_as_df)
#' gridExtra::grid.arrange(grob)

plot_add_polygon_overview <- function(plot, bgm_as_df, polygon_overview = 0.2) {
  bl <- plot_boxes(data = bgm_as_df)

  # Combine plots!
  g2 <- gridExtra::arrangeGrob(bl, ncol = 1,
                               heights = grid::unit(c(polygon_overview, 1 - polygon_overview), units = "npc"))

  if (ggplot2::is.ggplot(plot)) {
    grobs <- list(gridExtra::arrangeGrob(plot), g2)
  } else {
    grobs <- list(plot, g2)
  }

  final_grob <- gridExtra::arrangeGrob(grobs = grobs, ncol = 2,
                                         widths = grid::unit(c(1 - polygon_overview, polygon_overview), units = "npc"))

  return(final_grob)
}
