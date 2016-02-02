#' Function to plot ncdf output of physical variables.
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_physics(preprocess_setas$physics)

plot_physics <- function(data) {
  check_df_names(data = data, expect = c("time", "atoutput", "variable", "polygon"))

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variable ~ polygon, scales = "free", labeller = ggplot2::label_wrap_gen(width = 15)) +
    theme_atlantis()

  return(plot)
}




