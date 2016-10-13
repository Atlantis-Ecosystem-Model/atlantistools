#' Function to plot time series of atlantis ncdf output.
#'
#' @param data Dataframe to be plotted.
#' @param x x-variable. Default is \code{'time'}.
#' @param y y-variable. Default is \code{'atoutput'}.
#' @param wrap Wraping column. Default is \code{'species'}
#' @param col Column to use as colour. Default is \code{NULL}.
#' @param ncol Number of columns in multipanel plot.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' plot_line(preprocess_setas$biomass)
#' plot_line(preprocess_setas$biomass, col = "species")
#' plot_line(preprocess_setas$biomass_age, col = "agecl")


plot_line <- function(data, x = "time", y = "atoutput", wrap = "species", col = NULL, ncol = 7) {
  plot <- custom_map(data = data, x = x, y = y) +
    ggplot2::geom_line() +
    theme_atlantis()
  plot <- custom_wrap(plot, col = wrap, ncol = ncol)
  plot <- ggplot_custom(plot)

  # Add colour
  if (!is.null(col)) {
    # check if integer or not
    if (is.numeric(data[, col][[1]]) && all(data[, col] %% 1 == 0)) {
      plot <- plot + ggplot2::aes_(colour = lazyeval::interp(~factor(var), var = as.name(col)))
      plot <- plot + ggplot2::guides(col = ggplot2::guide_legend(nrow = 1))
    } else {
      plot <- plot + ggplot2::aes_(colour = lazyeval::interp(~var, var = as.name(col)))
    }
  }

  return(plot)
}


