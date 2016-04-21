#' Function to plot time series of atlantis ncdf output.
#'
#' @param data Dataframe to be plotted.
#' @param x x-variable. Default is \code{'time'}.
#' @param y y-variable. Default is \code{'atoutput'}.
#' @param wrap Wraping column. Default is \code{'species'}
#' @param ncol Number of columns in multipanel plot.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' plot_ts(preprocess_setas$biomass_age)

plot_ts <- function(data, x = "time", y = "atoutput", wrap = "species", ncol = 7) {
  if (!any(is.element(names(data), "time"))) {
    stop("Column time not found in data")
  }

  plot <- custom_map(data = data, x = x, y = y) +
    ggplot2::geom_line() +
    theme_atlantis() +
    ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(wrap)),
                        scales = "free_y",
                        ncol = ncol,
                        labeller = ggplot2::label_wrap_gen(width = 15))

  plot <- ggplot_custom(plot)

  # Allow plotting for both cohort and non-cohort data!
  if (is.element("agecl", names(data))) {
    plot <- plot +
      ggplot2::aes_(colour = ~factor(agecl)) +
      ggplot2::guides(col = ggplot2::guide_legend(nrow = 1))
  }

  return(plot)
}




