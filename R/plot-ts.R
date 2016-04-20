#' Function to plot time series of atlantis ncdf output.
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' plot_ts(preprocess_setas$biomass_age)

plot_ts <- function(data, x = "time", y = "atoutput", wrap = "species") {
  if (!any(is.element(names(data), "time"))) {
    stop("Column time not found in data")
  }

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = lazyeval::interp(~var, var = as.name(x)), y = lazyeval::interp(~var, var = as.name(y)))) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(wrap)), scales = "free_y", ncol = 9, labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
    theme_atlantis()
  plot <- ggplot_custom(plot)

  # Allow plotting for both cohort and non-cohort data!
  if (is.element("agecl", names(data))) {
    plot <- plot + ggplot2::aes_(colour = ~factor(agecl))
  }

  return(plot)
}




