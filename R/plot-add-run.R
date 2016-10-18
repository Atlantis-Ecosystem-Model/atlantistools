#' Low level plotting function to apply color coding to plots based on run variable!
#'
#' This function can be used to add colour coding to plotting routines without color specification
#' e.g. \code{plot_ts()}, \code{plot_calibrate()} or \code{plot_physics()}. The colorcoding is
#' based on the column run so you need to apply \code{combine_runs} to your data first!
#' Please note that \code{plot_ts()} and \code{plot_calibrate()} only work if they are created
#' with non-age based data.
#'
#' @param plot ggplot2 object.
#' @return ggplot2 plot.
#' @export
#' @family low-level-plot functions
#'
#' @examples
#' # We need to create dummy data from an additional simulation first.
#' dummy <- preprocess$biomass
#' dummy$atoutput <- dummy$atoutput * 1.5
#'
#' # Combine the two runs!
#' dfs <- combine_runs(list(list(preprocess$biomass), list(dummy)), runs = c("setas", "dummy"))
#'
#' # Plot timeseries and add color coding according to column 'run'.
#' plot <- plot_line(dfs[[1]])
#' plot_add_run(plot)

plot_add_run <- function(plot) {
  # Check input
  # Otherwise the error first occurs when the plot is drawn!
  if (!("run" %in% names(plot$data))) stop("Variable run not found in plot!")
  if (!ggplot2::is.ggplot(plot)) stop("Plot is no ggplot2 object.")
  if ("colour" %in% names(plot$mapping)) {
    warning("Color coding in plot already present. You probably don't want to overwrite this with model 'run'!")
  }
  plot <- plot + ggplot2::aes_(colour = ~run)
  return(plot)
}


