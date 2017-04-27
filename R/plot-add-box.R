#' Low level plotting function to add sudo confidence range to calibration plots.
#'
#' @param plot ggplot2 object.
#' @param range max and min relative change of data. Default is \code{c(0.5, 0.2)}.
#' @return ggplot2 plot.
#' @export
#' @family low-level-plot functions
#'
#' @examples
#' # Make sure to use a relative timeseries generated with \code{\link{convert_relative_initial}}.
#' df <- convert_relative_initial(preprocess$structn_age)
#'
#' # Create the plot with \code{\link{plot_line}}.
#' plot <- plot_line(df, col = "agecl")
#'
#' # Add lower and upper range.
#' plot_add_box(plot)
#'
#' # You can set the upper and lower range as you like!
#' plot_add_box(plot, range = c(0.8, 0.4))

# Function to add data range to calibration plot!
plot_add_box <- function(plot, range = c(0.5, 0.2)) {
  plot <- plot + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1 - range[1], ymax = 1 + range[1], alpha = 0.1)
  plot <- plot + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1 - range[2], ymax = 1 + range[2], alpha = 0.3)
  plot <- plot + ggplot2::geom_hline(yintercept = 1, linetype = "dotted")

  # Rearrange layers. Set newly added layers as first layers!
  nl <- length(plot$layers)
  plot$layers <- plot$layers[c((nl - 2):nl, 1:(nl - 3))]

  return(plot)
}
