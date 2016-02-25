#' Low level plotting function to apply color coding to plots based on run variable!
#'
#' This function can be used to use plotting routines without color specification
#' e.g. `plot_ts()`, `plot_calibrate()` or `plot_physics()` for model comparisons.
#' Please note that `plot_ts()` and `plot_calibrate()` only work for non age based
#' data. The colorcoding is based on the column run so you need to apply combine runs
#' to your data first!
#'
#' @param plot ggplot2 object.
#' @return ggplot2 plot.
#' @export
#'
#' @examples
#'
