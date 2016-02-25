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

plot <- plot_ts(preprocess_setas$biomass_age)
plot_add_run <- function()
if (!("run" %in% names(plot$data)) stop("Variable run not found in plot!")
if (!ggplot2::is.ggplot(plot)) stop("Provided plot is no ggplot2 plot.")
if ("colour" %in% names(plot$mapping)) {
  warning("Color coding in plot already present. You probably don't want to overwrite this with model runs!")
}

plot <- plot + ggplot2::aes(colour = species)

names(plot$mapping)

plot <- plot + ggplot2::aes_(colour = ~run)

