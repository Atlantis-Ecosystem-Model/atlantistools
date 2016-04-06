#' Low level plotting function to add range of observed values to time series plots.
#'
#' This function can be used to add the range of observed data to a timeseries plot
#' generated with \code{plot_ts()}. The density of the color gives an indication
#' of the likelihood of the value.
#'
#' @param plot ggplot2 object.
#' @param ex_data Dataframe with observed values for the specific timeseries.
#' @return ggplot2 plot.
#' @export
#'
#' @examples
#' ex_data <- preprocess_setas$biomass
#' ex_data$atoutput <- ex_data$atoutput * runif(n = nrow(ex_data), 0, 1)
#' ex_data$model <- "test"
#' plot <- plot_ts(preprocess_setas$biomass)
#' plot <- plot_add_range(plot, ex_data)
#' plot

plot_add_range <- function(plot, ex_data) {
  if (!ggplot2::is.ggplot(plot)) stop("plot has to be a ggplot object.")
  names(ex_data)[names(ex_data) == "biomass"] <- "atoutput"
  ex_data$time <- as.Date(x = ex_data$time, format = "%Y-%m-%d")

  check_df_names(ex_data, expect = c("time", "species", "atoutput", "model"))
  if ("colour" %in% names(plot$mapping)) {
    warning("Color coding in plot already present. You probably don't want to overwrite this with 'plot_add_range'.")
  }

  plot <- plot + ggplot2::geom_rug(data = ex_data, ggplot2::aes_(colour = ~model),
                                   sides = "lr", position = "jitter", alpha = 0.5)

  return(plot)
}


