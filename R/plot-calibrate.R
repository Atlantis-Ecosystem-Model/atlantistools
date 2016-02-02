#' Function to plot relative changes over time.
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_calibrate(preprocess_setas$structn_age)

plot_calibrate <- function(data) {
  if (!any(is.element(names(data), "time"))) {
    stop("Column time not found in data")
  }
  # Divide values by reference value (time = min(time))
  ref <- data[data$time == min(data$time), ]
  ref$time <- NULL
  names(ref)[names(ref) == "atoutput"] <- "atoutput_ref"
  result <- data %>%
    dplyr::left_join(ref) %>%
    dplyr::mutate_(atoutput = ~atoutput / atoutput_ref)
  result$atoutput[result$atoutput_ref == 0] <- 0

  anno <- c(min(data$time), max(data$time))

  plot <- ggplot2::ggplot(data = result, ggplot2::aes_(x = ~time, y = ~atoutput)) +
    ggplot2::annotate("rect", xmin = anno[1], xmax = anno[2], ymin = 0.5, ymax = 1.5, alpha = 0.1) +
    ggplot2::annotate("rect", xmin = anno[1], xmax = anno[2], ymin = 0.8, ymax = 1.2, alpha = 0.3) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted") +
    ggplot2::facet_wrap(~species, scales = "free_y", ncol = 8) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(y = "Relative to model start") +
    theme_atlantis()

  # This ensures that the plotting
  if (is.element("agecl", names(data))) {
    plot <- plot + ggplot2::aes_(colour = ~factor(agecl))
  }

  return(plot)
}




