#' Function to plot whole system metrics!
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_ws(preprocess_setas$biomass)

plot_ts <- function(data, combine_thresh) {
  check_df_names(data = data, expect = c("time", "atoutput", "species"))

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~species, scales = "free_y", ncol = 9, labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
    theme_atlantis()

  # Allow plotting for both cohort and non-cohort data!
  if (is.element("agecl", names(data))) {
    plot <- plot + ggplot2::aes_(colour = ~factor(agecl))
  }

  return(plot)
}




