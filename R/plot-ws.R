#' Function to plot whole system metrics!
#'
#' @param data Dataframe to be plotted.
#' @param combine_thresh Integer indicating minimum amount to the stomach contribution.
#' Each prey item with a lower contribution as this treshold is assigned to the
#' grpup "Rest". This is necessary for species with a wide variety of food items
#' in their diet. Otherwise the screen will be cluttered with colors in the
#' dietplots. Default is 0.03.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' plot_ws(preprocess_setas$biomass)

plot_ws <- function(data, combine_thresh = 15) {
  check_df_names(data = data, expect = c("time", "atoutput", "species"))

  data <- combine_groups(data, group_col = "species", combine_thresh = combine_thresh)

  # Arrange data according to contribution!
  agg_data <- agg_data(data, groups = c("species"), out = "sum_at", fun = sum)
  data$species <- factor(data$species, levels = agg_data$species[order(agg_data$sum_at, decreasing = TRUE)])

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = ~species)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = get_colpal()) +
    ggplot2::labs(y = "Biomass [t]") +
    theme_atlantis() +
    ggplot2::theme(legend.position = "right")
  plot <- ggplot_custom(plot)

  return(plot)
}




