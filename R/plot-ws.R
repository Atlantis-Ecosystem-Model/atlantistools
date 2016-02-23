#' Function to plot whole system metrics!
#'
#' @param data Dataframe to be plotted.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_ws(preprocess_setas$biomass)

plot_ws <- function(data, combine_thresh) {
  check_df_names(data = data, expect = c("time", "atoutput", "species"))

  data <- combine_groups(data, group_col = "species", groups = "time", combine_thresh = combine_thresh)

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = ~species)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = get_colpal()) +
    ggplot2::labs(y = "Biomass [t]")
    theme_atlantis() +
    ggplot2::theme(legend.position = "right")

  return(plot)
}




