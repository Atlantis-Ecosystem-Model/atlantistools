#' Function to plot relative contribution of biomass and numbers per cohort.
#'
#' @param data Dataframe to be plotted.
#' @param ncol Number of columns in multipanel plot.
#' @return ggplot2 object
#' @param combine_thresh Integer indicating numbers of species to display in the
#' final plot. The species are ranked according to their overall distribution to
#' the stacked bar. Species with a low contribution are lumped together to the
#' group 'Rest'.
#' @export
#' @family plot functions
#'
#' @examples
#' plot_struct(preprocess_setas$biomass_age)
#' plot_struct(preprocess_setas$biomass, combine_thresh = 3)

plot_struct <- function(data, ncol = 7, combine_thresh = NULL) {
  check_df_names(data = data, expect = c("time", "atoutput", "species"), optional = "agecl")

  if ("agecl" %in% names(data)) {
    data <- agg_perc(data = data, groups = c("species", "time"))
  } else {
    data <- agg_perc(data = data, groups = c("time"))
    data <- combine_groups(data = data, group_col = "species", groups = "time", combine_thresh = combine_thresh)
  }

  plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = ~factor(agecl))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(y = "Distribution [%]") +
    theme_atlantis()

  if ("agecl" %in% names(data)) {
    plot <- plot + ggplot2::facet_wrap( ~species, ncol = ncol, labeller = ggplot2::label_wrap_gen(width = 15))
    plot <- plot + ggplot2::aes_(fill = ~factor(agecl))
    plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  } else {
    plot <- plot + ggplot2::aes_(fill = ~species)
    plot <- plot + ggplot2::theme(legend.position = "right")
  }

  plot <- ggplot_custom(plot)

  return(plot)
}
