#' Function to plot relative contribution of biomass and numbers per cohort.
#'
#' @inheritParams plot_line
#' @param fill Column to use as filling colour. Default is \code{"species"}.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' plot_bar(preprocess$biomass)
#'
#' # Most models have a large number of groups. Please make sure to combine groups with a low
#' # contribution prior to plotting with \code{\link{combine_groups}}.
#' df <- combine_groups(preprocess$biomass, group_col = "species", combine_thresh = 3)
#' plot_bar(df)
#'
#' # This function can also be used to plot age-specific data.
#' plot_bar(preprocess$nums_age, fill = "agecl", wrap = "species")
#'
#' # Please use \code{\link{agg_perc}} to visualize the relative cohort structure over time.
#' df <- agg_perc(preprocess$nums_age, groups = c("time", "species"))
#' plot_bar(df, fill = "agecl", wrap = "species")

plot_bar <- function(data, x = "time", y = "atoutput", fill = "species", wrap = NULL, ncol = NULL) {
  plot <- custom_map(data = data, x = x, y = y) +
    ggplot2::geom_bar(stat = "identity") +
    theme_atlantis()

  # Add colour. check if integer or not
  if (is.numeric(data[, fill][[1]]) && all(data[, fill] %% 1 == 0)) {
    plot <- plot + ggplot2::aes_(fill = lazyeval::interp(~factor(var), var = as.name(fill)))
    plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  } else {# used in whole system and diet plots!
    plot <- plot + ggplot2::aes_(fill = lazyeval::interp(~var, var = as.name(fill)))
    plot <- plot + ggplot2::scale_fill_manual(values = get_colpal())
    plot <- plot + ggplot2::theme(legend.position = "right")
  }

  # Wrap in case wrap is not NULL!
  if (!is.null(wrap)) {
    if (is.null(ncol)) ncol <- 7  # set default if NULL
    plot <- custom_wrap(plot, col = wrap, ncol = ncol)
  }

  plot <- ggplot_custom(plot)

  return(plot)
}
