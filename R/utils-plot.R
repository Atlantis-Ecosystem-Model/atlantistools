#' Utility functions used for various plotting routines within atlantistools.
#'
#' @param plot ggplot2 plot.
#' @param grid_x character vector used in facet_grid in horizontal direction.
#' @param grid_y character vector used in facet_grid in vertical direction.
#' @return ggplot2 plot.
#' @export

# Customised nse version of facet_grid used within atlantistools
custom_grid <- function(plot, grid_x, grid_y) {
  px <- paste(grid_x, collapse = "+")
  py <- paste(grid_y, collapse = "+")
  plot <- plot + ggplot2::facet_grid(stats::as.formula(paste(py, "~", px)),
                                     scales = "free", labeller = ggplot2::label_wrap_gen(width = 15))
  return(plot)
}

# Customised nse version of facet_wrap used within atlantistools
custom_wrap <- function(plot, col, ncol = 7) {
  plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(col, collapse = "+"))),
                                     scales = "free_y", ncol = ncol, labeller = ggplot2::label_wrap_gen(width = 15))
  return(plot)
}

# Customised function to simplify nse mapping of x- and y variable.
# Additional mappings may be added here. However, x- and y are the most fundamental ones...
custom_map <- function(data, x, y) {
  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = lazyeval::interp(~var, var = as.name(x)),
                                                     y = lazyeval::interp(~var, var = as.name(y))))
  return(plot)
}

# Changes applied to every ggplot object within atlantistools.
ggplot_custom <- function(plot, scientific = TRUE) {
  plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
  if (scientific) {
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::scientific_format(digits = 2))
  } else {
    plot <- plot + ggplot2::scale_y_continuous(labels = abbreviate)
  }
  return(plot)
}

# Change specific theme elemets of plots. Only used in plot_species!
change_theme <- function(plot) {
  plot <- plot + ggplot2::theme(legend.position = "none",
                          axis.title.x = ggplot2::element_blank(),
                          strip.text.x = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
  return(plot)
}

