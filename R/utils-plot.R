custom_wrap <- function(plot, col, ncol = 7) {
  plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(col, collapse = "+"))),
                                     scales = "free_y", ncol = ncol, labeller = ggplot2::label_wrap_gen(width = 15))
  return(plot)
}

custom_grid <- function(plot, grid_x, grid_y) {
  px <- paste(grid_x, collapse = "+")
  py <- paste(grid_y, collapse = "+")
  plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste(px, "~", py)),
                                     scales = "free", labeller = ggplot2::label_wrap_gen(width = 15))
  return(plot)
}

custom_map <- function(data, x, y) {
  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = lazyeval::interp(~var, var = as.name(x)),
                                                     y = lazyeval::interp(~var, var = as.name(y))))
  return(plot)
}

ggplot_custom <- function(plot) {
  plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
  plot <- plot + ggplot2::scale_y_continuous(labels = abbreviate)
  return(plot)
}


plot_add_box <- function(plot) {


}

