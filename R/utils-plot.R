custom_wrap <- function(plot, col) {
  if (length(col) != 1) stop("Please supply only one wraping column.")
  plot <- plot + ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(col)),
                                     scales = "free_y",
                                     ncol = 9,
                                     labeller = ggplot2::label_wrap_gen(width = 15))
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
