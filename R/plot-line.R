#' Function to plot time series of atlantis ncdf output.
#'
#' @param data Dataframe to be plotted.
#' @param x x-variable. Default is \code{'time'}.
#' @param y y-variable. Default is \code{'atoutput'}.
#' @param wrap Wraping column. Default is \code{'species'}
#' @param col Column to use as colour. Default is \code{NULL}.
#' @param ncol Number of columns in multipanel plot. Default is \code{7}.
#' @param yexpand Expands the y axis so it always includes 0. Default is \code{FALSE}.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' plot_line(preprocess$biomass)
#' plot_line(preprocess$biomass, col = "species")
#' plot_line(preprocess$biomass_age, col = "agecl")
#' plot_line(preprocess$biomass_age, wrap = "agecl", col = "species")
#'
#' # The function can also be used to compare model outoput with observed data.
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' ex_data <- read.csv(file.path(d, "setas-bench.csv"), stringsAsFactors = FALSE)
#' names(ex_data)[names(ex_data) == "biomass"] <- "atoutput"
#'
#' data <- preprocess$biomass
#' data$model <- "atlantis"
#' comp <- rbind(ex_data, data, stringsAsFactors = FALSE)
#'
#' # Show atlantis as first factor!
#' lev_ord <- c("atlantis", sort(unique(comp$model))[sort(unique(comp$model)) != "atlantis"])
#' comp$model <- factor(comp$model, levels = lev_ord)
#'
#' # Create plot
#' plot_line(comp, col = "model")
#'
#' # Use \code{\link{convert_relative_initial}} and \code{\link{plot_add_box}}
#' # with \code{\link{plot_line}}. Use \code{\link{convert_relative_initial}} to
#' # generate a relative time series first.
#' df <- convert_relative_initial(preprocess$structn_age)
#'
#' # Create the base plot with \code{\link{plot_line}}.
#' plot <- plot_line(df, col = "agecl")
#'
#' # Add lower and upper range.
#' plot_add_box(plot)
#'
#' # Create spatial timeseries plots in conjuction with \code{\link{custom_grid}}.
#' plot <- plot_line(preprocess$physics, wrap = NULL)
#' custom_grid(plot, grid_x = "polygon", grid_y = "variable")
#'
#' plot <- plot_line(preprocess$flux, wrap = NULL, col = "variable")
#' custom_grid(plot, grid_x = "polygon", grid_y = "layer")

plot_line <- function(data, x = "time", y = "atoutput", wrap = "species", col = NULL, ncol = 7, yexpand = FALSE) {
  plot <- custom_map(data = data, x = x, y = y) +
    ggplot2::geom_line() +
    theme_atlantis()
  if (!is.null(wrap)) plot <- custom_wrap(plot, col = wrap, ncol = ncol)
  plot <- ggplot_custom(plot)

  # Add colour
  if (!is.null(col)) {
    # check if integer or not
    if (is.numeric(data[, col][[1]]) && all(data[, col] %% 1 == 0)) {
      plot <- plot + ggplot2::aes_(colour = lazyeval::interp(~factor(var), var = as.name(col)))
      plot <- plot + ggplot2::guides(col = ggplot2::guide_legend(nrow = 1))
    } else {
      plot <- plot + ggplot2::aes_(colour = lazyeval::interp(~var, var = as.name(col)))
    }
  }

  if (yexpand == TRUE){
    plot <- plot + ggplot2::expand_limits(y = 0)
  }
  return(plot)
}


