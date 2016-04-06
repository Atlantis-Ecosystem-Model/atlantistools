#' Function to plot time series of atlantis ncdf output.
#'
#' @param data Dataframe to be plotted.
#' @param ex_data Dataframe with observed values for the specific timeseries.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_ts(preprocess_setas$biomass_age)
#'
#' ex_data <- preprocess_setas$biomass
#' ex_data$atoutput <- ex_data$atoutput * runif(n = nrow(ex_data), 0, 1)
#' ex_data$model <- "test"
#' plot <- plot_ts(preprocess_setas$biomass, ex_data)
#' plot


plot_ts <- function(data, ex_data = NULL) {
  if (!any(is.element(names(data), "time"))) {
    stop("Column time not found in data")
  }

  check_df_names(data = data, expect = c("time", "atoutput", "species"), optional = c("agecl", "run"))

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput))
  # DOESNT WORK WITH ANNOTATE DUE TO FACET_WRAP!!!!!
  if (!is.null(ex_data)) {
    check_df_names(ex_data, expect = c("time", "atoutput", "species", "model"))
    models <- sort(unique(ex_data$model))
    colors <- get_colpal()[1:length(unique(ex_data$model))]
    for (i in seq_along(colors)) {
      # create data for model!
      wuwu <- ex_data
      wuwu$time <- NULL
      wuwu <- wuwu %>%
        dplyr::filter_(~model == models[i]) %>%
        dplyr::group_by_(~species) %>%
        dplyr::summarise(min = quantile(atoutput, probs = 0.25), max = quantile(atoutput, probs = 0.75))

    }

    # cols <- colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(10)
    # cols <- c(rev(cols), cols[-1])
    # wuwu <- ex_data
    # wuwu$time <- NULL
    # wuwu <- wuwu %>%
    #   dplyr::group_by(species, model) %>%
    #   dplyr::summarise(min = quantile(atoutput, probs = 0.25), max = quantile(atoutput, probs = 0.75))
    # wawa <- mapply(FUN = seq, wuwu$min, wuwu$max, MoreArgs = list(length.out = 19))
    # colnames(wawa) <- wuwu$species
    # wawa <- tidyr::gather_(as.data.frame(wawa), key = "species", value = "value", colnames(wawa))
    # wawa$col <- rep(cols, times = length(wuwu$species))
    # plot <- ggplot2::ggplot(data = wawa, ggplot2::aes_(yintercept = ~value, colour = ~value)) +
    #   ggplot2::geom_hline()
    #
    # plot <- plot + ggplot2::scale_color_continuous()
  }
  plot <- plot +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~species, scales = "free_y", ncol = 9, labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::coord_cartesian(expand = FALSE) +
    theme_atlantis()

  # Allow plotting for both cohort and non-cohort data!
  if (is.element("agecl", names(data))) {
    plot <- plot + ggplot2::aes_(colour = ~factor(agecl))
  }

  return(plot)
}


rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  annotation_raster(rainbow, 15, 20, 3, 4)


