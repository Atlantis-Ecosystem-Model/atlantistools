#' Plot specific mortality per predator and age!
#'
#' @param data Dataframe with information about dpecific mortality. The dataframe
#' should be generated with \code{\link{preprocess}}.
#' @param combine_thresh Integer indicating minimum amount to the mortality contribution.
#' Each species item with a lower contribution as this treshold is assigned to the
#' grpup "Rest". This is necessary for species with a wide large prey spectrum.
#' Otherwise the screen will be cluttered with colors in the plots. Default is 0.03.
#' @return ggplot2 grobs for multiple pages.
#' @export
#'
#' @examples
#' plots <- plot_specmort(data = preprocess_setas$diet_specmort, combine_thresh = 0.03)
#' gridExtra::grid.arrange(plots[[1]])

plot_dietcheck <- function(data, combine_thresh = 0.03) {
  check_df_names(data = data, expect = c("time", "pred", "agecl", "prey", "atoutput"))

  # Timeseries per species of absolute values!
  # ts_diet <- agg_sum(data, groups = c("time", "pred", "habitat"))

  # Combine prey groups with low contribution to the diet!
  pred <- combine_groups(data,
                         group_col = "prey",
                         groups = c("time", "pred", "agecl"),
                         combine_thresh = combine_thresh)

  prey <- combine_groups(data,
                         group_col = "pred",
                         groups = c("time", "prey", "agecl"),
                         combine_thresh = combine_thresh)

  # Convert diet data to percentages!
  pred <- agg_perc(pred, groups = c("time", "pred", "agecl"))
  prey <- agg_perc(prey, groups = c("time", "prey", "agecl"))

  # data$prey[data$atoutput <= combine_thresh] <- "Rest"
  # data <- agg_sum(data, groups = c("time", "pred", "habitat", "prey"))

  plot_func <- function(data) {
    # order data according to dietcontribution
    agg_data <- data %>%
      dplyr::group_by_(~prey) %>%
      dplyr::summarise_(sum_diet = ~sum(atoutput))
    data$prey <- factor(data$prey, levels = agg_data$prey[order(agg_data$sum_diet, decreasing = TRUE)])
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = ~prey)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = get_colpal()) +
      ggplot2::facet_wrap(~agecl, ncol = 5, labeller = ggplot2::label_wrap_gen(width = 15)) +
      ggplot2::labs(x = "time", y = "relative M2", title = NULL) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(title = paste("Predationmortality caused by:", unique(data$pred))) +
      theme_atlantis() +
      ggplot2::theme(legend.position = "right")

    return(plot)
  }

  plot_func2 <- function(data) {
    # order data according to dietcontribution
    # agg_data <- data %>%
    #   dplyr::group_by_(~pred) %>%
    #   dplyr::summarise_(sum_diet = ~sum(atoutput))
    # data$pred <- factor(data$pred, levels = agg_data$pred[order(agg_data$sum_diet, decreasing = TRUE)])
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = ~pred)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = get_colpal()) +
      ggplot2::facet_wrap(~agecl, ncol = 5, labeller = ggplot2::label_wrap_gen(width = 15)) +
      ggplot2::labs(x = "time", y = "relative M2", title = NULL) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(title = paste("Predationmortality excerted upon:", unique(data$pred))) +
      theme_atlantis() +
      ggplot2::theme(legend.position = "right")

    return(plot)
  }

  # plot_func_ts <- function(data) {
  #   plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput)) +
  #     ggplot2::geom_line() +
  #     ggplot2::labs(y = "DietCheck absolute") +
  #     theme_atlantis() +
  #     ggplot2::coord_cartesian(expand = FALSE)
  # }

  plots_pred <- lapply(split(pred, pred$pred), plot_func)
  plots_prey <- lapply(split(prey, prey$prey), plot_func2)
  # plots_ts <- lapply(split(ts_diet, ts_diet$pred), plot_func_ts)

  grobs <- mapply(gridExtra::arrangeGrob, plots_diet, plots_ts, MoreArgs = list(heights = grid::unit(c(0.7, 0.3), units = "npc")))

  return(grobs)
}


# df <- load_txt(dir = "Z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS", file = "outputNorthSeaSpecificPredMort.txt")
#
# preds <- c("COD")
#
# ggplot2::ggplot(data = subset(mort, is.element(pred, preds)), ggplot2::aes(x = time, y = prey, fill = atoutput)) +
#   ggplot2::geom_tile() +
#   ggplot2::scale_fill_gradientn(colours = rainbow(7), name = "Diet [%]") +
#   ggplot2::facet_wrap(~ pred + agecl, ncol = 10)


