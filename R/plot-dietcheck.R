#' Plot contribution of diet contents for each functional group.
#'
#' @param data Dataframe with information about diets. The dataframe
#' should be generated with \code{\link{load_dietcheck}}.
#' @param combine_thresh Integer indicating minimum amount to the stomach contribution.
#' Each prey item with a lower contribution as this treshold is assigned to the
#' grpup "Rest". This is necessary for species with a wide variety of food items
#' in their diet. Otherwise the screen will be cluttered with colors in the
#' dietplots. Default is 0.03.
#' @return List of ggplot2 objects.
#' @export
#'
#' @examples
#' plots <- plot_dietcheck(data = preprocess_setas$diet, combine_thresh = 0.03)
#' gridExtra::grid.arrange(plots[[1]])

plot_dietcheck <- function(data, combine_thresh = 0.03) {
  check_df_names(data = data, expect = c("time", "diet", "atoutput", "prey", "pred"), optional = c("habitat", "agecl"))

  # Timeseries per species of absolute values!
  ts_diet <- agg_data(data, groups = c("time", "pred", "habitat"), fun = sum)

  # Combine prey groups with low contribution to the diet!
  data <- combine_groups(data,
                         group_col = "prey",
                         groups = c("time", "pred", "habitat"),
                         combine_thresh = combine_thresh)

  # Convert diet data to percentages!
  data <- agg_perc(data, groups = c("time", "pred", "habitat"))

  # data$prey[data$atoutput <= combine_thresh] <- "Rest"
  # data <- agg_sum(data, groups = c("time", "pred", "habitat", "prey"))

  plot_func <- function(data) {
    # order data according to dietcontribution
    agg_data <- agg_data(data, groups = "prey", out = "sum_diet", fun = sum)
    data$prey <- factor(data$prey, levels = agg_data$prey[order(agg_data$sum_diet, decreasing = TRUE)])
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = ~prey)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = get_colpal()) +
      ggplot2::labs(x = "time", y = "contribution to diet [%]", title = NULL) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(title = paste("Predator:", unique(data$pred))) +
      theme_atlantis() +
      ggplot2::theme(legend.position = "right")

    # Only used with trunc models!
    if (is.element("agecl", names(data))) {
      if (length(unique(data$agecl)) > 1) {
        plot <- plot + ggplot2::facet_wrap(~agecl, ncol = 5, labeller = ggplot2::label_wrap_gen(width = 15))
      }
    }

    # This is repetitive... who cares...
    if (is.element("habitat", names(data))) {
      if (length(unique(data$habitat)) > 1) {
        plot <- plot + ggplot2::facet_wrap(~habitat, ncol = 4, labeller = ggplot2::label_wrap_gen(width = 15))
      }
    }

    return(plot)
  }

  plot_func_ts <- function(data) {
    plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~time, y = ~atoutput)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "DietCheck absolute") +
      theme_atlantis() +
      ggplot2::coord_cartesian(expand = FALSE)
  }

  plots_diet <- lapply(split(data, data$pred), plot_func)
  plots_ts <- lapply(split(ts_diet, ts_diet$pred), plot_func_ts)

  grobs <- mapply(gridExtra::arrangeGrob, plots_diet, plots_ts, MoreArgs = list(heights = grid::unit(c(0.7, 0.3), units = "npc")))

  return(grobs)
}

# load("preprocess-north-sea.rda")
# data <- result$diet
# # Combine prey groups with low contribution to the diet!
# data <- combine_groups(data,
#                        group_col = "prey",
#                        groups = c("time", "pred", "habitat"),
#                        combine_thresh = 0.00)
#
# # Convert diet data to percentages!
# data <- agg_perc(data, groups = c("time", "pred", "habitat"))
#
# ggplot2::ggplot(data = subset(data, habitat == "WC"), ggplot2::aes(x = time, y = prey, fill = atoutput)) +
#   ggplot2::geom_tile() +
#   ggplot2::scale_fill_gradientn(colours = rainbow(7), name = "Diet [%]") +
#   ggplot2::facet_wrap(~pred, ncol = 9)
#
#
# plots <- plot_dietcheck(data = dietns)
#
# ggplot2::ggplot(data = subset(dietns, agecl == 1), ggplot2::aes(x = time, y = prey, fill = diet)) +
#   ggplot2::geom_tile() +
#   ggplot2::scale_fill_gradientn(colours = rainbow(7), name = "Diet [%]"
#                        #, breaks = c(0.25,0.5,0.75,1), labels = c(.25,.5,.75,1)
#   ) +
#   ggplot2::facet_wrap(~pred)
#
# plots <- plot_dietcheck(data = data)
#
# dummy[[2]] <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = m2_perc, fill = prey)) +
#   ggplot2::geom_bar(stat = "identity") +
#   ggplot2::scale_fill_manual(values = col_pal) +
#   ggplot2::labs(x = "time [years]", y = "M2 [relative]", title = NULL)
#
# plot_func(data = subset(data, pred == "Shallow piscivourus fish"))
#
# df <- data.frame(v=c(1,2,3),f=c('a','b','c'))
# df$f <- factor(df$f)
#
#
#
# flips <- factor(c(0,1,1,0,0,1), levels=c(0,1), labels=c("Shallow piscivourus fish", "Heads"))
#
#
