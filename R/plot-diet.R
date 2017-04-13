#' Plot contribution of diet contents for each functional group.
#'
#' Visualise diet proportions form predator and prey perspective. The upper panel
#' plot shows the predator perspective while the lower panel plot shows the prey perspective
#' for a given group. Please note that this function only works with models
#' based on the trunk code. Bec_dev models should use \code{\link{plot_diet_bec_dev}} to get an indication
#' of the feeding interactions.
#'
#' @param bio_consumed Consumed biomass of prey groups by predatorgroup and agecl in tonnes
#' for each timestep and polygon. Dataframe with columns 'pred', 'agecl', 'polygon', 'time', 'prey'.
#' Consumed biomass in [t] is stored in column 'atoutput'. Should be generated with
#' \code{link{calculate_consumed_biomass}}.
#' @param species Character string giving the acronyms of the species you aim to plot. Default is
#' \code{NULL} resulting in all available species being ploted.
#' @param wrap_col Character specifying the column of the dataframe to be used as multipanel plot.
#' Default is \code{"agecl"}.
#' @param combine_thresh Number of different categories to plot. Lets say predator X has eaten
#' 20 different prey items. If you only want to show the 3 most important prey items set
#' \code{combine_thresh} to 3. As rule of thumb values < 10 are useful otherwise to many
#' colors are used in the plots. Default is \code{7}.
#' @return List of grobs composed of ggplot2 objects.
#' @export
#' @family plot functions
#'
#' @examples
#' plots <- plot_diet(ref_bio_cons, wrap_col = "agecl")
#' gridExtra::grid.arrange(plots[[1]])
#' gridExtra::grid.arrange(plots[[7]])
#'
#' # Use names() to get the species names!
#' names(plots)

plot_diet <- function(bio_consumed, species = NULL, wrap_col = "agecl", combine_thresh = 7) {
  # Check input dataframe structure.
  check_df_names(data = bio_consumed, expect = c("pred", "agecl", "time", "prey", "atoutput"), optional = "polygon")

  # Calculate % diet dirstribution from pred and prey perspective.
  agg_bio <- agg_data(bio_consumed, groups = c("time", "pred", "agecl", "prey"), fun = sum)
  preddata <- agg_perc(agg_bio, groups = c("time", "pred", "agecl"))
  preydata <- agg_perc(agg_bio, groups = c("time", "prey", "agecl"))

  # Combine groups for both dataframes!
  pred_comb <- combine_groups(preddata, group_col = "prey", groups = c("time", "pred", "agecl"), combine_thresh = combine_thresh)
  prey_comb <- combine_groups(preydata, group_col = "pred", groups = c("time", "prey", "agecl"), combine_thresh = combine_thresh)

  # Species specific ploting routine!
  plot_sp <- function(data, col, wrap_col) {
    # create empty plot in case no data was passed!
    if (nrow(data) == 0) {
      plot <- ggplot2::ggplot() + ggplot2::theme_void()
    } else {

      # order data according to dietcontribution
      agg_data <- agg_data(data, groups = col, out = "sum_diet", fun = sum)
      data[, col] <- factor(data[[col]], levels = agg_data[[1]][order(agg_data$sum_diet, decreasing = TRUE)])

      plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = lazyeval::interp(~var, var = as.name(col)))) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = get_colpal()) +
        ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(wrap_col)),
                            ncol = 5, labeller = "label_both") +
        ggplot2::labs(x = NULL, y = NULL, title = NULL) +
        theme_atlantis() +
        ggplot2::theme(legend.position = "right")
      plot <- ggplot_custom(plot)
    }

    return(plot)
  }

  # Select all available species if none have been selected! This is a bit hacky but it works...
  if (is.null(species)) species <- sort(union(union(union(preddata$pred, preddata$prey), preydata$pred), preydata$prey))
  grobs <- vector("list", length = length(species))
  for (i in seq_along(grobs)) {
    grobs[[i]] <- vector("list", length = 2)
  }

  for (i in seq_along(species)) {
    # Select species!
    df_pred <- dplyr::filter_(pred_comb, ~pred == species[i])
    df_prey <- dplyr::filter_(prey_comb, ~prey == species[i])
    grobs[[i]][[1]] <- plot_sp(df_pred, col = "prey", wrap_col = wrap_col)
    grobs[[i]][[2]] <- plot_sp(df_prey, col = "pred", wrap_col = wrap_col)
  }

  # Convert to 3x1 grob.
  for (i in seq_along(grobs)) {
    heading <- grid::textGrob(paste("Diet proportions for species:", species[i]), gp = grid::gpar(fontsize = 14))
    grobs[[i]][[1]] <- grobs[[i]][[1]] + ggplot2::labs(y = "Predator perspective")
    grobs[[i]][[2]] <- grobs[[i]][[2]] + ggplot2::labs(y = "Prey perspective")
    grobs[[i]] <- gridExtra::arrangeGrob(grobs = c(list(heading), grobs[[i]]),
                                         heights = grid::unit(c(0.05, 0.475, 0.475), units = "npc"))
  }

  names(grobs) <- species
  return(grobs)
}

