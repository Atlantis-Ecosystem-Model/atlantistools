#' Plot contribution of diet contents for each functional group.
#'
#' @param data Dataframe with information about diets. The dataframe
#' should be generated with \code{\link{load_dietcheck}}.
#' @param species Character string giving the acronyms of the species you aim to plot. Default is
#' \code{NULL} resulting in all available species being ploted.
#' @param wrap_col Character specifying the column of the dataframe to be used as multipanel plot.
#' In case you aim to plot DietCheck.txt data use "habitat".
#' In case you aim to plot SpecMort.txt data use either "agecl" or "stanza".
#' @param combine_thresh Integer indicating minimum amount to the stomach contribution.
#' Each prey item with a lower contribution as this treshold is assigned to the
#' grpup "Rest". This is necessary for species with a wide variety of food items
#' in their diet. Otherwise the screen will be cluttered with colors in the
#' dietplots. Default is 0.03.
#' @return List of ggplot2 objects.
#' @export
#'
#' @examples
#' # Plot DietCheck.txt
#' plots <- plot_diet(preprocess_setas$diet_dietcheck, wrap_col = "habitat")
#' gridExtra::grid.arrange(plots[[1]])
#'
#' # Plot SpecMort.txt per stanza First we need to transform the ageclasses to stanzas.
#' \dontrun{
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' diet_stanza <- combine_ages(dir = d,
#'                             data = preprocess_setas$diet_specmort,
#'                             col = "pred",
#'                             prm_biol = "VMPA_setas_biol_fishing_New.prm")
#' plots <- plot_diet(diet_stanza, wrap_col = "stanza")
#' gridExtra::grid.arrange(plots[[1]])
#' }
#'
#' # Plot SpecMort.txt per ageclass.
#' plots <- plot_diet(preprocess_setas$diet_specmort, wrap_col = "agecl")
#' gridExtra::grid.arrange(plots[[1]])
#'
#' # Only plot specific species
#' plots <- plot_diet(preprocess_setas$diet_specmort, species = "CEP", wrap_col = "agecl")
#' gridExtra::grid.arrange(plots[[1]])


plot_diet <- function(data, species = NULL, wrap_col, combine_thresh = 15) {
  check_df_names(data = data, expect = c("time", "atoutput", "prey", "pred"), optional = c("habitat", "agecl", "stanza"))

  # group_cols <- names(data)[!is.element(names(data), c("pred", "time", "atoutput"))]
  # data <- combine_groups(data, group_col = "pred", groups = group_cols, combine_thresh = combine_thresh)

  # Species specific ploting routine!
  plot_sp <- function(data, col, wrap_col) {
    # create empty plot in case no data was passed!
    if (nrow(data) == 0) {
      plot <- ggplot2::ggplot() + ggplot2::theme_void()
    } else {

      # Combine groups with low contribution!
      # data <- combine_groups(data, group_col = col, groups = group_cols, combine_thresh = combine_thresh)
      #
      # # Convert to percentages!
      # data <- agg_perc(data, groups = group_cols)

      # order data according to dietcontribution
      agg_data <- agg_data(data, groups = col, out = "sum_diet", fun = sum)
      data[, col] <- factor(data[[col]], levels = agg_data[[1]][order(agg_data$sum_diet, decreasing = TRUE)])

      plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, fill = lazyeval::interp(~var, var = as.name(col)))) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = get_colpal()) +
        ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(wrap_col)),
                            ncol = 5, labeller = "label_both") +
        ggplot2::labs(x = NULL, y = NULL, title = NULL) +
        ggplot2::coord_cartesian(expand = FALSE) +
        theme_atlantis() +
        ggplot2::theme(legend.position = "right")
    }

  return(plot)
  }

  # Select all available species if none have been selected!
  if (is.null(species)) species <- sort(union(data$pred, data$prey))
  grobs <- vector("list", length = length(species))
  for (i in seq_along(species)) {
    subgrobs <- list()
    specs <- c("pred", "prey")
    for (j in seq_along(specs)){
      df <- data[data[, specs[j]] == species[i], ]
      if (nrow(df) > 0) {
        df <- combine_groups(df, group_col = specs[specs != specs[j]], combine_thresh = combine_thresh)
        df <- agg_perc(df, groups = c(wrap_col, c("time", "pred", "prey")))
      }
      subgrobs[[j + 1]] <- gridExtra::arrangeGrob(plot_sp(df, col = specs[specs != specs[j]], wrap_col = wrap_col))
    }
    subgrobs[[1]] <- grid::textGrob(paste("Indication of feeding interaction:", species[i]), gp = grid::gpar(fontsize = 18))
    grobs[[i]] <- gridExtra::arrangeGrob(grobs = subgrobs,
                                         heights = grid::unit(c(0.05, 0.475, 0.475), units = "npc"))
    # as_pred <- data[data$pred == species[i], ]
    # as_pred <- combine_groups(as_pred, group_col = "prey", groups = group_cols, combine_thresh = combine_thresh)
    # as_pred <- plot_sp(as_pred, col = "prey", wrap_col = wrap_col)
    # as_pred <- as_pred + ggplot2::labs(y = "Predator perspective")

    # as_prey <- data[data$prey == species[i], ]
    # as_prey <- combine_groups(as_prey, group_col = "pred", groups = group_cols, combine_thresh = combine_thresh)
    # as_prey <- plot_sp(data = data[data$prey == species[i], ], col = "pred", wrap_col = wrap_col)
    # as_prey <- as_prey + ggplot2::labs(y = "Prey perspective")
    # grobs[[i]] <- gridExtra::arrangeGrob(heading, as_pred, as_prey,
    #                                      heights = grid::unit(c(0.05, 0.475, 0.475), units = "npc"))
  }

  names(grobs) <- species
  return(grobs)
}

