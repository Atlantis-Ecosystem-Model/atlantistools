#' Plot contribution of diet contents for each functional group.
#'
#' @param data Dataframe with information about diets. The dataframe
#' should be generated with \code{\link{load_dietcheck}}.
#' @return List of ggplot2 objects.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' diet <- load_dietcheck(dir = d,
#'     dietcheck = "outputSETASDietCheck.txt",
#'     fgs = "functionalGroups.csv",
#'     prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
#'     modelstart = "1991-01-01",
#'     combine_tresh = 0.03)
#' plots <- plot_dietcheck(data = diet)

plot_dietcheck <- function(data) {
  plot_func <- function(data) {
    # order data according to dietcontribution
    agg_data <- data %>%
      dplyr::group_by_(~prey) %>%
      dplyr::summarise_(sum_diet = ~sum(diet))
    data$prey <- factor(data$prey, levels = agg_data$prey[order(agg_data$sum_diet, decreasing = TRUE)])
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~diet, fill = ~prey)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = get_colpal()) +
      ggplot2::labs(x = "time", y = "contribution to diet [%]", title = NULL) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(title = paste("Predator:", unique(data$pred))) +
      theme_atlantis() +
      ggplot2::theme(legend.position = "right")

    if (is.element("agecl", names(data))) {
      if (length(unique(data$agecl)) > 1) {
        plot <- plot + ggplot2::facet_wrap(~agecl, ncol = 5)
      }
    }

    return(plot)
  }

  data_pred <- split(data, data$pred)
  data_pred <- lapply(data_pred, droplevels)
  plots <- lapply(data_pred, plot_func)
  return(plots)
}

# dietns <- load_dietcheck(dir = file.path("Z:", "Atlantis_models", "Runs", "dummy_01_ATLANTIS_NS"),
#                          dietcheck = "outputNorthSeaDietCheck.txt",
#                          fgs = "functionalGroups.csv",
#                          prm_run = "NorthSea_biol_fishing.prm",
#                          modelstart = "1991-01-01",
#                          combine_tresh = 0.03)
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
