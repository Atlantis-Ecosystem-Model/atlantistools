#' Plot contribution of diet contents for each functional group.
#'
#' Visualise diet proportions form predator and prey perspective. The upper panel
#' plot shows the predator perspective while the lower panel plot shows the prey perspective
#' for a given group. Please note that this function only works properly with models
#' based on the trunk code. Bec_dev models should use \code{plot_diet_bec_dev} to get an indication
#' of the feeding interactions.
#'
#' @param preddata Diet proportion of prey species in the stomach of a specific
#' predator/ageclass combination per time. This dataframe
#' should be generated with \code{\link{load_dietcheck}} or read in from a preprocessd
#' *.rda generated with \code{\link{preprocess}}.
#' @param preydata Proportion of consumed biomass of a preygroup by different predator/ageclass
#' combinations. This dataframe should be generated with \code{\link{biomass_flow}} with
#' \code{plot_diet} set to \code{TRUE}.
#' @param species Character string giving the acronyms of the species you aim to plot. Default is
#' \code{NULL} resulting in all available species being ploted.
#' @param wrap_col Character specifying the column of the dataframe to be used as multipanel plot.
#' Currently only "agecl" is supported.
#' @param combine_thresh Number of different categories to plot. Lets say predator X has eaten
#' 20 different prey items. If you only want to show the 3 most important prey items set
#' \code{combine_thresh} to 3. As rule of thumb values < 10 are useful otherwise to many
#' colors are used in the plots.
#' @return List of ggplot2 objects.
#' @export
#' @family plot functions
#'
#' @examples
# d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
# df_pred <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", report = FALSE, fgs = "SETasGroupsDem_NoCep.csv")
# df_orey <- biomass_flow(dir = d,
#                         nc_prod = "outputSETASPROD.nc",
#                         nc_gen <- "outputSETAS.nc",
#                         dietcheck = "outputSETASDietCheck.txt",
#                         prm_biol = "VMPA_setas_biol_fishing_Trunk.prm",
#                         prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
#                         bps = load_bps(dir = d, init = "INIT_VMPA_Jan2015.nc", fgs = "SETasGroupsDem_NoCep.csv"),
#                         fgs = "SETasGroupsDem_NoCep.csv",
#                         bboxes = get_boundary(load_box(dir = d, bgm = "VMPA_setas.bgm")),
#                         plot_diet = TRUE)
#
# plots <- plot_diet(preddata = df_pred, preydata = df_prey, wrap_col = "agecl")
# gridExtra::grid.arrange(plots[[1]])

# dir <- "c:/backup_z/Atlantis_models/Runs/dummy_02_ATLANTIS_NS/"
# nc_prod <- "outputNorthSeaPROD.nc"
# nc_gen <- "outputNorthSea.nc"
# dietcheck <- "outputNorthSeaDietCheck.txt"
# prm_biol <- "NorthSea_biol_fishing.prm"
# prm_run <- "NorthSea_run_fishing_F.prm"
# fgs <- "functionalGroups.csv"
# bps <- load_bps(dir = dir, init = "init_NorthSea.nc", fgs = fgs)
# bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
# preddata <- load_dietcheck(dir = dir, dietcheck = dietcheck, fgs = fgs, report = FALSE, version_flag = 2)
# preydata <- biomass_flow(dir, nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes, plot_diet = TRUE)
# wrap_col <- "agecl"
# combine_thresh <- 7
# plots <- plot_diet(preddata, preydata, wrap_col = "agecl")
# gridExtra::grid.arrange(plots[[1]])
# gridExtra::grid.arrange(plots[[2]])
# gridExtra::grid.arrange(plots[[3]])

plot_diet <- function(preddata, preydata, species = NULL, wrap_col, combine_thresh = 7) {
  check_df_names(data = preddata, expect = c("time", "atoutput", "prey", "pred", "agecl"))
  check_df_names(data = preydata, expect = c("time", "atoutput", "prey", "pred", "agecl"))

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

