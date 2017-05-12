#' Create species specific overview plot.
#'
#' This plotting routine is based on Raphael's (Ifremer) plotting routine
#' used during model calibration. Currently 6 plots are created by default:
#' - Biomass over time
#' - Biomass over time per age
#' - StructN over time per age
#' - ResN over time per age
#' - Condition over time per age
#' - Numbers over time per age
#'
#' @param data_pre List of preprocessed Atlantis simulation. The list of dataframes
#' should be created with \code{model-preprocess.Rmd}.
#' @param species Character srtring giving the name of the species to plot. Only age
#' based species are supported.
#' @return ggplot2 object of class grob
#' @export
#' @family plot functions
#'
#' @examples
#' plot <- plot_species(preprocess, species = "Shallow piscivorous fish")
#' # Use grid.arrange to draw the plot on the current device
#' gridExtra::grid.arrange(plot)

plot_species <- function(data_pre, species) {
  # Helper functions -------------------------------------------------------------------------------
  # Subset data based on species name.
  select_species <- function(df, species) {
    if (any(names(df) %in% "species")) {
      df <- df[df$species == species, ]
      return(df)
    }
  }

  # Main function code -----------------------------------------------------------------------------
  # Extract dfs from the list of preprocessed dataframes and perform some simple transformations!
  dfs <- list(data_pre$biomass, data_pre$biomass_age, data_pre$resn_age, data_pre$structn_age, data_pre$nums_age)
  dfs <- lapply(dfs, select_species, species = species)
  condition <- dplyr::inner_join(dfs[[3]], dfs[[4]], by = names(dfs[[4]])[!names(dfs[[4]]) %in% "atoutput"])
  condition$atoutput <- condition$atoutput.x / condition$atoutput.y
  condition$atoutput.x <- NULL
  condition$atoutput.y <- NULL
  dfs <- c(dfs, list(condition))

  # Create plots
  plot1 <- plot_line(dfs[[1]])
  plot2 <- lapply(dfs[2:length(dfs)], plot_line, col = "agecl")
  plots <- c(list(plot1), plot2)

  # Update y axis labels!
  labels <- c("Biomass[t]", "Biomass [t]",
              "Reserve nitrogen [mgN]", "Structural nitrogen [mgN]",
              "Numbers", "Condition (optimal = 2.65)")
  # Could be done with Maps(), however I dont know how to pass a assignment as parameter.
  for (i in seq_along(plots)) {
    plots[[i]] <- ggplot2::update_labels(plots[[i]], list(y = labels[i]))
  }

  # Extract legend from age-structutred plot! Create heading.
  g <- ggplot2::ggplotGrob(plots[[2]])$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  header <- grid::textGrob(species, gp = grid::gpar(fontsize = 18))

  # Remove theme elements and convert to grobs! Combine to single grob.
  plots <- lapply(plots, function(x) gridExtra::arrangeGrob(change_theme(x)))
  grob <- gridExtra::arrangeGrob(
    grobs = c(list(header), plots, list(legend)),
    layout_matrix = matrix(c(rep(1, 2), 2:(length(plots) + 1), rep(length(plots) + 2, 2)), nrow = length(plots) / 2 + 2, byrow = T),
    heights = grid::unit(c(0.05, rep(0.3, 3), 0.05), units = "npc"))

  return(grob)
}





