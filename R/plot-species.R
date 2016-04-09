#' Create species specific overvie plot.
#'
#' This plotting routine is based on Raphael's (Ifremer) plotting routune
#' used during model calibration. Currently 4 plots are created by default:
#' - StructN over time per age
#' - ResN over time per age
#' - Condition over time per age
#' - Numbers over time per age
#'
#' @param data_pre List of preprocessed Atlantis simulation. The list of dataframes
#' should be created with \code{preprocess()}.
#' @param species Character srtring giving the name of the species to plot. Only age
#' based species are supported.
#' @return ggplot2 object of class grob
#' @export
#' @family plot functions
#'
#' @examples
#' plot <- plot_species(preprocess_setas, species = "Shallow piscivorous fish")
#' # Use grid.arrange to draw the plot on the current device
#' gridExtra::grid.arrange(plot)

plot_species <- function(data_pre, species) {
  # Plot timeseries of structural and reserve nitrogen
  strn <- select_species(df = data_pre$structn_age, species = species)
  resn <- select_species(df = data_pre$resn_age,    species = species)
  p1 <- plot_ts(strn) %>% ggplot2::update_labels(labels = list(y = "Structural nitrogen [mgN]"))
  p2 <- plot_ts(resn) %>% ggplot2::update_labels(labels = list(y = "Reserve nitrogen [mgN"))

  # Plot condition of species RN/SN
  names(strn)[names(strn) == "atoutput"] <- "strn"
  condition <- dplyr::inner_join(strn, resn)
  condition$atoutput <- condition$atoutput / condition$strn
  p3 <- plot_ts(condition)

  # Plot timeseries of numbers
  p4 <- plot_ts(select_species(df = data_pre$nums_age, species = species))

  # Combine plot to grob!
  plots <- list(p1, p2, p3, p4, p4, p4)
  plots <- lapply(plots, function(x) gridExtra::arrangeGrob(change_theme(x)))
  # plots <- lapply(plots, grid::grob)
  header <- grid::textGrob(species, gp = grid::gpar(fontsize = 18))
  grob <- gridExtra::arrangeGrob(
    grobs = c(list(header), plots, list(header)),
    layout_matrix = matrix(c(rep(1, 2), 2:(length(plots) + 1), rep(length(plots) + 2, 2)), nrow = length(plots) / 2 + 2, byrow = T),
    heights = grid::unit(c(0.05, rep(0.3, 3), 0.05), units = "npc"))

  return(grob)

  # Helper functions -----------------------------------------------------------
  # Subset data based on species name.
  select_species <- function(df, species) {
    if (any(names(df) %in% "species")) {
      df <- df[df$species == species, ]
      return(df)
    }
  }

  # Change specific theme elemets of plots.
  change_theme <- function(p) {
    p <- p + ggplot2::theme(legend.position = "none",
                            axis.title.x = ggplot2::element_blank(),
                            strip.text = ggplot2::element_blank())
    return(p)
  }
}


