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
#' gridExtra::grid.arrange(plot_species(preprocess_setas, species = "Shallow piscivorous fish"))

plot_species <- function(data_pre, species) {
  select_species <- function(df, species) {
    if (any(names(df) %in% "species")) {
      df <- df[df$species == species, ]
      return(df)
    }
  }

  change_theme <- function(p) {
    p + ggplot2::theme(legend.position = "none",
                       x.axis.title = element_blank())
    return(p)
  }

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
  plots <- list(p1, p2, p3, p4)
  plots <- lapply(plots, change_theme)
  grob <- gridExtra::arrangeGrob(p1, p2, p3, p4, nrow = 2)

  return(grob)
}


