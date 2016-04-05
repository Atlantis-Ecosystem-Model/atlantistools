#' Plot mortality per group and ageclass.
#'
#' @param data Dataframe with information about specific mortality The dataframe
#' should be generated with \code{\link{preproces()}}. Normally this is stored as
#' \code{peprocess$spec_mort}.
#' @return ggplot2 plot
#' @export
#'
#' @examples
#' plot_mort(pre)


dir <- "c:/backup_c/ATLANTIS_output/1237_v.13.2.1_ATLANTIS_NS/"

plot_mort <- function(data) {
  mort <- load_txt(dir = dir, file = "outputNorthSeaSpecificMort.txt")
  mort <- tidyr::separate_(mort, col = "code", into = c("species", "agecl", "notsure", "mort"), convert = TRUE)

  if (length(unique(mort$notsure)) == 1) mort$notsure <- NULL

  mort$agecl <- mort$agecl + 1

  mort <- mort[mort$time != 0, ]

  convert_factor()

  ggplot2::ggplot(subset(mort, species == "SPR"), ggplot2::aes(x = factor(agecl), y = atoutput, colour = mort)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ species, scale = "free_y") +
    theme_atlantis()

}
