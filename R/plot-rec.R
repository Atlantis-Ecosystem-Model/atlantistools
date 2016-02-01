#' Plot recruitment.
#'
#' @param data Dataframe with information about ssb and recruits.
#' This is created from atlantis output files YOY.txt and SSB.txt
#' (Usually output[...]YOY.txt' & 'output[...]SSB.txt') using
#' \code{\link{load_rec}}.
#' @param ex_data Dataframe to compare the atlantis run with.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' data <- load_rec(dir = d,
#'    yoy = "outputSETASYOY.txt",
#'    ssb = "outputSETASSSB.txt",
#'    fgs = "SETasGroups.csv",
#'    prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    modelstart = "1991-01-01")
#'
#' ex_data <- data
#' ex_data$species <- as.character(ex_data$species)
#' ex_data$rec <- ex_data$rec * runif(n = nrow(ex_data), min = 0.8, max = 1.2)
#' ex_data$ssb <- ex_data$ssb * runif(n = nrow(ex_data), min = 0.8, max = 1.2)
#' ex_data$model <- "test_model"
#' plot_rec(data, ex_data)

plot_rec <- function(data, ex_data) {
  ex_data$time <- as.Date(x = ex_data$time, format = "%Y-%m-%d")
  ex_data$species <- factor(ex_data$species, levels = levels(data$species))
  ex_data$time <- NA

  data$model <- "atlantis"
  comp <- rbind(ex_data, data)

  # Atlantis as first factor level!
  comp$model <- factor(comp$model, levels = c("atlantis", sort(unique(comp$model))[sort(unique(comp$model)) != "atlantis"]))

  plot <- ggplot2::ggplot(data = comp, ggplot2::aes_(x = ~ssb, y = ~rec, shape = ~model, colour = ~time)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~species, ncol = 8, scale = "free") +
    ggplot2::scale_colour_gradientn(colours = rainbow(7)) +
    ggplot2::labs(x = "SSB [tonnes]", y = "Recruits [thousands]") +
    theme_atlantis()

  return(plot)
}
