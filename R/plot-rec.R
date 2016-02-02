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
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
data <- load_rec(dir = d,
   yoy = "outputSETASYOY.txt",
   ssb = "outputSETASSSB.txt",
   fgs = "SETasGroups.csv",
   prm_biol = "VMPA_setas_biol_fishing_New.prm",
   prm_run = "VMPA_setas_run_fishing_F_New.prm",
   modelstart = "1991-01-01")

ex_data <- data
ex_data$species <- as.character(ex_data$species)
ex_data$rec <- ex_data$rec * runif(n = nrow(ex_data), min = 0.8, max = 1.2)
ex_data$ssb <- ex_data$ssb * runif(n = nrow(ex_data), min = 0.8, max = 1.2)
ex_data$time <- as.character(ex_data$time)
ex_data$model <- "test_model"
plot_rec(data, ex_data)

plot_rec <- function(data, ex_data) {
  check_df_names(data = data, expect = c("ssb", "rec", "time", "species"))

  ex_data$time <- as.Date(x = ex_data$time, format = "%Y-%m-%d")
  ex_data$species <- factor(ex_data$species, levels = levels(data$species))

  data$model <- "atlantis"
  comp <- rbind(ex_data, data)
  comp$time[comp$model != "atlantis"] <- NA

  # Atlantis as first factor level!
  comp$model <- factor(comp$model, levels = c("atlantis", sort(unique(comp$model))[sort(unique(comp$model)) != "atlantis"]))

  breaks <- c(min(as.numeric(data$time)), max(as.numeric(data$time)))
  labels <- c(min(data$time), max(data$time))

  plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = ~ssb, y = ~rec, shape = ~model, colour = ~as.numeric(time))) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~species, ncol = 8, scale = "free", labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::scale_colour_gradientn("Time", colours = rainbow(7), breaks = breaks, labels = labels) +
    ggplot2::labs(x = "SSB [tonnes]", y = "Recruits [thousands]") +
    theme_atlantis() +
    ggplot2::guides(colour = ggplot2::guide_colorbar(label.theme = ggplot2::element_text(angle = 45, h)))

  return(plot)
}
