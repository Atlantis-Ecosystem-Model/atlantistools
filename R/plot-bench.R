#' Compare Atlantis scenario with other models/data sources.
#'
#' @param data Dataframe to be plotted.
#' @param ex_data Dataframe to compare the atlantis run with.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' ex_data <- read.csv(file.path(d, "setas-bench.csv"))
#' plot_bench(data = preprocess_setas$biomass, ex_data)

plot_bench <- function(data, ex_data) {
  check_df_names(data = data, expect = c("time", "atoutput", "species"))

  names(ex_data)[names(ex_data) == "biomass"] <- "atoutput"
  ex_data$time <- as.Date(x = ex_data$time, format = "%Y-%m-%d")

  data$model <- "atlantis"
  comp <- rbind(ex_data, data)

  comp$model <- factor(comp$model, levels = c("atlantis", sort(unique(comp$model))[sort(unique(comp$model)) != "atlantis"]))
  model_time <- comp %>%
    dplyr::group_by_(~model) %>%
    dplyr::summarise_(time = ~max(time))
  max_time <- min(model_time$time)

  comp <- comp[comp$time <= max_time, ]

  plot <- ggplot2::ggplot(comp, ggplot2::aes_(x = ~time, y = ~atoutput, colour = ~model)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~species, scales = "free_y", ncol = 9, labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(y = "Biomass in [t]") +
    theme_atlantis()
  plot <- ggplot_custom(plot)

  return(plot)
}
