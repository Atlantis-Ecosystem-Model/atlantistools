#' Plot recruitment.
#'
#' @param data Dataframe with information about ssb and recruits.
#' This is created from atlantis output files YOY.txt and SSB.txt
#' (Usually output[...]YOY.txt' & 'output[...]SSB.txt') using
#' \code{\link{load_rec}}.
#' @param ex_data Dataframe to compare the atlantis run with.
#' @param ncol Number of columns in multipanel plot. Default is \code{7}.
#' @return ggplot2 object
#' @export
#' @family plot functions
#'
#' @examples
#' \dontrun{
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' ex_data <- read.csv(file.path(d, "setas-ssb-rec.csv"), stringsAsFactors = FALSE)
#' plot_rec(preprocess_setas$ssb_rec, ex_data)
#' }

# d <- file.path("Z:", "Atlantis_models", "Runs", "dummy_01_ATLANTIS_NS")
# ex_data <- read.csv(file.path(d, "ssb_r_ssa_sms.csv"), stringsAsFactors = F)
# load(file.path(d, "preprocess-north-sea.rda"))
# data <- result$ssb_rec


plot_rec <- function(data, ex_data, ncol = 7) {
  check_df_names(data = data, expect = c("ssb", "rec", "time", "species"))

  # ex_data$time <- as.Date(x = ex_data$time, format = "%Y-%m-%d")
  # ex_data$species <- factor(ex_data$species, levels = levels(data$species))

  data$model <- "atlantis"
  comp <- rbind(ex_data, data)
  comp$time[comp$model != "atlantis"] <- NA

  # Atlantis as first factor level!
  comp$model <- factor(comp$model, levels = c("atlantis", sort(unique(comp$model))[sort(unique(comp$model)) != "atlantis"]))

  # Add breaks and labels to the legend. This is really hacky... We need a continous variable for time
  # to map rainbow colors...
  time_numeric <- sort(unique(as.numeric(data$time)))
  time_date <- sort(unique(data$time))
  pos <- c(1, 1:2 * trunc(length(time_numeric)/2), length(time_numeric))

  plot <- ggplot2::ggplot(data = comp, ggplot2::aes_(x = ~ssb, y = ~rec, shape = ~model, colour = ~as.numeric(time))) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~species, ncol = ncol, scales = "free", labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::scale_colour_gradient("Time", low = "red", high = "green" , breaks = time_numeric[pos], labels = time_date[pos]) +
    ggplot2::labs(x = "SSB [tonnes]", y = "Recruits [thousands]") +
    theme_atlantis() +
    ggplot2::guides(colour = ggplot2::guide_colorbar(label.theme = ggplot2::element_text(angle = 45),
                                                     label.hjust = 1, label.vjust = 1, title.vjust = 1))
  plot <- ggplot_custom(plot)

  return(plot)
}

