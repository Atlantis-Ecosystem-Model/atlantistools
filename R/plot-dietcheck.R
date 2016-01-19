#' Plot contribution of diet contents for each functional group.
#'
#' @param data Dataframe with information about diets. The dataframe
#' should be generated with \seealso{load_dietcheck}.
#' @return List of ggplot2 objects.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' plot_diet(dir = d, dietcheck = "outputSETASDietCheck.txt")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
data <- load_dietcheck(dir = d,
    dietcheck = "outputSETASDietCheck.txt",
    fgs = "functionalGroups.csv",
    prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
    modelstart = "1991-01-01")

plot_dietcheck <- function(data) {
  plot_func <- function(data) {
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~diet, fill = ~prey)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = col_pal) +
      ggplot2::facet_grid(. ~ agecl) +
      ggplot2::labs(x = "time", y = "contribution to diet [%]", title = NULL) +
      theme_atlantis() +
      ggplot2::theme(legend.position = "right")
    return(plot)
  }

  steps <- split(data, data$pred)
  steps <- lapply(steps, drop.levels)
  plots <- lapply(steps, plot_func)
  return(plots)
}


ggplot2::ggplot(subset(data, time == min(time)), ggplot2::aes(x = pred, y = prey, fill = diet)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradientn(colours = rainbow(7), name = "Diet [%]"
                       #, breaks = c(0.25,0.5,0.75,1), labels = c(.25,.5,.75,1)
  )
  # facet_grid(Subdomain ~ ., scales = "free", space = "free") +

plots <- plot_dietcheck(data = data)

dummy[[2]] <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = m2_perc, fill = prey)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_fill_manual(values = col_pal) +
  ggplot2::labs(x = "time [years]", y = "M2 [relative]", title = NULL)

plot_func(data = subset(data, pred == "Shallow piscivourus fish"))

df <- data.frame(v=c(1,2,3),f=c('a','b','c'))
df$f <- factor(df$f)



flips <- factor(c(0,1,1,0,0,1), levels=c(0,1), labels=c("Shallow piscivourus fish", "Heads"))


