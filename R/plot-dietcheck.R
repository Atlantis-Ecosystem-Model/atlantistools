#' Plot contribution of diet contents for each functional group.
#'
#' @param dir Character string giving the path of the Atlantis output folder.
#' @param dietcheck Character string of the DietCheck.txt file. Usually
#' 'output[...]DietCheck.txt'.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' plot_diet(dir = d, dietcheck = "outputSETASDietCheck.txt")

plot_diet <- function(dir = getwd(), dietcheck) {


  plot <- ggplot2::ggplot(diet, ggplot2::aes_(x = time, y = m2_perc, fill = prey)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = col_pal) +
    ggplot2::labs(x = "time [years]", y = "M2 [relative]", title = NULL)


  return(plot)
}
