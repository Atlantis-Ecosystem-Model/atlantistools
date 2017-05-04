#' Plot spatial overlap.
#'
#' @param df_list List of dataframes generated with \code{\link{calculate_spatial_overlap}}
#' @return ggplot2 plot.
#' @export
#'
#' @examples
#' sp_overlap <- calculate_spatial_overlap(ref_bio_sp, ref_dietmatrix, ref_agemat)
#' \dontrun{
#' plot_spatial_overlap(sp_overlap)
#' }

plot_spatial_overlap <- function(df_list) {
  # combine lists to dataframe!
  combine_list <- function(list, index) {
    my_list <- lapply(list, function(x) x[[index]])
    dplyr::bind_rows(my_list)
  }

  si_spec <- combine_list(df_list, 1)
  si_overall <- combine_list(df_list, 2)

  plot <- ggplot2::ggplot(si_spec, ggplot2::aes_(x = ~time, y = ~si, group = ~time)) +
    ggplot2::geom_violin() +
    ggplot2::geom_point(data = si_overall, colour = "red") +
    ggplot2::facet_grid(agecl_pred ~ pred)

  return(plot)
}
