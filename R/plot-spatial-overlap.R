#' Plot spatial overlap.
#'
#' @param df_list List of dataframes generated with \code{\link{calculate_spatial_overlap}}
#' @return ggplot2 plot.
#' @export
#'
#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' nc_gen <- "outputNorthSea.nc"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' prm_run = "NorthSea_run_fishing_F.prm"
#' bps = load_bps(dir, fgs = "functionalGroups.csv", init = "init_simple_NorthSea.nc")
#' fgs = "functionalGroups.csv"
#' bboxes = get_boundary(boxinfo = load_box(dir, bgm = "NorthSea.bgm"))
#'
#' biomass_spatial <- calculate_biomass_spatial(dir, nc_gen, prm_biol, prm_run, bps, fgs, bboxes)
#' dietmatrix <- load_dietmatrix(dir, prm_biol, fgs, convert_names = TRUE)
#' agemat <- prm_to_df(dir = dir, prm_biol = prm_biol, fgs = fgs,
#'                     group = get_age_acronyms(dir = dir, fgs = fgs),
#'                     parameter = "age_mat")
#'
#' sp_overlap <- calculate_spatial_overlap(biomass_spatial, dietmatrix, agemat)
#' plot_spatial_overlap(sp_overlap)

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
