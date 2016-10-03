#' Visualise the spatial distribution per species and stanza combination.
#'
#' @param bio_spatial Biomass per group and stanza in tonnes for each timestep,
#' layer and polygon. This dataframe should be generated with
#' \code{\link{calculate_biomass_spatial}}. The columns of the dataframe have to
#' be 'species', 'species_stanza', 'polygon', 'layer', 'time' and 'atoutput'.
#' Column 'atoutput' is the biomass in tonnes. Please use \code{\link{combine_ages}}
#' to transform an agebased dataframe to a stanza based dataframe.
#' @param bgm_as_df *.bgm file converted to a dataframe. Please use \code{\link{convert_bgm}}
#' to convert your bgm-file to a dataframe with columns 'lat', 'long', 'inside_lat',
#' 'inside_long' and 'polygon'.
#' @param timesteps Integer giving the number of timesteps to visualise. The minimum
#' value is 2 (default). By default the start and end of the simulation is shown. In case
#' timesteps > 2 equally spaces timesteps - 2 are added.
#' @return ggplot2 plot
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
#' df_agemat <- prm_to_df(dir = dir, prm_biol = prm_biol, fgs = fgs, group = get_age_acronyms(dir = dir, fgs = fgs),
#'                     parameter = "age_mat")
#'
#' df_sp <- calculate_biomass_spatial(dir, nc_gen, prm_biol, prm_run, bps, fgs, bboxes)
#' bio_spatial <- combine_ages(df_sp, grp_col = "species", agemat = df_agemat)
#' bgm_as_df <- convert_bgm(dir, bgm = "NorthSea.bgm")

plot_spatial <- function(bio_spatial, bgm_as_df, timesteps = 2){
  # Check input dataframe!
  check_df_names(bio_spatial, expect = c("species", "polygon", "layer", "time", "species_stanza", "atoutput"))
  check_df_names(bgm_as_df, expect = c("lat", "long", "inside_lat", "inside_long", "polygon"))

  # Get available species and stanzas!
  pred_stanza <- unique(dplyr::select_(bio_spatial, .dots = c("species", "species_stanza")))

  # Step1: Calculate different summary tables
  # - perc biomass per box and layer
  perc_bio <- agg_perc(bio_spatial, groups = c("time", "layer", "species", "species_stanza"))
  # - biomass timeseries per box
  ts_bio <- agg_data(bio_spatial, groups = c("time", "species", "species_stanza", "polygon"), fun = sum)

  # Plot spatial distribution per species, species_stanza, timesteps and layer!
  # Use layer (y-direction) and timestep (x-direction) to facet_grid
  plot_spatial_species <- function(data) {
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~long, y = ~lat, fill = ~factor(polygon), group = ~factor(polygon), label = ~polygon)) +
      ggplot2::geom_polygon(colour = "black") +
      ggplot2::geom_text(data = inside) +
      theme_atlantis() +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3)) +
      ggplot2::theme(legend.position = "right")
  }



  dfs <- split_dfs(perc_bio, cols = c("species", "species_stanza"))

}

# Utility functions
# Select timesteps
select_time <- function(df, timesteps = 2) {
  select_time <- c(min(df$time), max(df$time))
  if (timesteps > 2) {
    time_sorted <- sort(unique(df$time))
    pos <- 1:(timesteps - 2) * (ceiling(length(time_sorted) / timesteps)) + 1
    select_time <- c(select_time, time_sorted[pos])
    dplyr::filter_(df, ~time %in% select_time)
  }
}

# Setpx: Split data into species / species_stanza categories and apply plotting routines
split_dfs <- function(df, cols) {
  if (any(!cols %in% names(df))) stop("Column names in df do not match with cols.")
  df_cat <- unique(dplyr::select_(df, .dots = cols))
  dfs <- vector(mode = "list", length = nrow(df_cat))
  # Should work much better with filter but the nse-part is a bit tricky...
  for (i in seq_along(dfs)) {
    df_join <- dplyr::slice(df_cat, i)
    dfs[[i]] <- dplyr::inner_join(df, df_join, by = cols)
  }
  return(dfs)
}

