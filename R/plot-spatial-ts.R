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
#' @param vol Volume per polygon and timestep. See model-preprocess.Rmd for details.
#' @param select_species Character vector listing the species to plot. If no species are selected
#' \code{NULL} (default) all available species are plotted.
#' @param ncol Number of columns in final plot. Default is \code{7}.
#' @param polygon_overview numeric value between 0 and 1 indicating the size used to plot the polygon overview in the
#' upper right corner of the plot. Default is \code{0.2}.
#' @return grob of 3 ggplot2 plots.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#'
#' bgm_as_df <- convert_bgm(file.path(d, "VMPA_setas.bgm"))
#' vol <- agg_data(ref_vol, groups = c("time", "polygon"), fun = sum, out = "volume")
#'
#' # Spatial distribution in Atlantis is based on adu- and juv stanzas.
#' # Therefore, we need to aggregate the age-based biomass to
#' # stanzas with \code{\link{combine_ages}}.
#' bio_spatial <- combine_ages(ref_bio_sp, grp_col = "species", agemat = ref_agemat)
#'
#' # Apply \code{\link{plot_spatial_ts}}
#' grobs <- plot_spatial_ts(bio_spatial, bgm_as_df, vol)
#' gridExtra::grid.arrange(grobs[[1]])
#' gridExtra::grid.arrange(grobs[[7]])

# check plot layout
# load("c:/ATLANTIS_output/1582_v.19.4.1_ATLANTIS_NS/preprocess-north-sea.rda", verbose = TRUE)
# bgm_as_df <- convert_bgm(dir = "c:/ATLANTIS_Stuff/Baseruns/1559_v.18.0.0_ATLANTIS_NS/", bgm = "NorthSea.bgm")
# df <- subset(result$biomass_spatial_stanza, species == "Sprat" & species_stanza == 1 & polygon == 14)
# bio_spatial <- df
# vol <- result$vol
#
#
# g <- plot_spatial_ts(df, bgm_as_df, result$vol)
# gridExtra::grid.arrange(g[[1]])
#
# names(result)

plot_spatial_ts <- function(bio_spatial, bgm_as_df, vol, select_species = NULL, ncol = 7, polygon_overview = 0.2) {
  # Check input dataframe!
  check_df_names(bio_spatial, expect = c("species", "polygon", "layer", "time", "species_stanza", "atoutput"))
  check_df_names(bgm_as_df, expect = c("lat", "long", "inside_lat", "inside_long", "polygon"))

  # Flip layers in bio_spatial!
  bio_spatial <- flip_layers(bio_spatial)

  # Filter by species if select_species not NULL!
  # Warning: Will change input parameter which makes it harder to debug...
  if (!is.null(select_species)) {
    if (all(select_species %in% unique(bio_spatial$species))) {
      bio_spatial <- dplyr::filter_(bio_spatial, ~species %in% select_species)
    } else {
      stop("Not all selected_species are present in bio_spatial.")
    }
  } else {
    select_species <- sort(unique(bio_spatial$species))
  }

  # Step1: Calculate summary tables
  # - biomass timeseries per box
  ts_bio <- agg_data(bio_spatial, groups = c("time", "species", "species_stanza", "polygon"), fun = sum) %>%
    dplyr::left_join(vol) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput / volume), "density"))

  plot_ts_species <- function(data, ncol) {
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~density, colour = ~atoutput)) +
      ggplot2::geom_line(lineend = "round") +
      ggplot2::facet_wrap(~polygon, ncol = ncol, labeller = ggplot2::label_wrap_gen(width = 15)) +
      # ggplot2::scale_y_continuous(breaks = function(x) c(min(x), max(x)), labels = function(x) scales::scientific(x, digits = 2)) +
      ggplot2::scale_colour_gradientn(paste("Biomass [t]", sep = "\n"), colours = grDevices::rainbow(n = 7),
                                      labels = function(x) scales::scientific(x, digits = 2)) +
      ggplot2::labs(x = "Time [years]", y = "Biomassdensity [t/m^-3]") +
      theme_atlantis() +
      ggplot2::theme(legend.position = "right") +
      ggplot2::labs(title = paste("Species:", unique(data$species), "with stanza:", unique(data$species_stanza)))

    plot <- ggplot_custom(plot)

    return(plot)
  }

  # Step2: Apply predator and stanza specific plot function
  dfs_species <- split_dfs(df = ts_bio, cols = "species")
  dfs_species <- dfs_species[select_species]
  grobs <- vector(mode = "list", length = length(select_species))
  for (i in seq_along(grobs)) {
    # Divide species specific data in stanza specific data
    dfs <- split(dfs_species[[i]], dfs_species[[i]]$species_stanza)
    plots <- lapply(dfs, plot_ts_species, ncol = ncol)
    grobs[[i]] <- gridExtra::arrangeGrob(grobs = plots, ncol = 1, heights = grid::unit(rep(0.5, 2), units = "npc"))
  }

  # Step3: Combine plots with polygon overview!
  grobs <- lapply(grobs, plot_add_polygon_overview, bgm_as_df = bgm_as_df, polygon_overview = polygon_overview)
  names(grobs) <- select_species

  return(grobs)
}


