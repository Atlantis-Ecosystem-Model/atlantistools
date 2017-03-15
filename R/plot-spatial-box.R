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
#' @param select_species Character vector listing the species to plot. If no species are selected
#' \code{NULL} (default) all available species are plotted.
#' @param timesteps Integer giving the number of timesteps to visualise. Default is \code{2}.
#' By default the start and end of the simulation is shown. In case
#' timesteps > 2 equally spaced timesteps are added.
#' @param polygon_overview numeric value between 0 and 1 indicating the size used to plot the polygon overview in the
#' upper right corner of the plot.  Default is \code{0.2}.
#' @return grob of 3 ggplot2 plots.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#'
#' bgm_as_df <- convert_bgm(file.path(d, "VMPA_setas.bgm"))
#'
#' # Spatial distribution in Atlantis is based on adu- and juv stanzas.
#' # Therefore, we need to aggregate the age-based biomass to
#' # stanzas with \code{\link{combine_ages}}.
#' bio_spatial <- combine_ages(ref_bio_sp, grp_col = "species", agemat = ref_agemat)
#'
#' # Apply \code{\link{plot_spatial_box}}
#' grobs <- plot_spatial_box(bio_spatial, bgm_as_df, timesteps = 3)
#' gridExtra::grid.arrange(grobs[[1]])
#' gridExtra::grid.arrange(grobs[[9]])
#'
#' # use names() to select specific plots
#' names(grobs)
#'
#' # Plot specific species
#' grobs <- plot_spatial_box(bio_spatial, bgm_as_df,
#'                           select_species = "Shallow piscivorous fish", timesteps = 3)
#' gridExtra::grid.arrange(grobs[[1]])
#' gridExtra::grid.arrange(grobs[[2]])

plot_spatial_box <- function(bio_spatial, bgm_as_df, select_species = NULL, timesteps = 2, polygon_overview = 0.2){
  # Check input dataframe!
  check_df_names(bio_spatial, expect = c("species", "polygon", "layer", "time", "species_stanza", "atoutput"))
  check_df_names(bgm_as_df, expect = c("lat", "long", "inside_lat", "inside_long", "polygon"))

  # Flip layers in bio_spatial!
  bio_spatial <- flip_layers(bio_spatial)

  # Create dataframe with all polygon + layer combinations.
  full_grid <- expand.grid(polygon = unique(bgm_as_df$polygon), layer = min(bio_spatial$layer):max(bio_spatial$layer))
  full_grid <- dplyr::left_join(full_grid, bgm_as_df)

  # Filter by species if select_species not NULL!
  # Warning: Will change input parameter which makes it harder to debug...
  if (!is.null(select_species)) {
    if (all(select_species %in% unique(bio_spatial$species))) {
      bio_spatial <- dplyr::filter_(bio_spatial, ~species %in% select_species)
    } else {
      stop("Not all selected_species are present in bio_spatial.")
    }
  }

  # Step1: Calculate summary table
  # - perc biomass per box and layer
  perc_bio <- agg_perc(bio_spatial, groups = c("time", "species", "species_stanza"))

  # Plot spatial distribution per species, species_stanza, timesteps and layer!
  # Use layer (y-direction) and timestep (x-direction) to facet_grid
  plot_spatial_species <- function(data, full_grid) {
    # add time to polygon layout
    bgrd <- merge(full_grid, unique(dplyr::select_(data, .dots = c("time"))))
    p_title <- paste("Species:", unique(data$species), "with stanza:", unique(data$species_stanza))

    data <- dplyr::left_join(bgrd, data, by = c("polygon", "layer", "time"))
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~long, y = ~lat, fill = ~atoutput, group = ~factor(polygon))) +
      ggplot2::geom_polygon(colour = "black") +
      ggplot2::facet_grid(layer ~ time) +
      ggplot2::scale_fill_gradient("biomass distribution", low = "red", high = "green") +
      ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 20)) +
      ggplot2::coord_equal() +
      theme_atlantis() +
      ggplot2::labs(title = p_title)

    plot <- ggplot_custom(plot, scientific = FALSE)

    return(plot)
  }

  # Step2: Apply predator and stanza specific plot function
  dfs_spatial <- select_time(perc_bio, timesteps = timesteps) %>%
    split_dfs(cols = c("species", "species_stanza"))
  plots_spatial <- lapply(dfs_spatial, plot_spatial_species, full_grid = full_grid)

  # Step3: Combine plots with polygon overview!
  grobs <- lapply(plots_spatial, plot_add_polygon_overview, bgm_as_df = bgm_as_df, polygon_overview = polygon_overview)

  return(grobs)
}

# Utility functions
# Select timesteps
select_time <- function(df, timesteps = 2) {
  time_sorted <- sort(unique(df$time))
  if (timesteps < length(time_sorted)) {
    select_time <- c(min(df$time), max(df$time))
    if (timesteps > 2) {
      pos <- (1:(timesteps - 1) * (trunc(length(time_sorted) / (timesteps - 1)))) + 1
      pos <- pos[-length(pos)]
      select_time <- c(select_time, time_sorted[pos])
    }
    dplyr::filter_(df, ~time %in% select_time)
  } else {
    df
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
  names(dfs) <- apply(df_cat, MARGIN = 1, paste, collapse = " ")
  return(dfs)
}

