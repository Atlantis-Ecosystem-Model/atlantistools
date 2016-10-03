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
#' @param timesteps Integer giving the number of timesteps to visualise. The minimum
#' value is 2 (default). By default the start and end of the simulation is shown. In case
#' timesteps > 2 equally spaced timesteps - 2 are added.
#' @return grob of 3 ggplot2 plots.
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
#'
#' \dontrun{
#' grobs <- plot_spatial(bio_spatial, bgm_as_df, timesteps = 3)
#' gridExtra::grid.arrange(grobs[[3]])
#' }
#'
#' grobs <- plot_spatial(bio_spatial, bgm_as_df, select_species = "Crangon", timesteps = 3)
#' gridExtra::grid.arrange(grobs[[1]])

plot_spatial <- function(bio_spatial, bgm_as_df, select_species = NULL, timesteps = 2){
  # Check input dataframe!
  check_df_names(bio_spatial, expect = c("species", "polygon", "layer", "time", "species_stanza", "atoutput"))
  check_df_names(bgm_as_df, expect = c("lat", "long", "inside_lat", "inside_long", "polygon"))

  # Filter by species if select_species not NULL!
  # Warning: Will change input parameter which makes it harder to debug...
  if (!is.null(select_species)) {
    if (all(select_species %in% unique(bio_spatial$species))) {
      bio_spatial <- dplyr::filter_(bio_spatial, ~species %in% select_species)
    } else {
      stop("Not all selected_species are present in bio_spatial.")
    }
  }

  # Get available species and stanzas!
  pred_stanza <- unique(dplyr::select_(bio_spatial, .dots = c("species", "species_stanza")))

  # Step1: Calculate different summary tables
  # - perc biomass per box and layer
  perc_bio <- agg_perc(bio_spatial, groups = c("time", "species", "species_stanza"))
  # - biomass timeseries per box
  ts_bio <- agg_data(bio_spatial, groups = c("time", "species", "species_stanza", "polygon"), fun = sum)

  # Plot spatial distribution per species, species_stanza, timesteps and layer!
  # Use layer (y-direction) and timestep (x-direction) to facet_grid
  plot_spatial_species <- function(data) {
    # add time to polygon layout
    bgrd <- merge(bgm_as_df, unique(dplyr::select_(data, .dots = c("time", "layer"))))
    data <- dplyr::left_join(bgrd, data, by = c("polygon", "layer", "time"))
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~long, y = ~lat, fill = ~atoutput, group = ~factor(polygon))) +
      ggplot2::geom_polygon(colour = "black") +
      ggplot2::facet_grid(layer ~ time) +
      ggplot2::scale_fill_gradient("biomass distribution", low = "red", high = "green") +
      ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 20)) +
      ggplot2::coord_equal() +
      theme_atlantis()

    return(plot)
  }

  plot_ts_species <- function(data) {
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, y = ~atoutput, colour = ~atoutput)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~polygon, scales = "free_y", ncol = 2, labeller = ggplot2::label_wrap_gen(width = 15)) +
      # ggplot2::scale_y_continuous(breaks = function(x) c(min(x), max(x)), labels = function(x) scales::scientific(x, digits = 2)) +
      ggplot2::scale_colour_gradientn("biomass [t]", colours = rainbow(n = 7), labels = function(x) scales::scientific(x, digits = 2)) +
      ggplot2::labs(x = "time [years]", y = "biomasstrend per polygon") +
      ggplot2::guides(colour = ggplot2::guide_colorbar(barwidth = 10)) +
      theme_atlantis() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())

    return(plot)
  }

  # Create panels
  # 1. Overview of the polygon layout
  bl <- plot_boxes(data = bgm_as_df)
  bl <- bl + ggplot2::theme_void()
  bl <- bl + ggplot2::theme(legend.position = "none")

  # 2. Spatial distribution per predator and stanza per time, layer, polygon
  dfs_spatial <- select_time(perc_bio, timesteps = timesteps) %>%
    split_dfs(cols = c("species", "species_stanza"))
  plots_spatial <- lapply(dfs_spatial, plot_spatial_species)

  # 3. Biomasstimeseries per box
  dfs_ts <- split_dfs(ts_bio, cols = c("species", "species_stanza"))
  plots_ts <- lapply(dfs_ts, plot_ts_species)

  # Combine plots!
  grobs <- vector(mode = "list", length = nrow(pred_stanza))
  for (i in seq_along(grobs)) {
    header <- grid::textGrob(paste("Species:", pred_stanza[i, 1], "with stanza:", pred_stanza[i, 2]),
                             gp = grid::gpar(fontsize = 18))

    grobs[[i]] <- gridExtra::arrangeGrob(
      grobs = c(list(header), list(plots_spatial[[i]]), list(bl), list(plots_ts[[i]])),
      layout_matrix = matrix(c(rep(1, 4), c(rep(2, 3), 3), rep(c(rep(2, 3), 4), 2)), ncol = 4, byrow = TRUE),
      heights = grid::unit(c(0.04, rep(0.32, 3)), units = "npc"))
  }

  names(grobs) <- apply(pred_stanza, MARGIN = 1, paste, collapse = " ")
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

