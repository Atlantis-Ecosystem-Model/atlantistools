#' Circle diagramm to visualise the consumed biomass for the whole system.
#'
#' @param bio_consumed Consumed biomass of prey groups by predatorgroup and agecl in tonnes
#' for each timestep and polygon. Dataframe with columns 'pred', 'agecl', 'polygon', 'time', 'prey'.
#' Consumed biomass in [t] is stored in column 'atoutput'. Should be generated with
#' \code{link{calculate_consumed_biomass}}.
#' @param select_time Numeric value to control the simulation time in years to visualise.
#' By default the start of the simulation is shown.
#' @param show Numeric value between 0 - 1 to control the amount of links shown. Default value is 0.95.
#' Thus, the most important 95% of the total biomass flows are shown. The remaining interactions
#' are grouped together as 'Rest'.
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- "c:/backup_z/Atlantis_models/Runs/dummy_02_ATLANTIS_NS/"
#' nc_prod <- "outputNorthSeaPROD.nc"
#' nc_gen <- "outputNorthSea.nc"
#' dietcheck <- "outputNorthSeaDietCheck.txt"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' prm_run <- "NorthSea_run_fishing_F.prm"
#' fgs <- "functionalGroups.csv"
#' bps <- load_bps(dir = dir, init = "init_NorthSea.nc", fgs = fgs)
#' bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
#'
#' bio_consumed <- calculate_consumed_biomass(dir, nc_prod, nc_gen, dietcheck, prm_biol,
#'                                            prm_run, bps, fgs, bboxes)
#'
#' select_time <- NULL
#' show <- 0.95
#' }

plot_consumed_biomass <- function(bio_consumed, select_time = NULL, show = 0.95) {
  # Restrict to selected timestep!
  if (is.null(select_time)) {
    one_time <- dplyr::filter_(bio_consumed, ~time == min(time))
  } else {
    if (length(select_time) == 1) {
      one_time <- dplyr::filter_(bio_consumed, ~time == select_time)
    } else {
      stop("Only one value for select_time allowed per function call.")
    }
  }

  # Sum up consumed prey over ageclasses and polygons!
  one_time <- agg_data(one_time, groups = c("prey", "pred"), fun = sum)

  # Select main feedig interactions based on cumulative treshold.
  main_links <- one_time %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput/sum(atoutput)), "perc")) %>%
    dplyr::arrange_(quote(desc(perc)))
  main_links <- main_links[1:min(which(cumsum(main_links$perc) > show)), ]

  # Combine groups with low contribution to Rest.
  grps <- union(main_links$prey, main_links$pred)
  one_time$pred[!is.element(one_time$pred, grps)] <- "Rest"
  one_time$prey[!is.element(one_time$prey, grps)] <- "Rest"

  # Setup final dataframes for plotting!
  clean_df <- agg_data(one_time, groups = c("pred", "prey"), fun = sum)


  # See: https://github.com/gjabel/migest/blob/master/demo/cfplot_reg2.R
  # for Details!
  circlize::circos.clear()
  circlize::circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
  graphics::par(mar = rep(0, 4))

  circlize::chordDiagram(x = clean_df, transparency = 0.25,
                          directional = 1,
                         direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                         annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
                         link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)

  # line1: read in the data (clean_df) and set colours of the outer sectors (set_cols).
  # line2: set order of outer sectors and indicates that chords should be directional.
  # line3: direction of the chords will be illustrated by both arrows and a difference in height. The
  #        height difference is negative to make the chord shorter at the end (with the arrow head).
  # line4: annotations outside the sectors are not plotted, but provides a track measures.
  # line5: use big arrows, sort the chords left to right in each sector and plot the smallest chords first.


  # cols <- unique(clean_df$pred)
  # cols <- data.frame(pred = cols, col = rep(get_colpal(), 3)[1:length(cols)], stringsAsFactors = FALSE)
  #
  # plot_df <- dplyr::left_join(clean_df, cols)
  # plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)

  # ring <- agg_data(clean_df, groups = "prey", fun = sum) %>%
  #   dplyr::arrange_(~desc(atoutput))
  #
  # circlize::circos.clear()
  # circlize::circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.12, 0.12),
  #                      cell.padding = c(0,0), points.overflow.warning = FALSE)
  # par(mar = rep(0, 4))
  #
  # circlize::chordDiagram(x = plot_df, col = plot_df$col, transparency = 0.1, directional = 1,
  #                        direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
  #                        annotationTrack = "grid",
  #                        link.arr.type = "big.arrow",link.sort = TRUE, link.largest.ontop = TRUE)
  #
  # # circlize::chordDiagram(x = clean_df,  transparency = 0.1, directional = 1,
  # #                        direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
  # #                        annotationTrack = "grid",
  # #                        link.arr.type = "big.arrow",link.sort = TRUE, link.largest.ontop = TRUE)
  #
  # circlize::circos.trackPlotRegion(
  #   track.index = 1,
  #   bg.border = NA,
  #   panel.fun = function(x, y) {
  #     xlim = circlize::get.cell.meta.data("xlim")
  #     sector.index = circlize::get.cell.meta.data("sector.index")
  #     # reg1 = df9$reg1[df9$pob == sector.index]
  #     # reg2 = df9$reg2[df9$pob == sector.index]
  #     # kit = df9$kit[df9$pob == sector.index]
  #
  #     circlize::circos.text(x = mean(xlim), y = 1, labels = ring$prey, col = "white",
  #                   facing = "clockwise", pos = 4, cex = 0.7, offset = 0)
  #
  #   }
  # )
  #
  # circlize::circos.text(x = mean(xlim), y = 1, labels = ring$prey, col = "white",
  #                       facing = "clockwise", pos = 4, cex = 0.7, offset = 0)



}


# plot_biomass_flow(df = data_cons, select_time = 3, show = 0.95)
