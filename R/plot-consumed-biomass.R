#' Circle diagramm to visualise the consumed biomass for the whole system.
#'
#' @param bio_consumed Consumed biomass of prey groups by predatorgroup and agecl in tonnes
#' for each timestep and polygon. Dataframe with columns 'pred', 'agecl', 'polygon', 'time', 'prey'.
#' Consumed biomass in [t] is stored in column 'atoutput'. Should be generated with
#' \code{link{calculate_consumed_biomass}}.
#' @param select_time Numeric value to control the simulation time in years to visualise.
#' By default the start of the simulation is shown. Default is \code{NULL}.
#' @param show Numeric value between 0 - 1 to control the amount of links shown. Default is \code{0.95}.
#' Thus, the most important 95% of the total biomass flows are shown. The remaining interactions
#' are grouped together as 'Rest'.
#' @return plot
#' @export
#'
#' @examples
#' plot_consumed_biomass(ref_bio_cons)
#' plot_consumed_biomass(ref_bio_cons, select_time = 1.8)
#' plot_consumed_biomass(ref_bio_cons, select_time = 1.8, show = 0.99)
#'
#' # Add some gns testing.
#' \dontrun{
#' load("z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/preprocess-north-sea.rda", verbose = T)
#' plot_consumed_biomass(result$biomass_consumed, show = 0.95, select_time = 2.2)
#'
#' times <- c(0.2, seq(10, 90, 10), 99.8)
#' par(mfcol = c(3, 4))
#' for (i in seq_along(times)) {
#'   plot_consumed_biomass(result$biomass_consumed, show = 0.95, select_time = times[i])
#' }
#'
#' times <- seq(0.2, 2.2, 0.2)
#' # times <- times[times != 1]
#' par(mfcol = c(3, 4))
#' for (i in seq_along(times)) {
#'   plot_consumed_biomass(result$biomass_consumed, show = 0.95, select_time = times[i])
#' }
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
  # Will break if any predator only group is not grouped to Rest!
  d1 <- dplyr::select_(clean_df, .dots = c("group" = "pred", "atoutput"))
  d2 <- dplyr::select_(clean_df, .dots = c("group" = "prey", "atoutput"))
  set_cols <- dplyr::bind_rows(d1, d2) %>%
    agg_data(groups = "group", fun = sum, out = "order") %>%
    dplyr::arrange_(~desc(order))
  set_cols$order <- 1:nrow(set_cols)
  set_cols$col <- rep(get_colpal(), 3)[1:nrow(set_cols)]
  grp_sp <- stringr::str_split(set_cols$group, pattern = " ", n = 2)
  set_cols$reg1 <- abbreviate(sapply(grp_sp, function(x) x[1]))
  set_cols$reg2 <- abbreviate(sapply(grp_sp, function(x) x[2]))
  set_cols$reg2[is.na(set_cols$reg2)] <- ""

  clean_df$rank <- NULL

  # See: https://github.com/gjabel/migest/blob/master/demo/cfplot_reg2.R
  # for Details!
  circlize::circos.clear()
  circlize::circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
  graphics::par(mar = rep(0, 4))

  # chordDiagramFromDataFrame has a weird if !is.null(df$rank) which throws a warning by default...
  # Might be hard to bedug this in case of a different warning.
  suppressWarnings(circlize::chordDiagram(x = clean_df, grid.col = set_cols$col, transparency = 0.25,
                         order = set_cols$group, directional = 1,
                         direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                         annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
                         link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE))

  # line 1: read in the data (clean_df) and set colours of the outer sectors (set_cols).
  # line 2: set order of outer sectors and indicates that chords should be directional.
  # line 3: direction of the chords will be illustrated by both arrows and a difference in height. The
  #        height difference is negative to make the chord shorter at the end (with the arrow head).
  # line 4: annotations outside the sectors are not plotted, but provides a track measures.
  # line 5: use big arrows, sort the chords left to right in each sector and plot the smallest chords first.

  # Create space between x-axis ticks. In total 24 ticks for the whole circle --> every 15°
  sectors <- sapply(circlize::get.all.sector.index(), circlize::get.cell.meta.data, name = "xlim")[2, ]
  tick_step <- sum(sectors) / 24

  # Add axis and text.
  circlize::circos.trackPlotRegion(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      xlim = circlize::get.cell.meta.data("xlim")
      sector.index = circlize::get.cell.meta.data("sector.index")
      reg1 = set_cols$reg1[set_cols$group == sector.index]
      reg2 = set_cols$reg2[set_cols$group == sector.index]

      test <- nchar(reg2)
      circlize::circos.text(x = mean(xlim), y = ifelse(test == 0, yes = 6.8, no = 7.6),
                           labels = reg1, facing = "bending")
      circlize::circos.text(x = mean(xlim), y = 6,
                            labels = reg2, facing = "bending")

      circlize::circos.axis(h = "top",
                            major.at = as.numeric(scales::scientific(seq(from = 0, to = xlim[2], by = tick_step), digits = 2)),
                            labels.cex = 0.8,
                            major.tick.percentage = 0.5,
                            labels.niceFacing = FALSE)
    }
  )

  # line 1:   only use 1st track.
  # line 2:   do not plot borders around the tracks.
  # line 3:   Add a track.
  # line 4+5: collect individual track meta data from plot object.
  # line 6+7: collect matching name information from plot data frame (cet_cols).
  # line 8:   (1st circos.text) add text from (reg1) either at y = 6 (if there is a second part of the name in reg2) or 5.2.
  # line 9:   (2nd circos.text) add text (reg2).
  # line 10:  add axis with major and minor ticks, without flipping the axis labels in the bottom half.
}



