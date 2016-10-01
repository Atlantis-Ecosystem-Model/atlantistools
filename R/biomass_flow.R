#' Calculate the consumed biomass from predator x to prey Y.
#'
#' Consumption data is extracted from output[...]PROD.nc. Age based groups
#' are stored as "Eat_" non age based groups as "Grazing_". Units are mg N m^-3 d^-1.
#' Factors are species, time, box and agecl (if present). We will refer to species as
#' pred from heron to indicate the predator perspective.
#' Diet constribution data is extracted from DietCheck.txt. Currently this only works
#' with models based on the trunk code. Units are % diet contribution (Will check if
#' it sums to 1 per predator/agecl combination). Factors are pred, time, agecl, prey.
#' The biomass flows are calculated as follows:
#' - Calculate consumed biomass as Eat (or Grazing) * boxvolume per time, pred, agecl, box.
#' - Convert to biomass in [t].
#' - Sum consumed biomass over the model domain per pred, time, agecl.
#' - Combine with diet contributions and calculate consumed biomass of prey species.
#' - Sum up consumed biomass per pred, time, prey.
#' @inheritParams preprocess
#' @param plot_diet Logical indicating if you want to apply the \code{plot_diet} function or not.
#' Default is \code{FALSE}.
#' @return Dataframe

#' @export
#' @examples
#' dir <- "c:/backup_z/Atlantis_models/Runs/dummy_02_ATLANTIS_NS/"
#' nc_prod <- "outputNorthSeaPROD.nc"
#' nc_gen <- "outputNorthSea.nc"
#' dietcheck <- "outputNorthSeaDietCheck.txt"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' prm_run <- "NorthSea_run_fishing_F.prm"
#' fgs <- "functionalGroups.csv"
#
# bps <- load_bps(dir = dir, init = "init_NorthSea.nc", fgs = fgs)
# bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
#
# data_cons <- biomass_flow(dir, nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes)

biomass_flow <- function(dir = getwd(), nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes, plot_diet = FALSE) {
  # Setup group variables
  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  # Extract data
  vars <- list("Eat", "Grazing")
  grps <- list(groups_age, groups_rest)

  data_eat <- Map(load_nc, select_variable = vars, select_groups = grps,
                  MoreArgs = list(dir, nc = nc_prod, bps = bps, fgs = fgs, bboxes = bboxes))
  data_eat <- do.call(rbind, data_eat)
  data_eat$species <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = data_eat$species)
  data_eat <- convert_time(dir = dir, prm_run = prm_run, data = data_eat)

  vol <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = "volume", bboxes = bboxes, aggregate_layers = F)
  vol <- convert_time(dir = dir, prm_run = prm_run, data = vol)

  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  data_dm <- load_dietcheck(dir = dir, dietcheck = dietcheck, fgs = fgs, report = F, version_flag = 2)
  data_dm <- convert_time(dir = dir, prm_run = prm_run, data = data_dm)

  # Check DietCheck.txt
  check <- agg_data(data_dm, groups = c("time", "pred", "agecl"), fun = sum)
  if (!all(abs(check$atoutput -1) < 0.001)) stop("DietCheck.txt does not sum to 1 for all predators.")

  # Check timesteps!
  ts_eat <- sort(unique(data_eat$time))
  ts_dm <- sort(unique(data_dm$time))
  matching <- sum(ts_eat %in% ts_dm) / length(ts_eat)
  message(paste0(100 * round(matching, digits = 2), "% matching timesteps between PROD.nc and DietCheck.txt"))

  # Step1: Calculate consumed biomass as Eat (or Grazing) * boxvolume per time, pred, agecl, box.
  # Weired stuff is happening here... Epibenthic groups consume the bulk of biomass!
  # Let's chceck this again with a different model!
  boxvol <- agg_data(vol, groups = c("polygon", "time"), out = "vol", fun = sum)
  consumed_bio <- dplyr::left_join(data_eat, boxvol, by = c("polygon", "time")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput * vol), "atoutput")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput * bio_conv), "atoutput")) %>%
  # Setp2: Aggregate spatially
    agg_data(groups = c("species", "time", "agecl"), fun = sum) %>%
  # Step3: Combine with diet contribution. We need a full join to make sure no data is lost!
    dplyr::full_join(data_dm, by = c("species" = "pred", "time", "agecl")) %>%
  # Restrict timesteps to netcdf data!
    dplyr::filter_(~time %in% ts_eat) %>%
    dplyr::rename_(.dots = c("pred" = "species"))

  # Some detective work is needed here!
  det_eat <- consumed_bio[is.na(consumed_bio$atoutput.x), ]
  det_dm <- consumed_bio[is.na(consumed_bio$atoutput.y), ]

  # Remove NAs!
  if (nrow(det_eat) > 0) {
    message(paste0(100 * round(nrow(det_eat)/nrow(consumed_bio), digits = 2),
                   "% data is lost due to missing diet data despite available eat data."))
  }
  if (nrow(det_dm) > 0) {
    message(paste0(100 * round(nrow(det_dm)/nrow(consumed_bio), digits = 2),
                   "% data is lost due to missing eat data despite available diet data."))
  }

  # atoutput.x = eat, atoutput.y = diet
  # Setp4: Calculate consumed biomass of prey species.
  data_cons <- consumed_bio %>%
    dplyr::filter_(~!is.na(atoutput.x)) %>%
    dplyr::filter_(~!is.na(atoutput.y)) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput.x * atoutput.y), "atoutput"))

  # This is intended as workaround for the moment! Will clean this mess later.
  if (plot_diet) {
    data_cons <- agg_perc(data_cons, groups = c("time", "prey", "agecl"))
    data_cons$atoutput.x <- NULL
    data_cons$atoutput.y <- NULL
  } else {
    # Setp5: Sum up consumed biomass over ages per time, pred and prey!
    data_cons <- agg_data(data_cons, groups = c("time", "pred", "prey"), fun = sum)
  }

  return(data_cons)
}


# df = data_cons
# select_time = 5
# show = 0.95
#
# plot_biomass_flow <- function(df, select_time, show) {
#   # Restrict to selected timestep!
#   one_time <- dplyr::filter(df, time == select_time) %>%
#     dplyr::ungroup() %>%
#     dplyr::select_(.dots = c("prey", "pred", "atoutput"))
#
#   # Select main feedig interactions based on cumulative treshold.
#   main_links <- one_time %>%
#     dplyr::mutate_(.dots = stats::setNames(list(~atoutput/sum(atoutput)), "perc")) %>%
#     dplyr::arrange_(quote(desc(perc)))
#   main_links <- main_links[1:min(which(cumsum(main_links$perc) > show)), ]
#
#   # Only select species contributing to main interactions.
#   grps <- union(main_links$prey, main_links$pred)
#   one_time$pred[!is.element(one_time$pred, grps)] <- "Rest"
#   one_time$prey[!is.element(one_time$prey, grps)] <- "Rest"
#
#   clean_df <- agg_data(one_time, groups = c("pred", "prey"), fun = sum)
#
#   # cols <- unique(clean_df$pred)
#   # cols <- data.frame(pred = cols, col = rep(get_colpal(), 3)[1:length(cols)], stringsAsFactors = FALSE)
#   #
#   # plot_df <- dplyr::left_join(clean_df, cols)
#   # plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
#
#   ring <- agg_data(clean_df, groups = "prey", fun = sum) %>%
#     dplyr::arrange_(~desc(atoutput))
#
#   circlize::circos.clear()
#   circlize::circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.12, 0.12),
#                        cell.padding = c(0,0), points.overflow.warning = FALSE)
#   par(mar = rep(0, 4))
#
#   circlize::chordDiagram(x = plot_df, col = plot_df$col, transparency = 0.1, directional = 1,
#                          direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
#                          annotationTrack = "grid",
#                          link.arr.type = "big.arrow",link.sort = TRUE, link.largest.ontop = TRUE)
#
#   # circlize::chordDiagram(x = clean_df,  transparency = 0.1, directional = 1,
#   #                        direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
#   #                        annotationTrack = "grid",
#   #                        link.arr.type = "big.arrow",link.sort = TRUE, link.largest.ontop = TRUE)
#
#   circlize::circos.trackPlotRegion(
#     track.index = 1,
#     bg.border = NA,
#     panel.fun = function(x, y) {
#       xlim = circlize::get.cell.meta.data("xlim")
#       sector.index = circlize::get.cell.meta.data("sector.index")
#       # reg1 = df9$reg1[df9$pob == sector.index]
#       # reg2 = df9$reg2[df9$pob == sector.index]
#       # kit = df9$kit[df9$pob == sector.index]
#
#       circlize::circos.text(x = mean(xlim), y = 1, labels = ring$prey, col = "white",
#                     facing = "clockwise", pos = 4, cex = 0.7, offset = 0)
#
#     }
#   )
#
#   circlize::circos.text(x = mean(xlim), y = 1, labels = ring$prey, col = "white",
#                         facing = "clockwise", pos = 4, cex = 0.7, offset = 0)
#
#
#
# }
#
#
# plot_biomass_flow(df = data_cons, select_time = 3, show = 0.95)



