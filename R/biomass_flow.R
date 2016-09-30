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
# bps <- load_bps(dir = dir, init = "init_NorthSea.nc", fgs = fgs)
# bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))

biomass_flow <- function(dir = getwd(), nc_prod, nc_gen, dietcheck, prm_biol, prm_run, bps, fgs, bboxes) {
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

  data_dm <- load_dietcheck(dir = dir, dietcheck = dietcheck, report = F, version_flag = 2)
  data_dm <- convert_time(dir = dir, prm_run = prm_run, data = data_dm)
  data_dm <- dplyr::mutate_at(data_dm, .cols = c("pred", "prey"), .funs = convert_factor, data_fgs = fgs_data)

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
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput.x * atoutput.y), "atoutput")) %>%
  # Setp5: Sum up consumed biomass over ages per time, pred and prey!
    agg_data(groups = c("time", "pred", "prey"), fun = sum)

  return(data_cons)
}


# df <- data_cons
# select_time <- 5
# plot_biomass_flow <- function(df, select_time) {
#   one_time <- dplyr::filter(df, time == select_time) %>%
#     dplyr::ungroup() %>%
#     dplyr::select_(.dots = c("prey", "pred", "atoutput"))
#
#   # remove prey only groups.
#   grps <- unique(one_time$pred)
#   plot_df <- one_time %>%
#     dplyr::filter(is.element(prey, grps)) %>%
#     dplyr::mutate_(.dots = stats::setNames(list(~atoutput/sum(atoutput)), "perc")) %>%
#     dplyr::arrange_(quote(desc(perc)))
#
#   main_links <- plot_df[1:min(which(cumsum(plot_df$perc) > 0.95)), ]
#   clean_df <- plot_df
#   clean_df$pred[!is.element(clean_df$pred, unique(c(main_links$pred, main_links$prey)))] <- "Rest"
#   clean_df$prey[!is.element(clean_df$prey, main_links$prey)] <- "Rest"
#
#   clean_df <- agg_data(clean_df, col = "perc", groups = c("pred", "prey"), fun = sum)
#
#   # Select main links!
#
#   ggplot2::ggplot(one_time, ggplot2::aes(x = pred, y = atoutput, fill = prey)) +
#     ggplot2::geom_bar(stat = "identity") +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
#
#
#   # remove pred only groups.
#   grps <- unique(one_time$prey)
#   plot_df <- one_time %>%
#     dplyr::filter(is.element(pred, grps))
#
#   # Does not work
#   circlize::chordDiagram(plot_df)
#   circlize::chordDiagram(clean_df)
#
#   # Does work!
#   test <- tidyr::spread_(one_time, key_col = "prey", value_col = "atoutput", fill = 0)
#   test <- dplyr::filter(test, is.element(pred, c("Cod", "Crangon", "Dab")))
#   test <- dplyr::select(test, pred, Cod, Crangon, Dab)
#   dd <- data.frame(pred = rep(test$pred, 3), prey = rep(test$pred, each = 3), value = unlist(test[1:3, 2:4]))
#
#   circlize::chordDiagram(dd)
#
#   dd2 <- rbind(dd, data.frame(pred = "Cod", prey = "xxx", value = 2000))
#
#   circlize::chordDiagram(dd2)
#
#   dd3 <- rbind(dd, data.frame(pred = "xxx", prey = "Cod", value = 2000))
#
#   circlize::chordDiagram(dd3)
#
#   dd4 <- dd
#   dd4$value <- dd4$value/100000000000000
#
#   circlize::chordDiagram(dd4)
#
#   dd5 <- dplyr::filter(dd, value != 0)
#   circlize::chordDiagram(dd5)


  #   dplyr::filter(is.element(pred, grps) & is.element(prey, grps))
  #   dplyr::filter_(~is.element(prey, grps))
  #
  # plot_df <- one_time %>%
  #   dplyr::filter(is.element(prey, grps))
  #
  # cols <- data.frame(pred = unique(dd$pred), col = get_colpal()[1:3], stringsAsFactors = FALSE)
  # cols <- data.frame(pred = grps, col = rep(get_colpal(), 3)[1:length(grps)], stringsAsFactors = FALSE)
  #
  # plot_df <- dplyr::left_join(dd, cols)
  # plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  #
  # circlize::circos.clear()
  # circlize::circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.12, 0.12),
  #                      cell.padding = c(0,0), points.overflow.warning = FALSE)
  # par(mar = rep(0, 4), bg = "black")
  #
  # circlize::chordDiagram(x = plot_df, col = plot_df$col, grid.col = get_colpal()[1:3], transparency = 0.1, directional = 1,
  #                        direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
  #                        annotationTrack = "grid", annotationTrackHeight = c(0.01, 0.01),
  #                        link.arr.type = "big.arrow",link.sort = TRUE, link.largest.ontop = TRUE)



# }








