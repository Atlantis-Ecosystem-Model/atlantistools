#' Calculate 3d overlap of predator groups with their prey over time using Schoener Index.
#'
#' @param biomass_spatial Biomass timeseries of each group and ageclass per polygon
#' and layer. This dataframe should be generated with \code{\link{calculate_biomass_spatial}}.
#' @param dietmatrix Availability matrix given in the biological parameter file.
#' This dataframe should be generated with \code{\link{load_dietmatrix}}. Please
#' make sure to use \code{convert_names = TRUE} in \code{load_dietmatrix}.
#' @param agemat First mature age class for age structured groups. This dataframe should
#' be generated with \code{\link{prm_to_df}} using "age_mat" as parameter.
#' @inheritParams preprocess
#' @return Schoener's similarity index ranging from 1 (perfect overlap) to
#' 0 (zero overlap).
#'
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
#' agemat <- prm_to_df(dir = dir, prm_biol = prm_biol, fgs = fgs, group = get_age_acronyms(dir = dir, fgs = fgs),
#'                     parameter = "age_mat")
#'
#' sp_overlap <- calculate_spatial_overlap(biomass_spatial, dietmatrix)


calculate_spatial_overlap <- function(biomass_spatial, dietmatrix, agemat) {
  # Check input dataframes!
  check_df_names(biomass_spatial, expect = c("species", "agecl", "polygon", "layer", "time", "atoutput"))
  check_df_names(dietmatrix, expect = c("pred", "pred_stanza", "prey_stanza","code", "prey", "avail", "prey_id"))
  check_df_names(agemat, expect = c("species", "age_mat"))

  # Step1: Calculate relative biomass per box and layer per group and stanza!
  # - Age based groups!
  bio_stanza <- combine_ages(biomass_spatial, grp_col = "species", agemat = agemat)
  bio_stanza$species_stanza[is.na(bio_stanza$species_stanza)] <- 1 # Not sure if this is correct!
  biomass <- agg_perc(bio_stanza, groups = c("time", "species", "species_stanza"), out = "perc_bio")

  # Fill data gaps to make sure that both combinations are present later:
  # - pred/stanza present in box/layer combination & prey/stanza absent
  # - prey/stanza present in box/layer combination $ pred/stanza absent
  ac_box_layer <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("polygon", "layer")))
  ac_pred_stan <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("species", "species_stanza")))
  ac_time <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("time")))

  data_bio <- merge(ac_box_layer, ac_pred_stan) %>%
    merge(ac_time) %>%
    dplyr::left_join(biomass, by = c("time", "species", "species_stanza", "polygon", "layer"))
  data_bio$perc_bio[is.na(data_bio$perc_bio)] <- 0

  # Apply Schoener calculations to all predators!
  ps <- data_bio %>%
    dplyr::select_(.dots = c("species", "species_stanza")) %>%
    unique()
  sis <- Map(schoener, pred = ps$species, pred_stanza = ps$species_stanza,
             MoreArgs = list(biomass = data_bio, avail = dietmatrix))
}


# 3rd step: Calculate schoener index per pred / prey combination (including stanzas)
# - pred: %biomass per predator ageclass per time, box, layer
# - avail: availability matrix
# - prey: overall preybiomass
# pred <- ps$species[1]
# pred_stanza <- ps$species_stanza[1]
# biomass <- data_bio
# avail <- dietmatrix
schoener <- function(pred, pred_stanza, biomass, avail) {
  if (!(length(pred) == 1 & length(pred_stanza) == 1)) {
    stop("Only one predator/agecl combination allowed in dataframe 'pred'.")
  }

  # Select specific predator/predator stanza combination! (columns still called species at this point)
  df_pred <- biomass[biomass$species_stanza == pred_stanza & biomass$species == pred, ]
  df_avail <- avail[avail$pred == pred & avail$pred_stanza == pred_stanza, ]
  # Remove predator/predstanza since overlap is 1 by default!
  biomass_clean <- dplyr::anti_join(biomass, df_pred) %>%
  # Remove sediment layer! Need to read in the sediment penetration depth (KDEP) to include sedimant layer.
    dplyr::filter_(lazyeval::interp(~ col != max(col), col = as.name("layer")))

  # Combine predator data with prey data!
  # WARNING: This may lead to a very huge dataframe... all (even non existing)
  # pred/prey combinations are combined! Therefore the calculations are applied per predator.
  # The 2nd join has to be an inner_join to make sure only available prey groups are
  # used to calculate the overlap index!
  si <- dplyr::left_join(df_avail, df_pred, by = c("pred" = "species", "pred_stanza" = "species_stanza")) %>%
    dplyr::inner_join(biomass, by = c("prey" = "species", "prey_stanza" = "species_stanza", "time", "polygon", "layer"))

  si$si <- with(si, abs(perc_bio.x - perc_bio.y))

  # Schoner index per pred/predstanza/prey/preystanza combination!
  si_spec <- si %>%
    agg_data(col = "si", groups = c("time", "pred", "pred_stanza", "prey", "prey_stanza", "avail"), out = "si", fun = sum) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~1 - si * 0.5), "si"))

  # 4th step: Aggregate schoner index based on availabilities
  # Combine to pred/predstanza index! Weight with present availabilities!
  si_overall <- si_spec %>%
    agg_perc(col = "avail", groups = c("time", "pred", "pred_stanza"), out = "avail") %>%
    dplyr::mutate_(.dots = stats::setNames(list(~si * avail), "si")) %>%
    agg_data(col = "si", groups = c("time", "pred", "pred_stanza"), out = "si", fun = sum)

  return(list(si_spec, si_overall))
}

# ggplot2::ggplot(si_spec, ggplot2::aes(x = time, y = si, group = time)) +
#   ggplot2::geom_violin() +
#   ggplot2::geom_point(data = si_overall, colour = "red")

# ggplot2::ggplot(sis[[18]][[1]], ggplot2::aes(x = time, y = si, group = time)) +
#   ggplot2::geom_violin() +
#   ggplot2::geom_point(data = sis[[18]][[2]], colour = "red")




