#' Calculate 3d overlap of predator groups with their prey over time using Schoener Index.
#'
#' @param biomass_spatial Biomass timeseries of each group and ageclass per polygon
#' and layer. This dataframe should be generated with \code{\link{calculate_biomass_spatial}}.
#' @param dietmatrix Availability matrix given in the biological parameter file.
#' This dataframe should be generated with \code{\link{load_dietmatrix}}. Please
#' use \code{convert_names = TRUE} in \code{load_dietmatrix}.
#' @param agemat First mature age class for age structured groups. This dataframe should
#' be generated with \code\link{prm_to_df} using "age_mat" as parameter.
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
  names(data_bio$structn)[names(data_bio$structn) == "atoutput"] <- "sn"
  names(data_bio$resn)[names(data_bio$resn) == "atoutput"] <- "rn"
  biomass_age <- dplyr::inner_join(data_bio$nums, data_bio$structn) %>%
    dplyr::left_join(data_bio$resn) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~convert_factor(data_fgs = fgs_data, col = species)), "species")) %>%
    dplyr::left_join(age_mat) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~ifelse(agecl < age_mat, 1, 2)), "pred_stanza")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~(sn + rn) * atoutput * bio_conv), "bio")) %>%
    agg_data(col = "bio", groups = c("species", "pred_stanza", "time", "polygon", "layer"), out = "bio", fun = sum)

  # - Non age based groups!
  vol <- tidyr::spread_(data = vol, key_col = c("variable"), value_col = "atoutput")
  biomass_pools <- dplyr::left_join(data_bio$n, vol)
  biomass_pools$bio <- with(biomass_pools, ifelse(species %in% bps, atoutput * volume / dz * bio_conv, atoutput * volume * bio_conv))
  biomass_pools <- dplyr::select_(biomass_pools, .dots = c("species", "time", "polygon", "layer", "bio"))
  biomass_pools$pred_stanza <- 1
  biomass_pools$species <- convert_factor(data_fgs = fgs_data, col = biomass_pools$species)

  # Combine with biomass from age-groups and calculate % biomass per box and layer!
  biomass <- dplyr::bind_rows(biomass_age, biomass_pools) %>%
    agg_perc(col = "bio", groups = c("species", "time", "pred_stanza"), out = "perc_bio")

  # Fill data gaps to make sure that both combinations are present later:
  # - pred/stanza present in box/layer combination & prey/stanza absent
  # - prey/stanza present in box/layer combination $ pred/stanza absent
  ac_box_layer <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("polygon", "layer")))
  ac_pred_stan <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("species", "pred_stanza")))
  ac_time <- unique(dplyr::select_(dplyr::ungroup(biomass), .dots = c("time")))

  data_bio <- merge(ac_box_layer, ac_pred_stan)
  data_bio <- merge(data_bio, ac_time) %>%
    dplyr::left_join(biomass)
  data_bio$perc_bio[is.na(data_bio$perc_bio)] <- 0


  # Apply calculations to all predators!
  preds <- rep(convert_factor(data_fgs = fgs_data, col = acr_age), each = 2)
  stanzas <- rep(1:2, times = length(acr_age))
  sis <- Map(schoener, pred = preds, pred_stanza = stanzas, MoreArgs = list(biomass = data_bio, avail = data_dm))
}


# 3rd step: Calculate schoener index per pred / prey combination (including stanzas)
# - pred: %biomass per predator ageclass per time, box, layer
# - avail: availability matrix
# - prey: overall preybiomass
# pred <- "Cod"
# pred_stanza <- 1
# avail <- dm
schoener <- function(pred, pred_stanza, biomass, avail) {
  if (!(length(pred) == 1 & length(pred_stanza) == 1)) {
    stop("Only one predator/agecl combination allowed in dataframe 'pred'.")
  }

  df_pred <- biomass[biomass$pred_stanza == pred_stanza & biomass$species == pred, ]

  df_avail <- avail[avail$pred == pred & avail$pred_stanza == pred_stanza, ]

  # Combine predator data with prey data!
  # WARNING: This may lead to a very huge dataframe... all (even non existing)
  # pred/prey combinations are combined!
  # The 2nd join has to be an inner_join to make sure only available prey groups are
  # used to calculate the overlap index!
  si <- dplyr::left_join(df_avail, df_pred, by = c("pred" = "species", "pred_stanza")) %>%
    dplyr::inner_join(biomass, by = c("prey" = "species", "prey_stanza" = "pred_stanza", "time", "polygon", "layer"))

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
  # ggplot2::ggplot(si_spec, ggplot2::aes(x = time, y = si, group = time)) +
  #   ggplot2::geom_violin() +
  #   ggplot2::geom_point(data = si_overall, colour = "red")
}


# ggplot2::ggplot(sis[[18]][[1]], ggplot2::aes(x = time, y = si, group = time)) +
#   ggplot2::geom_violin() +
#   ggplot2::geom_point(data = sis[[18]][[2]], colour = "red")




