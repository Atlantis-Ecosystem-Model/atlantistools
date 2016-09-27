#' Calculate 3d overlap of predators groups with their prey over time.
#'

dir <- "c:/backup_z/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/"
nc <- "outputNorthSea.nc"
prm_biol <- "NorthSea_biol_fishing.prm"
fgs <- "functionalGroups.csv"
bps <- load_bps(dir = dir, fgs = fgs, init = "init_NorthSea.nc")
bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
pred <- NULL

sc_overlap <- function(dir = getwd(), nc, prm_biol, bps, fgs, bboxes, out,
                       pred = NULL, save_to_disc = FALSE) {

  fgs_data <- load_fgs(dir = dir, fgs = fgs)

  if (is.null(pred)) {
    acr_age <- get_age_acronyms(dir = dir, fgs = fgs)
  } else {
    acr_age <- fgs_data$Code[is.element(fgs_data$LongName, pred)]
    if (length(acr_age) == 0) stop("Please provide pred as LongName.")
    if (length(acr_age) != length(pred)) stop("Not all predators present in functionalGroups file")
  }

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  # 1st step: Load in data!
  # - n per box and layer for each invert group
  # - nums, resn, structn for each vert group per box, layer and ageclass
  # - availability matrix
  # - Volume per layer and box, layerthickness
  # - convrsion factor mgN to bio t
  vars <- list("StructN", "ResN", "Nums", "N")
  grps <- list(groups_age, groups_age, groups_age, groups_rest)

  data_bio <- Map(load_nc, select_variable = vars, select_groups = grps,
                  MoreArgs = list(dir, nc = nc, bps = bps, fgs = fgs, bboxes = bboxes))
  names(data_bio) <- tolower(vars)

  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs) %>%
    dplyr::filter(!grepl(pattern = "sed", x = prey, ignore.case = FALSE)) %>% # Not present in fgs, therefore convert_factor will break if present.
    dplyr::filter(is.element(pred, acr_age) & avail != 0) %>%
    dplyr::mutate_at(.cols = c("pred", "prey"), .funs = convert_factor, data_fgs = fgs_data)

  vol <- load_nc_physics(dir = dir, nc = nc, select_physics = c("volume", "dz"), bboxes = bboxes, aggregate_layers = F)

  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  age_mat <- vapply(paste0(acr_age, "_", "age_mat"), extract_prm,
                    dir = dir, prm_biol = prm_biol, numeric(1), USE.NAMES = FALSE)
  age_mat <- data.frame(species = acr_age, age_mat = age_mat, stringsAsFactors = FALSE)
  age_mat$species <- convert_factor(data_fgs = fgs_data, col = age_mat$species)

  # 2nd step: Calculate relative biomass per box and layer per group and stanza!
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
    df_avail$avail <- NULL
    # df_avail <- dplyr::mutate_(df_avail, .dots = stats::setNames(list(~avail/sum(avail)), "avail"))

    # Combine predator data with prey data!
    # WARNING: This may lead to a very huge dataframe... all (even non existing)
    # pred/prey combinations are combined!
    si <- dplyr::full_join(df_pred, biomass, by = c("time", "polygon", "layer"))
    names(si)[names(si) == "species.x"] <- "pred"
    names(si)[names(si) == "species.y"] <- "prey"
    names(si)[names(si) == "pred_stanza.x"] <- "pred_stanza"
    names(si)[names(si) == "pred_stanza.y"] <- "prey_stanza"
    names(si)[names(si) == "perc_bio.x"] <- "perc_bio_pred"
    names(si)[names(si) == "perc_bio.y"] <- "perc_bio_prey"
    # Fill predator data-gaps! How to handle mising prey overlap?
    ids <- is.na(si$pred)
    if (sum(ids) > 0) {
      si$pred[ids] <- pred
      si$pred_stanza[ids] <- pred_stanza
      si$perc_bio_pred[ids] <- 0
    }
    si <- dplyr::inner_join(si, df_avail, by = c("pred", "pred_stanza", "prey", "prey_stanza"))
    si$si <- with(si, abs(perc_bio_pred - perc_bio_prey))

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

  # Apply calculations to all predators!
  preds <- rep(acr_age, each = 2)
  stanzas <- rep(1:2, times = length(acr_age))
  sis <- Map(schoener, pred = preds, pred_stanza = stanzas, MoreArgs = list(biomass = biomass, avail = data_dm))
}

