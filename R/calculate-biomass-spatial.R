#' Calculate spatiallly explicit biomass (in [t]) for each group and ageclass per timestep.
#'
#' @inheritParams preprocess
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' nc_gen <- "outputSETAS.nc"
#' prm_biol <- "VMPA_setas_biol_fishing_Trunk.prm",
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    bps = load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc"),
#'    fgs = "SETasGroups.csv",
#'    select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Diatom", "Zoo"),
#'    bboxes = get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm")),
#'    check_acronyms = TRUE,
#'    modelstart = "1991-01-01",
#'    out = "preprocess.Rda",
#'    save_to_disc = FALSE,
#'    version_flag = 1)
#'    }

calculate_biomass_spatial <- function(dir = getwd(), nc_gen, prm_biol, bps, fgs, bboxes) {
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
                  MoreArgs = list(dir, nc = nc_gen, bps = bps, fgs = fgs, bboxes = bboxes))
  names(data_bio) <- tolower(vars)

  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs) %>%
    dplyr::filter(!grepl(pattern = "sed", x = prey, ignore.case = FALSE)) %>% # Not present in fgs, therefore convert_factor will break if present.
    dplyr::filter(is.element(pred, acr_age) & avail != 0) %>%
    dplyr::mutate_at(.cols = c("pred", "prey"), .funs = convert_factor, data_fgs = fgs_data)

  vol <- load_nc_physics(dir = dir, nc = nc_gen, select_physics = c("volume", "dz"), bboxes = bboxes, aggregate_layers = F)

  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)

  age_mat <- vapply(paste0(acr_age, "_", "age_mat"), extract_prm,
                    dir = dir, prm_biol = prm_biol, numeric(1), USE.NAMES = FALSE)
  age_mat <- data.frame(species = acr_age, age_mat = age_mat, stringsAsFactors = FALSE)
  age_mat$species <- convert_factor(data_fgs = fgs_data, col = age_mat$species)

  # 2nd step: Calculate biomass per time, box and layer per group and ageclass!
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

}

