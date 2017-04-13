#' Sanity check initial conditions file
#'
#' @inheritParams load_fgs
#' @inheritParams load_init
#' @inheritParams extract_prm
#' @inheritParams load_nc
#' @inheritParams load_dietmatrix
#' @param pred Vector of predator acronyms to check. If \code{NULL} (default) all age based
#' predators are selected.
#' @param set_avail Numeric value. All present availabilities can be set to a spefiic value.
#' Default value is \code{NULL} which results in no changes to the present availability matrix.
#' @param df Dataframe to pass to \code{plot_sc_init()}. df should be generated with
#' sc_init or read in from *.rda (also generated with sc_init()).
#' @param mult_mum Numeric vector of multiplication factors applied to the initial
#' mum values.
#' @param mult_c Numeric vector of multiplication factors applied to the initial
#' C values.
#' @return Dataframe/ Plot.
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' init <- file.path(d, "INIT_VMPA_Jan2015.nc")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))
#'
#' data1 <- sc_init(init, prm_biol, fgs, bboxes)

#' \dontrun{
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' fgs <- "functionalGroups.csv"
#' init <- "init_simple_NorthSea.nc"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
#' mult_mum <- seq(0.5, 10, by = 1)
#' mult_c <- seq(0.5, 10, by = 1)
#' no_avail <- FALSE
#' save_to_disc <- FALSE
#' data1 <- sc_init(dir, init, prm_biol, fgs, bboxes, save_to_disc = FALSE)
#' plot_sc_init(df = data1, mult_mum, mult_c)
#' plot_sc_init(df = data1, mult_mum, mult_c, pred = "Cod")
#'
#' data2 <- sc_init(dir, init, prm_biol, fgs, bboxes, pred = "Cod", save_to_disc = FALSE)
#' plot_sc_init(df = data2, mult_mum, mult_c)
#' }

#' @export

# AEEC debuging
# dir <- "c:/backup_z/Atlantis_models/AEECmodel/"
# init = "AEEC35_Fcalibrated.nc"
# prm_biol = "AEEC35_setas_biol_marie.prm"
# fgs = "SETasGroups.csv"
# bboxes = get_boundary(load_box(dir = dir, bgm = "poly_atlantisEC35_projETRS89_LAEA_snapped0p002.bgm"))
# mult_mum = seq(0.5, 1.5, by = 0.1)
# mult_c = seq(0.5, 1.5, by = 0.1)
# pred <- get_age_acronyms(dir = dir, fgs = fgs)
# pred <- pred[!pred %in% c("SB", "CET", "SXX", "SHK")]
# sc_init(dir, nc, init, prm_biol, fgs, bboxes, pred = pred, no_avail = T)

# GNS debuging
# dir <- "c:/backup_z/Atlantis_models/baserun/"
# init = "init_NorthSea.nc"
# prm_biol = "NorthSea_biol_fishing.prm"
# fgs = "functionalGroups.csv"
# bboxes = get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))

# mult_mum = seq(0.5, 1.5, by = 0.1)
# mult_c = seq(0.5, 1.5, by = 0.1)
# pred <- get_age_acronyms(dir = dir, fgs = fgs)
# pred <- pred[!pred %in% c("SB", "CET", "SXX", "SHK")]
# sc_init(dir, nc, init, prm_biol, fgs, bboxes, pred = pred, no_avail = T)

# function start
sc_init <- function(init, prm_biol, fgs, bboxes, pred = NULL, set_avail = NULL, version_flag = 2) {
  fgs_data <- load_fgs(fgs = fgs)

  if (is.null(pred)) {
    acr_age <- get_age_acronyms(fgs = fgs)
  } else {
    acr_age <- fgs_data$Code[is.element(fgs_data$LongName, pred)]
    if (length(acr_age) == 0) stop("Please provide pred as LongName.")
    if (length(acr_age) != length(pred)) stop("Not all predators present in functionalGroups file")
  }

  groups <- get_groups(fgs = fgs)
  groups_age <- get_age_groups(fgs = fgs)
  groups_stanza <- fgs_data$Name[fgs_data$NumCohorts == 2]
  groups_rest <- groups[!is.element(groups, c(groups_age, groups_stanza))]
  maxl <- max(get_layers(init = init), na.rm = TRUE) + 1

  message("Read in data from out.nc, init.nc and prm.biol!")
  # Extract volume per box and layer!
  vol <- load_init_physics(init = init, select_variable = "volume", bboxes = bboxes)

  surface <- vol %>%
    dplyr::filter_(~layer != maxl) %>%
    agg_data(col = "layer", groups = "polygon", fun = max, out = "layer")

  vol <- vol %>%
    dplyr::inner_join(surface, by = c("polygon", "layer")) %>%
    dplyr::rename_(.dots = c("vol" = "atoutput")) %>%
    dplyr::select_(quote(-variable))

  # Extract data for age based groups
  pd1 <- prm_to_df(prm_biol  = prm_biol, fgs = fgs, group = acr_age,
                   parameter = c("KWRR", "KWSR", "AgeClassSize", "age_mat"))
  pd2 <- prm_to_df_ages(prm_biol = prm_biol, fgs = fgs, group = acr_age, parameter = c("mum", "C"))
  pd <- dplyr::left_join(pd1, pd2, by = c("species"))
  pd <- split(pd, pd$species)

  weights <- load_init_weight(init = init, fgs = fgs, bboxes = bboxes)

  # Calculate weight difference from one ageclass to the next!
  for (i in seq_along(pd)) {
    pd[[i]] <- dplyr::left_join(pd[[i]], weights, by = c("species", "agecl"))
    pd[[i]]$wdiff <- c((pd[[i]]$rn[1] + pd[[i]]$sn[1]) - (pd[[i]]$kwrr[1] + pd[[i]]$kwsr[1]),
                       diff(pd[[i]]$rn + pd[[i]]$sn))
  }
  pd <- do.call(rbind, pd)
  pd$pred_stanza <- ifelse(pd$agecl < pd$age_mat, 1, 2)
  pd$growth_req <- pd$wdiff / (365 *pd$ageclasssize)
  if (any(pd$growth_req < 0)) {
    warning("Required growth negative for some groups. Please check your initial conditions files.")
  }

  # Extract assimilation efficiencies per predator group!
  asseff <- prm_to_df(prm_biol = prm_biol, fgs = fgs, group = acr_age, parameter = c("E", "EPlant", "EDL", "EDR")) %>%
    tidyr::gather_(key_col = "ass_type", value_col = "asseff", gather_cols = names(.)[names(.) != "species"])

  # Extract prey densities!
  # Convert numbers to biomass density! --> distribute over watercolumn!
  # Calculate prey density per stanza!

  # Comment in to extract numbers from output!
  # nums <- load_nc(dir = dir, nc = nc, bps = bps, select_variable = "Nums",
  #                 fgs = fgs, select_groups = groups_age, bboxes = bboxes) %>%
  #   dplyr::filter(time == 0)
  nums <- load_init_age(init = init, fgs = fgs, select_variable = "Nums", bboxes = bboxes) %>%
    dplyr::inner_join(surface, by = c("polygon", "layer")) # not needed in case numbers are already only in surface in init file

  # Add stanzas for all age-based groups!
  all_age_acr <- get_age_acronyms(fgs = fgs)
  stanzas <- prm_to_df(prm_biol = prm_biol, fgs = fgs, group = all_age_acr, parameter = "age_mat")
  nums <- dplyr::left_join(nums, stanzas, by = "species")
  nums$prey_stanza <- ifelse(nums$agecl < nums$age_mat, 1, 2)

  preydens_ages <- nums %>%
    dplyr::left_join(weights, by = c("species", "agecl")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~(rn + sn) * atoutput), "atoutput")) %>%
    agg_data(groups = c("species", "polygon", "prey_stanza", "layer"), fun = sum) %>%
    dplyr::left_join(vol, by = c("polygon", "layer")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~atoutput / vol), "atoutput")) %>%
    dplyr::select(-vol) %>%
    dplyr::ungroup()
  # Get nitrogen desity for non age based groups and combine with age based data

  # Comment in to get data from output file
  # preydens_invert <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_rest,
  #                            select_variable = "N", bboxes = bboxes) %>%
  #   dplyr::filter(time == 0)
  preydens_invert <- load_init_nonage(init = init, fgs = fgs, select_groups = groups_rest,
                                      bboxes = bboxes, bps = load_bps(fgs = fgs, init = init))
  if (length(groups_stanza) > 0) {
    # calculate mean density over stanzas (This is used as hotfix...)
    preydens_stanza <- load_init_stanza(init = init, fgs = fgs, select_groups = groups_stanza, bboxes = bboxes) %>%
      agg_data(groups = c("polygon", "layer", "species"), fun = mean)
    preydens_invert <- dplyr::bind_rows(preydens_invert, preydens_stanza)
  }

  preydens_invert <- preydens_invert %>%
    dplyr::mutate(prey_stanza = 2) %>%
    dplyr::inner_join(surface, by = c("polygon", "layer"))
    # dplyr::select_(.dots = names(.)[!names(.) %in% "agecl"]) # only remove column "agecl" if present!
  preydens <- rbind(preydens_ages, preydens_invert) %>%
    dplyr::rename_(.dots = c("prey" = "species", "preydens" = "atoutput"))

  # Extract availability matrix and combine with assimilation types
  ass_type <- dplyr::select_(fgs_data, .dots = c("Code", names(fgs_data)[is.element(names(fgs_data), c("GroupType", "InvertType"))]))
  names(ass_type) <- c("prey", "grp")
  ass_type$ass_type <- "e"
  ass_type$ass_type[ass_type$grp == "LAB_DET"] <- "edl"
  ass_type$ass_type[ass_type$grp == "REF_DET"] <- "edr"
  # NOTE: This may not work for all models out there!
  ass_type$ass_type[unlist(sapply(c("PHY", "SEAGRAS"), grep, x = ass_type$grp))] <- "eplant"
  ass_type$grp <- NULL
  ass_type$prey <- convert_factor(data_fgs = fgs_data, col = ass_type$prey)

  dm <- load_dietmatrix(prm_biol = prm_biol, fgs = fgs, convert_names = TRUE, version_flag = version_flag) %>%
    dplyr::filter_(~avail != 0) %>%
    dplyr::left_join(ass_type, by = "prey")
  if (!is.null(set_avail)) dm$avail <- set_avail
  ## Combine everything to one dataframe!
  result <- dplyr::select_(pd, .dots = c("species", "agecl", "pred_stanza")) %>%
    dplyr::left_join(asseff, by = "species") %>%
    dplyr::inner_join(dm, by = c("species" = "pred", "pred_stanza", "ass_type")) %>%
    dplyr::inner_join(preydens, by = c("prey_stanza", "prey")) %>% # only use prey items which are consumed (e.g. no juvenile inverts)
    dplyr::rename_(.dots = c("pred" = "species")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(~preydens * avail * asseff), "atoutput")) %>% # available biomass
    agg_data(groups = c("pred", "agecl", "polygon", "layer"), out = "availbio", fun = sum) %>% # sum up per pred/agcl/time/box/layer
    dplyr::ungroup()

  # Add mum and C
  pd <- dplyr::rename_(pd, .dots = c("pred" = "species")) %>%
    dplyr::select_(.dots = c("pred", "agecl", "mum", "c", "growth_req"))
  result <- dplyr::left_join(result, pd, by = c("pred", "agecl"))

  return(result)
}

#' @export
#' @rdname sc_init
plot_sc_init <- function(df, mult_mum, mult_c, pred = NULL) {
  if (!is.null(pred)) df <- df[is.element(df$pred, pred), ]

  calc_growth <- function(df, mult_mum, mult_c) {
    result <- df %>%
      dplyr::mutate_(.dots = stats::setNames(list(~mum * mult_mum), "mum")) %>%
      dplyr::mutate_(.dots = stats::setNames(list(~c * mult_c), "c")) %>%
      dplyr::filter_(~!is.na(availbio)) %>% # Only needed in case data is read in from init
      dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~c * availbio / (x + c / mum * availbio), x = 1)), "atoutput")) %>%  # calculate realised growth rate
      agg_data(groups = c("pred", "agecl", "growth_req"), out = "growth_feed", fun = mean) # mean over spatial domain
    return(result)
  }

  mult1 <- rep(mult_mum, each = length(mult_c))
  mult2 <- rep(mult_c, times = length(mult_mum))
  mults <- data.frame(id = 1:length(mult1), mult_mum = mult1, mult_c = mult2)

  # Would have liked to do this with Map but it does not work....
   result <- vector(mode = "list", length = length(mult1))
  for (i in seq_along(result)) {
    dd          <- calc_growth(df = df, mult_mum = mult1[i], mult_c = mult2[i])
    dd$id       <- i
    result[[i]] <- dd
  }
  result <- do.call(rbind, result) %>%
    dplyr::left_join(mults, by = "id") %>%
    dplyr::mutate_(.dots = stats::setNames(list(~growth_feed / growth_req), "rel_growth"))

  plot <- ggplot2::ggplot(result, ggplot2::aes_(x = ~mult_mum, y = ~mult_c, fill = ~rel_growth)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    # ggplot2::geom_tile() +
    ggplot2::facet_wrap(~pred + agecl, labeller = ggplot2::label_wrap_gen(width = 15, multi_line = FALSE)) +
    ggplot2::scale_fill_gradient("growth real / growth req.", low = "red", high = "green") +
    ggplot2::labs(x = "mult.factor MUM", y = "mult. factor C") +
    theme_atlantis(scale_font = 0.8)

  plot <- ggplot_custom(plot)

  return(plot)
}
