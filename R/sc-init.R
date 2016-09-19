#' Sanity check initial conditions file
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param nc Character string giving the filename of the general netcdf
#' file Usually "output[...].nc". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc_gen. In addition set dir to 'NULL' in this
#' case.
#' @param init Character string giving the filename of the initial conditions netcdf
#' file Usually "init[...].nc".
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param bboxes Integer vector giving the box-id of the boundary boxes.
#' @param mult_mum Numeric vector of multiplication factors applied to the initial
#' mum values.
#' @param mult_c Numeric vector of multiplication factors applied to the initial
#' C values.
#' @param no_avail Boolean indicating if all availabilities should be set to
#' 1 \code{TRUE} or the actual values from the availability matrix are used
#' \code{FALSE}. Default is \code{FALSE}.
#' @return Named list with the dataframes as list entry saved as .Rda file.
#'
#' @examples
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' fgs <- "functionalGroups.csv"
#' init <- "init_simple_NorthSea.nc"
#' nc <- "outputNorthSea.nc"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
#' mult_mum <- c(0.5, 1, 1.5, 2, 5, 10, 25)
#' mult_c <- c(0.5, 1, 1.5, 2, 5, 10, 25)
#' no_avail <- FALSE
#' sc_init(dir, nc, init, prm_biol, fgs, bboxes, mult_mum, mult_c)

#' @export

# function start
sc_init <- function(dir = getwd(), nc, init, prm_biol, fgs, bboxes, mult_mum, mult_c, no_avail = FALSE) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  acr_age <- get_age_acronyms(dir = dir, fgs = fgs)
  bps <- load_bps(dir = dir, fgs = fgs, init = init)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  # Extract volume per box and layer!
  vol <- load_nc_physics(dir = dir, nc = nc, select_physics = "volume", bboxes = bboxes, aggregate_layers = F) %>%
    dplyr::filter(time == 0) %>%
    dplyr::rename(vol = atoutput) %>%
    dplyr::select(-variable)

  # Extract data for age based groups
  get_pred_data <- function(dir, prm_biol, predacr) {
    mum <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = paste0("mum_", predacr))[1, ]
    c <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = paste0("C_", predacr))[1, ]
    # flag: parameter_XXX
    prms1 <- vapply(paste0(c("KWSR", "KWRR"), "_", predacr),
                    extract_prm, dir = dir, prm_biol = "NorthSea_biol_fishing.prm", numeric(1), USE.NAMES = FALSE)
    # flag: XXX_parameter
    prms2 <- vapply(paste0(predacr, "_", c("AgeClassSize", "age_mat")),
                    extract_prm, dir = dir, prm_biol = "NorthSea_biol_fishing.prm", numeric(1), USE.NAMES = FALSE)
    prms <- c(prms1, prms2)

    df <- data.frame(species = predacr, mum = mum, c = c, stringsAsFactors = FALSE) %>%
      dplyr::mutate(agecl = 1:nrow(.)) %>%
      cbind(sapply(prms, rep, each = nrow(.)))
    names(df)[5:ncol(df)] <- c("kwsr", "kwrr", "acs", "ageclmat")

    return(df)
  }

  weights <- load_init_weight(dir = dir, nc = init, fgs = fgs)
  weights$species <- convert_factor(fgs_data, col = weights$species)
  pd <- lapply(acr_age, get_pred_data, dir = dir, prm_biol = prm_biol)
  # Calculate weight difference from one ageclass to the next!
  for (i in seq_along(pd)) {
    pd[[i]]$species <- convert_factor(fgs_data, col = pd[[i]]$species)
    pd[[i]] <- dplyr::left_join(pd[[i]], weights, by = c("species", "agecl"))
    pd[[i]]$wdiff <- c((pd[[i]]$rn[1] + pd[[i]]$sn[1]) - (pd[[i]]$kwrr[1] + pd[[i]]$kwsr[1]),
                       diff(pd[[i]]$rn + pd[[i]]$sn))
  }
  pd <- do.call(rbind, pd)
  pd$pred_stanza <- ifelse(pd$agecl < pd$ageclmat, 1, 2)
  pd$growth_req <- pd$wdiff / (365 *pd$acs)

  # get_vert_distrib <- function(dir, predacr, prm_biol, nc) {
  #   tags <- as.vector(outer(X = predacr, Y = 1:2, FUN = paste0))
  #   df <- as.data.frame(sapply(paste("VERTday", tags, sep = "_"),
  #                              extract_prm_cohort, dir = dir, prm_biol = prm_biol))
  #   names(df) <- tags
  #   df <- tidyr::gather(df, key = "species", value = "vdistrib") %>%
  #     dplyr::mutate(pred_stanza = as.numeric(stringr::str_sub(species, start = -1))) %>%
  #     dplyr::mutate(species = stringr::str_sub(species, end = stringr::str_length(species) - 1))
  #
  #   return(df)
  # }
  #
  # vdistrib <- get_vert_distrib(dir = dir, predacr = acr_age, prm_biol = prm_biol)
  # vdistrib$species <- convert_factor(fgs_data, col = vdistrib$species)
  # NOTE: STill need to combine vdistrib to the rest of the dataframes!

  get_ass_eff <- function(dir, prm_biol, predacr) {
    # Assimilation efficiencies
    asseff <- vapply(paste0(c("E", "EPlant", "EDL", "EDR"), "_", predacr),
                     extract_prm, dir = dir, prm_biol = "NorthSea_biol_fishing.prm", numeric(1), USE.NAMES = FALSE)
    asseff <- data.frame(species = predacr,
                         ass_type = c("live", "plant", "lab_det", "ref_det"),
                         asseff = asseff, stringsAsFactors = FALSE)
    return(asseff)
  }

  asseff <- do.call(rbind, lapply(acr_age, get_ass_eff, dir = dir, prm_biol = prm_biol))
  asseff$species <- convert_factor(fgs_data, col = asseff$species)

  # Extract prey densities!
  # Convert numbers to biomass density! --> distribute over watercolumn!
  # Calculate prey density per stanza!
  nums <- load_nc(dir = dir, nc = nc, bps = bps, select_variable = "Nums",
                  fgs = fgs, select_groups = groups_age, bboxes = bboxes) %>%
    dplyr::filter(time == 0) %>%
    dplyr::mutate(species = convert_factor(fgs_data, col = species)) %>%
    dplyr::left_join(unique(dplyr::select(pd, species, agecl, pred_stanza))) %>%
    dplyr::left_join(asseff)

  preydens_ages <- nums %>%
    dplyr::rename(prey_stanza = pred_stanza) %>%
    dplyr::left_join(weights) %>%
    dplyr::mutate(atoutput = (rn + sn) * atoutput) %>%
    agg_data(groups = c("species", "polygon", "layer", "time", "prey_stanza"), fun = sum) %>%
    dplyr::left_join(vol) %>%
    dplyr::mutate(atoutput = atoutput / vol) %>%
    dplyr::select(-vol) %>%
    dplyr::ungroup()
  # dens <- dplyr::left_join(nums, weights) %>%
  #   dplyr::left_join(vol) %>%
  #   dplyr::mutate(atoutput = (rn + sn) * atoutput / vol) %>%
  #   dplyr::select(-rn, -sn, -vol)
  # Get nitrogen desity for non age based groups and combine with age based data
  preydens_invert <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_rest,
                             select_variable = "N", bboxes = bboxes) %>%
    dplyr::filter(time == 0) %>%
    dplyr::mutate(prey_stanza = 2) %>%
    dplyr::select(-agecl) %>%
    dplyr::mutate(species = convert_factor(data_fgs = fgs_data, col = species))
  preydens <- rbind(preydens_ages, preydens_invert) %>%
    dplyr::rename(prey = species, preydens = atoutput)

  # Extract availability matrix and combine with assimilation types
  ass_type <- dplyr::select_(fgs_data, .dots = c("Code", names(fgs_data)[is.element(names(fgs_data), c("GroupType", "InvertType"))]))
  names(ass_type) <- c("prey", "grp")
  ass_type$ass_type <- "live"
  ass_type$ass_type[ass_type$grp == "LAB_DET"] <- "lab_det"
  ass_type$ass_type[ass_type$grp == "REF_DET"] <- "ref_det"
  # NOTE: This may not work for all models out there!
  ass_type$ass_type[unlist(sapply(c("PHY", "SEAGRAS"), grep, x = ass_type$grp))] <- "plant"
  ass_type$grp <- NULL

  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs) %>%
    dplyr::filter(is.element(pred, acr_age) & avail != 0) %>%
    dplyr::left_join(ass_type) %>%
    dplyr::mutate_at(.cols = c("pred", "prey"), .funs = convert_factor, data_fgs = fgs_data)
  if (no_avail) dm$avail <- 1
  # dm <- split(dm, dm$pred)
  # dm <- dm[acr_age]

  # Combine everything to one dataframe! For some reason old ageclasses aren't present...
  all_data <- dplyr::left_join(dm, nums, by = c("pred" = "species", "pred_stanza", "ass_type")) %>%
    dplyr::inner_join(preydens) %>%  # only use prey items which are consumed (e.g. no juvenile inverts)
    dplyr::left_join(dplyr::select(pd, pred = species, agecl, mum, c)) %>%
    dplyr::mutate(atoutput = preydens * avail * asseff) %>% # available biomass
    agg_data(groups = c("pred", "agecl", "time", "polygon", "layer", "mum", "c"), out = "availbio", fun = sum) # sum up per pred/agcl/time/box/layer

  calc_growth <- function(df, mult_mum, mult_c) {
    result <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(mum = mum * mult_mum) %>%
      dplyr::mutate(c = c * mult_c) %>%
      dplyr::mutate(atoutput = c * availbio / (1 + c / mum * availbio)) %>%  # calculate realised growth rate
      agg_data(groups = c("pred", "agecl", "time"), out = "growth_feed", fun = mean) # mean over spatial domain
    return(result)
  }

  mult1 <- rep(mult_mum, each = length(mult_c))
  mult2 <- rep(mult_c, times = length(mult_mum))
  mults <- data.frame(id = 1:length(mult1), mult_mum = mult1, mult_c = mult2)

  # Would have liked to do this with Map but it does not work....
  result <- vector(mode = "list", length = length(mult1))
  for (i in seq_along(result)) {
    dd <- calc_growth(df = all_data, mult_mum = mult1[i], mult_c = mult2[i])
    dd$id <- i
    result[[i]] <- dd
  }
  result <- do.call(rbind, result) %>%
    dplyr::left_join(mults) %>%
    dplyr::left_join(dplyr::select(pd, pred = species, agecl, growth_req)) %>%
    dplyr::mutate(rel_growth = growth_feed / growth_req)

  plot <- ggplot2::ggplot(result, ggplot2::aes(x = factor(mult_mum), y = factor(mult_c), fill = rel_growth)) +
    ggplot2::geom_tile() +
    ggplot2::facet_grid(agecl ~ pred) +
    ggplot2::scale_fill_gradient(low = "red", high = "green") +
    theme_atlantis()

  return(plot)
}



