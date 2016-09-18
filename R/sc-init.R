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
#' @return Named list with the dataframes as list entry saved as .Rda file.
#'
#' @examples

#' @export
#' dir <- system.file("extdata", "gns", package = "atlantistools")
#' fgs <- "functionalGroups.csv"
#' init <- "init_simple_NorthSea.nc"
#' nc <- "outputNorthSea.nc"
#' prm_biol <- "NorthSea_biol_fishing.prm"
#' bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))

# function start
sc_init <- function(dir = getwd(), nc, init, prm_biol, fgs, bboxes) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  acr_age <- get_age_acronyms(dir = dir, fgs = fgs)
  bps <- load_bps(dir = dir, fgs = fgs, init = init)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  get_pred_data <- function(dir, prm_biol, predacr) {
    mum <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = paste0("mum_", predacr))[1, ]
    c <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = paste0("C_", predacr))[1, ]
    # flag: parameter_XXX
    prms1 <- vapply(paste0(c("E", "EPlant", "EDL", "EDR", "KWSR", "KWRR"), "_", predacr),
                    extract_prm, dir = dir, prm_biol = "NorthSea_biol_fishing.prm", numeric(1), USE.NAMES = FALSE)
    # flag: XXX_parameter
    prms2 <- vapply(paste0(predacr, "_", c("AgeClassSize", "age_mat")),
                    extract_prm, dir = dir, prm_biol = "NorthSea_biol_fishing.prm", numeric(1), USE.NAMES = FALSE)

    prms <- c(prms1, prms2)

    df <- data.frame(species = predacr, mum = mum, c = c, stringsAsFactors = FALSE)
    df$agecl <- 1:nrow(df)
    df <- cbind(df, sapply(prms, rep, each = nrow(df)))
    names(df)[5:ncol(df)] <- c("e", "eplant", "edl", "edr", "kwrr", "kwsr", "acs", "ageclmat")

    return(df)
  }

  # Extract volume per box and layer!
  vol <- load_nc_physics(dir = dir, nc = nc, select_physics = "volume", bboxes = bboxes, aggregate_layers = F) %>%
    dplyr::filter(time == 0) %>%
    dplyr::rename(vol = atoutput) %>%
    dplyr::select(-variable)

  # Extract data for age based groups
  weights <- load_init_weight(dir = dir, nc = init, fgs = fgs)
  weights$species <- convert_factor(fgs_data, col = weights$species)
  pd <- do.call(rbind, lapply(acr_age, get_pred_data, dir = dir, prm_biol = prm_biol))
  pd$species <- convert_factor(fgs_data, col = pd$species)
  pd <- dplyr::left_join(pd, weights)
  nums <- load_nc(dir = dir, nc = nc, bps = bps, select_variable = "Nums", fgs = fgs, select_groups = groups_age, bboxes = bboxes) %>%
    dplyr::filter(time == 0)
  nums$species <- convert_factor(fgs_data, col = nums$species)

  get_vert_distrib <- function(dir, predacr, prm_biol, nc) {
    tags <- as.vector(outer(X = predarc, Y = 1:2, FUN = paste0))
    df <- as.data.frame(sapply(paste("VERTday", tags, sep = "_"),
                               extract_prm_cohort, dir = dir, prm_biol = prm_biol))
    names(df) <- tags
    df <- tidyr::gather(df, key = "species", value = "vdistrib") %>%
      dplyr::mutate(pred_stanza = as.numeric(stringr::str_sub(species, start = -1))) %>%
      dplyr::mutate(species = stringr::str_sub(species, end = stringr::str_length(species) - 1))
    nl <- df %>%
      dplyr::group_by(species, pred_stanza) %>%
      dplyr::summarise(nl =

    df$species <- stringr::str_sub(df$species, end = stringr::str_length(df$species) - 1)
    return(df)
  }

  vdistrib <- get_vert_distrib(dir = dir, predacr = acr_age, prm_biol = prm_biol)
  vdistrib$species <- convert_factor(fgs_data, col = vdistrib$species)

  # Convert numbers to biomass density! --> distribute over watercolumn!
  dens <- dplyr::left_join(nums, weights) %>%
    dplyr::left_join(vol) %>%
    dplyr::mutate(atoutput = (rn + sn) * atoutput / vol) %>%
    dplyr::select(-rn, -sn, -vol)

  # Get nitrogen desity for non age based groups and combine with age based data
  n <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_rest, select_variable = "N", bboxes = bboxes) %>%
    dplyr::filter(time == 0) %>%
    rbind(dens)

  # Extract availability matrix and split into predator groups!
  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs) %>%
    dplyr::filter(is.element(pred, acr_age) & avail != 0)
  dm <- split(dm, dm$pred)
  dm <- dm[acr_age]

  # Calculate available prey biomass per predator
  calc_avail <- function(agegr, nagegr) {

  }

}



