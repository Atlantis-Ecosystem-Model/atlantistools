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
  acr_age <- get_age_acronyms(dir = dir, fgs = fgs)
  bps <- load_bps(dir = dir, fgs = fgs, init = init)

  groups <- get_groups(dir = dir, fgs = fgs)
  groups_age <- get_age_groups(dir = dir, fgs = fgs)
  groups_rest <- groups[!is.element(groups, groups_age)]

  get_pred_data <- function(dir, prm_biol, predacr) {
    mum <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = paste0("mum_", predacr))[1, ]
    c <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = paste0("C_", predacr))[1, ]
    prms <- vapply(paste0(c("E", "EPlant", "EDL", "EDR", "KWSR", "KWRR"), "_", predacr),
                   extract_prm, dir = dir, prm_biol = "NorthSea_biol_fishing.prm", numeric(1), USE.NAMES = FALSE)
    prms <- c(prms, extract_prm(dir = dir, prm_biol = "NorthSea_biol_fishing.prm", variable = paste0(predacr, "_AgeClassSize")))

    df <- data.frame(acronym = predacr, mum = mum, c = c, stringsAsFactors = FALSE)
    df$agecl <- 1:nrow(df)
    df <- cbind(df, sapply(prms, rep, each = nrow(df)))
    names(df)[5:ncol(df)] <- c("e", "eplant", "edl", "edr", "kwrr", "kwsr", "acs")

    return(df)
  }

  # Extract data for age based groups
  weights <- load_init_weight(dir = dir, nc = init, fgs = fgs)
  pd <- lapply(acr_age, get_pred_data, dir = dir, prm_biol = prm_biol)
  nums <- load_nc(dir = dir, nc = nc, bps = bps, select_variable = "Nums", fgs = fgs, select_groups = groups_age, bboxes = bboxes) %>%
    dplyr::filter(time == 0)

  # Get nitrogen desity for non age based groups
  n <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_rest, select_variable = "N", bboxes = bboxes) %>%
    dplyr::filter(time == 0)

  # Extract volume per box and layer!
  vol <- load_nc_physics(dir = dir, nc = nc, select_physics = "volume", bboxes = bboxes, aggregate_layers = F) %>%
    dplyr::filter(time == 0)


  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs)

}



