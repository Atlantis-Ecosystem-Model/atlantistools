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
  maxl <- max(get_layers(dir = dir, init = init)) + 1

  # 1st step: Load in data!
  # - n per box and layer for each invert group
  # - nums, resn, structn for each vert group per box, layer and ageclass
  # - availability matrix
  at_structn_l <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_age,
                          select_variable = "StructN", bboxes = bboxes)

  at_resn_l    <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_age,
                          select_variable = "ResN", bboxes = bboxes)

  at_nums_l    <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_age,
                          select_variable = "Nums", bboxes = bboxes)

  at_n_pools   <- load_nc(dir = dir, nc = nc, bps = bps, fgs = fgs, select_groups = groups_rest,
                          select_variable = "N", bboxes = bboxes)

  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs)


  # 2nd step: Calculate relative biomass per box and layer per group and agecl

  # 3rd step: Calculate schoener index per pred / prey combination (including ageclasses)

  # 4th step: Aggregate schoner index based on availabilities

}

