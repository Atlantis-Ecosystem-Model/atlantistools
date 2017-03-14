# This function is used to debug the NS model.
# Health care warning: Heavy use of the assign function...
# Variables are created in the GlobalEnvironment...
# debug_ns <- function() {
#   d <- "z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/"
#   assign("dir", d, envir = .GlobalEnv)
#   assign("fgs", "functionalGroups.csv", envir = .GlobalEnv)
#   assign("nc", "outputNorthSea.nc", envir = .GlobalEnv)
#   assign("nc_gen", "outputNorthSea.nc", envir = .GlobalEnv)
#   assign("nc_prod", "outputNorthSeaPROD.nc", envir = .GlobalEnv)
#   assign("dietcheck", "outputNorthSeaDietCheck.txt", envir = .GlobalEnv)
#   assign("yoy", "outputNorthSeaYOY.txt", envir = .GlobalEnv)
#   assign("ssb", "outputNorthSeaSSB.txt", envir = .GlobalEnv)
#   assign("specmort", "outputNorthSeaSpecificMort.txt", envir = .GlobalEnv)
#   assign("specpredmort", "outputNorthSeaSpecificPredMort.txt", envir = .GlobalEnv)
#   assign("prm_biol", "NorthSea_biol_fishing.prm", envir = .GlobalEnv)
#   assign("prm_run", "NorthSea_run_fishing_F.prm", envir = .GlobalEnv)
#   assign("bps", load_bps(dir = d, fgs = "functionalGroups.csv", init = "init_NorthSea.nc"), envir = .GlobalEnv)
#   assign("fgs", "functionalGroups.csv", envir = .GlobalEnv)
#   assign("select_groups", get_groups(dir = d, fgs = "functionalGroups.csv"), envir = .GlobalEnv)
#   assign("bboxes", get_boundary(load_box(dir = d, bgm = "NorthSea.bgm")), envir = .GlobalEnv)
#   assign("out", "preprocess-north-sea.rda", envir = .GlobalEnv)
#   assign("save_to_disc", FALSE, envir = .GlobalEnv)
#   assign("check_acronyms", TRUE, envir = .GlobalEnv)
#   assign("report", TRUE, envir = .GlobalEnv)
#   assign("warn_zeros", TRUE, envir = .GlobalEnv)
# }

# split dataframe in list of dataframe based on multiple (or a single) column.
multisplit <- function(df, groups) {
  for (i in seq_along(groups)) {
    # first split results in a list of dataframes!
    if (i == 1) result <- split(df, df[, groups[i]])
    # sucessive splits work with a list of dataframes.
    if (i > 1) {
      result <- purrr::map(result, function(x) split(x, x[, groups[i]]))
      # remove one dimension of the list to allow for the next split to work.
      result <- purrr::flatten(result)
    }
  }
  return(result)
}

# This function is not used anymore.
file_ending <- function(filename, ending = "nc") {
  file_ending <- strsplit(filename, "\\.")[[1]][length(strsplit(filename, "\\.")[[1]])]
  if (file_ending != ending) stop(paste("The file", filename, "does not end in", ending))
}
