#' Read in the atlantis dietcheck.txt file and perform some basic data transformations.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param dietcheck Character string of the DietCheck.txt file. Usually
#' 'output[...]DietCheck.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}.
#' @param combine_tresh Integer indicating minimum amount to the stomach contribution.
#' Each prey item with a lower contribution as this treshold is assigned to the
#' grpup "Rest". This is necessary for species with a wide variety of food items
#' in their diet. Otherwise the screen will be cluttered with colors in the
#' dietplots.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   species, time, agecl, and atoutput (i.e., variable).
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' diet <- load_dietcheck(dir = d,
#'     dietcheck = "outputSETASDietCheck.txt",
#'     fgs = "functionalGroups.csv",
#'     prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
#'     modelstart = "1991-01-01",
#'     combine_tresh = 0.03)
#' head(diet, n = 25)
#' str(diet)

load_dietcheck <- function(dir, dietcheck, fgs, prm_run, modelstart, combine_tresh) {
  dietcheck <- convert_path(dir = dir, file = dietcheck)
  if (!file.exists(dietcheck)) {
    stop(paste("File", dietcheck, "not found. Plase check parameters dir and dietcheck."))
  }

  # read in diet information
  diet <- read.table(file = dietcheck, header = TRUE, sep = " ", stringsAsFactors = FALSE)

  # Check if multiple stocks are available per functional group!
  if (all(diet$Stock) == 0) {
    diet$Stock <- NULL
  } else {
    stop("Multiple stocks present. Dietcheck only works with 1 stock per funtional group.")
  }

  # Cohorts start with 0 in DietCheck.txt!
  diet$Cohort <- diet$Cohort + 1

  # remove entries without any diet-information!
  diet <- diet[rowSums(x = diet[, 4:ncol(diet)]) != 0,]

  # Convert to long dataframe and rename columns!
  diet_long <- tidyr::gather_(data = diet, key_col = "prey", value_col = "diet", gather_cols = names(diet)[4:ncol(diet)])
  names(diet_long)[names(diet_long) == "Group"] <- "pred"
  names(diet_long)[names(diet_long) == "Cohort"] <- "agecl"
  names(diet_long) <- tolower(names(diet_long))

  # Remove entries without spefific diet information
  diet_long <- diet_long[diet_long$diet != 0, ]

  # Combine prey groups with low contribution to the diet!
  diet_long$prey[diet_long$diet <= combine_tresh] <- "Rest"
  diet_long <- dplyr::group_by_(diet_long, ~time, ~pred, ~agecl, ~prey)
  diet_long <- dplyr::summarise_(diet_long, diet = ~sum(diet))

  diet_long <- convert_time(dir = dir, prm_run = prm_run, data = diet_long, modelstart = modelstart, stock_state = TRUE)

  # Add factors with pretty labels
  fgs <- load_fgs(dir = dir, fgs = fgs)
  diet_long$pred <- convert_factor(data_fgs = fgs, col = diet_long$pred, diet = TRUE)
  diet_long$prey <- convert_factor(data_fgs = fgs, col = diet_long$prey, diet = TRUE)

  return(diet_long)
}


