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
#'     modelstart = "1991-01-01")
#' head(diet, n = 25)
#' str(diet)

# dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
# dietcheck <- "outputSETASDietCheck.txt"
# fgs <- "functionalGroups.csv"
# prm_run <- "VMPA_setas_run_fishing_F_Trunk.prm"
# modelstart <- "1991-01-01"

load_dietcheck <- function(dir, dietcheck, fgs, prm_run, modelstart) {
  dietcheck <- convert_path(dir = dir, file = dietcheck)
  if (!file.exists(dietcheck)) {
    stop(paste("File", dietcheck, "not found. Plase make sure to use "))
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

  # remove entries without diet-information!
  diet <- diet[rowSums(x = diet[, 4:ncol(diet)]) != 0,]

  # Convert to long dataframe and rename columns!
  diet_long <- tidyr::gather_(data = diet, key_col = "prey", value_col = "diet", gather_cols = names(diet)[4:ncol(diet)])
  names(diet_long)[names(diet_long) == "Group"] <- "pred"
  names(diet_long)[names(diet_long) == "Cohort"] <- "agecl"
  names(diet_long) <- tolower(names(diet_long))
  diet_long <- convert_time(dir = dir, prm_run = prm_run, data = diet_long, modelstart = modelstart, stock_state = TRUE)

  # Add factors with pretty labels
  fgs <- load_fgs(dir = dir, fgs = fgs)
  diet_long$pred <- factor(diet_long$pred)
  levels(diet_long$pred) <- fgs[sapply(levels(diet_long$pred), function(x) which(fgs$Code == x)), "LongName"]
  diet_long$prey <- factor(diet_long$prey)
  levels(diet_long$prey) <- fgs[sapply(levels(diet_long$prey), function(x) which(fgs$Code == x)), "LongName"]

  return(diet_long)
}
