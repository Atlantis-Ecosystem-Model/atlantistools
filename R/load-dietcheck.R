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
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   time, pred, habitat, prey and atoutput (i.e., variable).
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' diet <- load_dietcheck(dir = d,
#'     dietcheck = "outputSETASDietCheck.txt")
#' head(diet, n = 25)

load_dietcheck <- function(dir = getwd(), dietcheck) {
  dietcheck <- convert_path(dir = dir, file = dietcheck)
  if (!file.exists(dietcheck)) {
    stop(paste("File", dietcheck, "not found. Plase check parameters dir and dietcheck."))
  }

  # read in diet information
  diet <- read.table(file = dietcheck, header = TRUE, sep = " ", stringsAsFactors = FALSE)

  # Check if multiple stocks are available per functional group! Only used with trunc code!
#   if (all(diet$Stock) == 0) {
#     diet$Stock <- NULL
#   } else {
#     stop("Multiple stocks present. Dietcheck only works with 1 stock per funtional group.")
#   }

  # Cohorts start with 0 in DietCheck.txt! Only used with trunc code!
#   diet$Cohort <- diet$Cohort + 1

  # remove entries without any diet-information!
  diet <- diet[rowSums(x = diet[, 4:ncol(diet)]) != 0,]

  # Convert to long dataframe and rename columns!
  diet_long <- tidyr::gather_(data = diet, key_col = "prey", value_col = "diet", gather_cols = names(diet)[4:ncol(diet)])
  names(diet_long)[names(diet_long) == "Predator"] <- "pred"
  # Only used with trunc code!
  # names(diet_long)[names(diet_long) == "Cohort"] <- "agecl"
  names(diet_long) <- tolower(names(diet_long))

  # Remove entries without spefific diet information
  diet_long <- diet_long[diet_long$diet != 0, ]

  # Convert to percentages!
  diet_long <- agg_perc(data = diet_long, col = "diet", groups = c("time", "pred", "habitat"))

  # Combine prey groups with low contribution to the diet!
  # diet_long$prey[diet_long$atoutput <= combine_tresh] <- "Rest"
  # diet_long <- agg_sum(data = diet_long, groups = c("time", "pred", "habitat", "prey"))

  # diet_long <- convert_time(dir = dir, prm_run = prm_run, data = diet_long, modelstart = modelstart, stock_state = TRUE)

  # Add factors with pretty labels
  # fgs <- load_fgs(dir = dir, fgs = fgs)
  # diet_long$pred <- convert_factor(data_fgs = fgs, col = diet_long$pred, diet = TRUE)
  # diet_long$prey <- convert_factor(data_fgs = fgs, col = diet_long$prey, diet = TRUE)

  return(diet_long)
}


