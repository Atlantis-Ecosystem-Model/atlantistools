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
#' @param report Logical indicating if incomplete DietCheck information shall
#' be printed \code{TRUE} or not \code{FALSE}.
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   time, pred, habitat, prey and atoutput (i.e., variable).
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' diet <- load_dietcheck(dir = d,
#'     dietcheck = "outputSETASDietCheck.txt")
#' head(diet, n = 10)

load_dietcheck <- function(dir = getwd(), dietcheck, report = TRUE) {
  dietcheck <- convert_path(dir = dir, file = dietcheck)
  if (!file.exists(dietcheck)) {
    stop(paste("File", dietcheck, "not found. Plase check parameters dir and dietcheck."))
  }

  # read in diet information
  diet <- utils::read.table(file = dietcheck, header = TRUE, sep = " ", stringsAsFactors = FALSE)

  # Check if multiple stocks are available per functional group! Only used with trunc code!
  #   if (all(diet$Stock) == 0) {
  #     diet$Stock <- NULL
  #   } else {
  #     stop("Multiple stocks present. Dietcheck only works with 1 stock per funtional group.")
  #   }

  # Cohorts start with 0 in DietCheck.txt! Only used with trunc code!
  # diet$Cohort <- diet$Cohort + 1

  # remove entries without any diet-information!
  empty_rows <- rowSums(x = diet[, 4:ncol(diet)]) == 0
  # Create intermediate dataframe to print predators without diet information!
  print_diet <- diet[empty_rows, c("Time", "Predator", "Habitat")] %>%
    dplyr::group_by_(~Predator, ~Habitat) %>%
    dplyr::summarise_(out = ~dplyr::n_distinct(Time)) %>%
    dplyr::filter_(~out != 1)
  if (nrow(print_diet) != 0) {
    print_diet <- print_diet %>%
      dplyr::mutate_(out = ~out / length(unique(diet$Time)) * 100) %>%
      tidyr::spread_(key_col = "Predator", value_col = "out") %>%
      as.data.frame()
    if (report) {
      warning("Incomplete diet information.\n% timesteps without any diet information per predator.")
      print(print_diet)
    }
  }

  diet <- diet[!empty_rows, ]

  # Convert to long dataframe and rename columns!
  diet_long <- tidyr::gather_(data = diet, key_col = "prey", value_col = "atoutput", gather_cols = names(diet)[4:ncol(diet)])
  names(diet_long)[names(diet_long) == "Predator"] <- "pred"
  # Only used with trunc code!
  # names(diet_long)[names(diet_long) == "Cohort"] <- "agecl"
  names(diet_long) <- tolower(names(diet_long))

  # Remove entries without spefific diet information
  diet_long <- diet_long[diet_long$atoutput != 0, ]

  # Convert to percentages!
  # diet_long <- agg_perc(data = diet_long, col = "diet", groups = c("time", "pred", "habitat"))

  return(diet_long)
}


