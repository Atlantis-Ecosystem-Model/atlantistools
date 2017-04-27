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
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' In addition set dir to 'NULL' in this case.
#' @param convert_names Logical indicating if group codes are transformed to LongNames (\code{TRUE})
#' or not (default = \code{FALSE}).
#' @param report Logical indicating if incomplete DietCheck information shall
#' be printed \code{TRUE} or not \code{FALSE}.
#' @param version_flag The version of atlantis that created the output files. 1 for bec_dev, 2 for trunk.
#'
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following coumn names:
#'   time, pred, habitat, prey and atoutput (i.e., variable).
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' diet <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt",
#'                        fgs = "SETasGroups.csv",
#'                        prm_run = "VMPA_setas_run_fishing_F_New.prm")
#' head(diet, n = 10)
#'
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' diet <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt",
#'                        fgs = "SETasGroupsDem_NoCep.csv",
#'                        prm_run = "VMPA_setas_run_fishing_F_Trunk.prm")
#' head(diet, n = 10)

#BJS 7/6/16 change to be compatible with trunk version; added version_flag
load_dietcheck <- function(dir = getwd(), dietcheck, fgs, prm_run, convert_names = FALSE, report = FALSE, version_flag = 1) {
    dietcheck <- convert_path(dir = dir, file = dietcheck)
  if (!file.exists(dietcheck)) {
    stop(paste("File", dietcheck, "not found. Plase check parameters dir and dietcheck."))
  }

  # read in diet information
  diet <- utils::read.table(file = dietcheck, header = TRUE, sep = " ", stringsAsFactors = FALSE)

  #Check if multiple stocks are available per functional group for trunk branch!
  if (version_flag == 2) {
    if (all(diet$Stock) == 0) {
      diet$Stock <- NULL
    } else {
      stop("Multiple stocks present. Dietcheck only works with 1 stock per funtional group.")
    }

    diet$Cohort <- diet$Cohort + 1 # Cohorts start with 0 in DietCheck.txt!
  }

  prey_col_start <- 4 #bjs remove magic number below



  # remove entries without any diet-information!
  empty_rows <- rowSums(x = diet[, prey_col_start:ncol(diet)]) == 0

  #BJS change predator/habitat to colnames(diet)[*], generalizes code to work with both trunk and bec_dev
  #This is only being used if report==TRUE so no need to do the calculations if report flag is false
  # Create intermediate dataframe to print predators without diet information!
  if (report) {
    print_diet <- diet[empty_rows, c("Time", colnames(diet)[2], colnames(diet)[3])] %>%
      dplyr::group_by_(stats::as.formula(paste0("~", colnames(diet)[2])), stats::as.formula(paste0("~", colnames(diet)[3]))) %>%

      dplyr::summarise_(out = ~dplyr::n_distinct(Time)) %>%
      dplyr::filter_(~out != 1)

    if (nrow(print_diet) != 0) {
      print_diet <- print_diet %>%
        dplyr::mutate_(out = ~out / length(unique(diet$Time)) * 100) %>%
        #BJS predator -> colnames(diet)[2]
        tidyr::spread_(key_col = colnames(diet)[2], value_col = "out") %>%
        as.data.frame()


      warning("Incomplete diet information.\n% timesteps without any diet information per predator.", immediate. = TRUE)
      print(print_diet)
    }
  }

  diet <- diet[!empty_rows, ]

  # Convert to long dataframe and rename columns!
  #bjs change 4 to prey_col_start to remove magic number
  diet_long <- tidyr::gather_(data = diet, key_col = "prey", value_col = "atoutput",
                              gather_cols = names(diet)[prey_col_start:ncol(diet)])
  names(diet_long)[names(diet_long) == colnames(diet)[2]] <- "pred" #bjs predator -> colnames(diet)[2]

  if(version_flag == 2) {
    names(diet_long)[names(diet_long) == colnames(diet)[3]] <- "agecl" #bjs cohort -> colnames(diet)[3]
    diet_long <- diet_long[-which(diet_long$prey == "Updated"),]
  }

  names(diet_long) <- tolower(names(diet_long))

  # Remove entries without spefific diet information
  diet_long <- diet_long[diet_long$atoutput != 0, ]

  # Convert species codes to longnames!
  if (convert_names) {
    diet_long <- dplyr::mutate_at(diet_long, .cols = c("pred", "prey"), .funs = convert_factor,
                                  data_fgs = load_fgs(dir = dir, fgs = fgs))
  }

  # Convert timestep to time in years!
  diet_long$time <- convert_time(dir = dir, prm_run = prm_run, col = diet_long$time)


  return(diet_long)
}


