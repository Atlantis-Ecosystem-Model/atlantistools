#' Read in the atlantis dietcheck.txt file and perform some basic data transformations.
#'
#' @inheritParams load_nc
#' @inheritParams load_fgs
#' @inheritParams load_dietmatrix
#' @param dietcheck Character string giving the connection of the dietcheck file.
#' The filename usually contains \code{Dietcheck} and ends in \code{.txt}".
#' @param report Logical indicating if incomplete DietCheck information shall
#' be printed \code{TRUE} or not \code{FALSE}.
#'
#' @family load functions
#' @export
#' @return A \code{data.frame} in long format with the following column names:
#'   time, pred, habitat, prey and atoutput (i.e., variable).
#'
#' @examples
#' # Apply to bec-dev models.
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' dietcheck <- file.path(d, "outputSETASDietCheck.txt")
#' fgs <- file.path(d, "SETasGroups.csv")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
#'
#' diet <- load_dietcheck(dietcheck, fgs, prm_run, version_flag = 1)
#' head(diet, n = 10)
#'
#' # Apply to trunk models.
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' dietcheck <- file.path(d, "outputSETASDietCheck.txt")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")

#' diet <- load_dietcheck(dietcheck, fgs, prm_run)
#' head(diet, n = 10)

#BJS 7/6/16 change to be compatible with trunk version; added version_flag
load_dietcheck <- function(dietcheck, fgs, prm_run, convert_names = FALSE, report = FALSE, version_flag = 2) {
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
        tidyr::spread_(data = ., key_col = colnames(diet)[2], value_col = "out") %>%
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
  names(diet_long)[names(diet_long) == "Predator"] <- "pred" #bjs predator -> colnames(diet)[2]

  if (version_flag == 2) {
    names(diet_long)[names(diet_long) == "Cohort"] <- "agecl" #bjs cohort -> colnames(diet)[3]
    # Column Updated was added to runk code.
    if ("Updated" %in% names(diet_long)) {
      diet_long <- diet_long[-which(diet_long$prey == "Updated"), ]
    }
  }

  names(diet_long) <- tolower(names(diet_long))

  # Remove entries without spefific diet information
  diet_long <- diet_long[diet_long$atoutput != 0, ]

  # Convert species codes to longnames!
  if (convert_names) {
    diet_long <- dplyr::mutate_at(diet_long, .cols = c("pred", "prey"), .funs = convert_factor,
                                  data_fgs = load_fgs(fgs = fgs))
  }

  # Convert timestep to time in years!
  diet_long$time <- convert_time(prm_run = prm_run, col = diet_long$time)


  return(diet_long)
}


