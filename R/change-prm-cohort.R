#' Change biological parameterfile for parameters which expect multiple values.
#'
#'
#' This function is used to help automate the calibration routine for ATLANTIS models.
#'
#' @inheritParams extract_prm
#' @param select_acronyms Character vector of funtional groups which shall be read in.
#' Names have to match the ones used in the *.prm file. Check column "Code" in
#' "functionalGroups.csv" for clarification.
#' @param roc Matrix of multiplication factors which shall be applied to the old set of parameters.
#' Please supply one row per selected group. Each row should have as many entries as the
#' parameter itself. E.g. if you want to change the clearance rate for two fish groups you
#' need to supply a matrix with 2 rows and 10 columns. In case you use different cohort
#' numbers for age-structured groups supply a list of multiplication factors. Each list entry
#' should be group specific.
#' @param parameter Character value of the model parameter which shall be changed.
#' Only one parameter can be selected per function call.
#' @param relative Logical if TRUE values are changed relative to base values. If FALSE new values can
#' be passed directly. Default is \code{TRUE}.
#' @param save_to_disc Logical indicating if the resulting prm file should be overwritten
#' (\code{TRUE}) or not (\code{FALSE}). Default is \code{TRUE}.
#' @return parameterfile *.prm file with the new parameter values.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#'
#' new_prm <- change_prm_cohort(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
#'                              select_acronyms = c("FPS", "FVS"),
#'                              roc = matrix(rep(2, times = 20), nrow = 2, ncol = 10),
#'                              parameter = "C",
#'                              save_to_disc = FALSE)
#' # C_FPS is in line 640. Old values are 0.0002 0.3 0.6 0.6 0.6 0.6 0.5 0.5 0.4 and 0.4.
#' new_prm[640:641]
#' # C_FVS is in line 652. Old values are 40.0 40.0 40.0 120.0 150.0 250.0 250.0 300.0 300.0 and 300.0.
#' new_prm[652:653]
#'
#' # Also works for lists as argument
#' new_prm <- change_prm_cohort(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
#'                              select_acronyms = c("FPS", "FVS"),
#'                              roc = list(rep(3, times = 10), rep(2, times = 10)),
#'                              parameter = "C",
#'                              save_to_disc = FALSE)

# prm_biol <- file.path("Z:", "Atlantis_models", "Runs", "dummy_01_ATLANTIS_NS", "NorthSea_biol_fishing.prm")
# select_acronyms <- c("COD", "WHG")
# roc <- list(COD = 1:12,
#             HER = 1:8)

change_prm_cohort <- function(prm_biol, select_acronyms, roc, parameter, relative = TRUE, save_to_disc = TRUE) {
    if (length(parameter) != 1) stop("Please suply only one parameter per function call.")

    # Convert to matrix if only one species is selected and roc is a vector!
    if (length(select_acronyms) == 1 & is.vector(roc)) roc <- matrix(roc, nrow = 1)

    # Convert matrix to list (it is not possible to programm indexing for both lists and matrices).
    # Therefore we convert every user input to a list.
    if (is.matrix(roc)) roc <- lapply(seq_len(nrow(roc)), function(i) roc[i,])

    # Check if all rocs equal to 1! leave function in this case!
    if (all(sapply(roc, function(x) all(x == 1))) & relative) {
      message("All rocs 1 no changes applied to prm-file.")
    } else {
      if (length(select_acronyms) != length(roc)) {
        stop("Dimensions of select_acronyms and roc do not match. Please supply one row of values per group.")
      }

      # Read in parameter file!
      prm_biol_new <- readLines(con = prm_biol)

      # Function to update a specific parameter composed of a parameter string
      # a group acronym and a seperator (by default "_") found in a prm file.
      update_prm_species <- function(prm_biol, acronym, roc, parameter, relative) {
        if (parameter %in% c("mL", "mQ")) {
          flag <- paste(acronym, parameter, sep = "_") # only works with trunc code!
        } else {
          flag <- paste(parameter, acronym, sep = "_")
        }
        pos <- scan_prm(chars = prm_biol, variable = flag)
        # Values are stored in the next row in the *.prm file.
        pos <- pos + 1
        # In case row is commented out use next column!
        while (substr(prm_biol[pos], 1, 1) == "#") pos <- pos + 1

        # Keep all numeric values
        old_value <- str_split_twice(char = prm_biol[pos], min_only = FALSE)
        if (length(old_value) != length(roc)) {
          stop(paste(length(old_value), "values found but only", length(roc), "new values supplied."))
        }

        if (relative) {
          new_value <- old_value * roc
        } else {
          new_value <- roc
        }

        prm_biol[pos] <- paste(new_value, collapse = "\t")
        return(prm_biol)
      }

      for (i in seq_along(select_acronyms)) {
        if (!(all(roc[[i]] == 1) & relative)) {
          prm_biol_new <- update_prm_species(prm_biol = prm_biol_new, acronym = select_acronyms[i],
                                             roc = roc[[i]], parameter = parameter, relative = relative)
        }
      }

      if (save_to_disc) {
        print("Writing new prm file!")
        writeLines(text = prm_biol_new, con = prm_biol, sep = "\n")
      }

      invisible(prm_biol_new)
  }
}


