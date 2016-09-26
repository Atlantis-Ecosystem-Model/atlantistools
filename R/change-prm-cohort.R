#' Change biological parameterfile for parameters which expect multiple values.
#'
#'
#' This functionis is used to help automate the calibration routine for ATLANTIS models.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
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
#' be passed directly.
#' @param save_to_disc Logical indicating if the resulting prm file should be overwritten
#' (\code{TRUE}) or not (\code{FALSE}).
#' @param variables Character vector of the variables to extract from the file.
#' @return parameterfile *.prm file with the new parameter values.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' new_prm <- change_prm_cohort(dir = d,
#'                              prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'                              select_acronyms = c("FPS", "FVS"),
#'                              roc = matrix(rep(2, times = 20), nrow = 2, ncol = 10),
#'                              parameter = "C",
#'                              save_to_disc = FALSE)
#'
#' # Also works for lists as argument
#' new_prm <- change_prm_cohort(dir = d,
#'                              prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'                              select_acronyms = c("FPS", "FVS"),
#'                              roc = list(rep(3, times = 10), rep(2, times = 10)),
#'                              parameter = "C",
#'                              save_to_disc = FALSE)

# dir <- file.path("Z:", "Atlantis_models", "Runs", "dummy_01_ATLANTIS_NS")
# prm_biol <- "NorthSea_biol_fishing.prm"
# select_acronyms <- c("COD", "WHG")
# roc <- list(COD = 1:12,
#             HER = 1:8)

change_prm_cohort <- function(dir = getwd(), prm_biol, select_acronyms, roc, parameter, relative = TRUE, save_to_disc = TRUE) {
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
      prm_biol_new <- convert_path(dir = dir, file = prm_biol)
      prm_biol_new <- readLines(con = prm_biol_new)

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
          prm_biol_new <- update_prm_species(prm_biol = prm_biol_new, acronym = select_acronyms[i], roc = roc[[i]], parameter = parameter, relative = relative)
        }
      }

      if (save_to_disc) {
        print("Writing new prm file!")
        writeLines(text = prm_biol_new, con = convert_path(dir = dir, file = prm_biol), sep = "\n")
      }

      invisible(prm_biol_new)
  }
}

#' @export
#' @rdname change_prm_cohort
# Extract value for a specific cohort parameter from a Vector of character strings.
extract_prm_cohort <- function(dir = getwd(), prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- convert_path(dir = dir, file = prm_biol)
  prm_biol_new <- readLines(con = prm_biol_new)

  slice <- function(prm, variable) {
    pos <- scan_prm(chars = prm, variable = variable)
    pos <- pos + 1
    while (substr(prm[pos], 1, 1) == "#") pos <- pos + 1

    # Keep all numeric values
    value <- str_split_twice(char = prm[pos], min_only = FALSE)
    return(value)
  }

  values <- lapply(variables, slice, prm = prm_biol_new)
  # rbind to matrix in case all groups have the same number of cohorts!
  if (length(unique(sapply(values, length))) == 1) {
    values <- do.call(rbind, values)
    rownames(values) <- variables
  } else {
    names(values) <- variables
  }
  return(values)
}

