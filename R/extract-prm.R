#' Extract values for Atlantis parameters from the biological parameter file.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param variables Character string giving the flag to search for. This should be
#' a combination of the parameter name and the group-Code.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.

#' @export
extract_prm <- function(dir = getwd(), prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- convert_path(dir = dir, file = prm_biol)
  prm_biol_new <- readLines(con = prm_biol_new, warn = FALSE)

  pos <- vapply(variables, scan_prm, FUN.VALUE = integer(1), chars = prm_biol_new)
  result <- prm_biol_new[pos]
  result <- vapply(result, str_split_twice, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  return(result)
}


#' @export
#' @rdname extract_prm
# Extract value for a specific cohort parameter from a Vector of character strings.
extract_prm_cohort <- function(dir = getwd(), prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- convert_path(dir = dir, file = prm_biol)
  prm_biol_new <- readLines(con = prm_biol_new,  warn = FALSE)

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
  # if (length(unique(sapply(values, length))) == 1) {
  #   values <- do.call(rbind, values)
  #   rownames(values) <- variables
  # } else {
    names(values) <- variables
  # }
  return(values)
}
