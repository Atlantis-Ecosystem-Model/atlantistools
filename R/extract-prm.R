#' Extract values for Atlantis parameters from the biological parameter file.
#'
#' @param prm_biol Character string giving the connection to the biological parameterfile.
#' The filename usually contains \code{biol_fishing} and does end in \code{.prm}.
#' @param variables Character string giving the flag to search for. This should be
#' a combination of the parameter name and the group-Code.
#' @return numeric vector.
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#'
#' # You can pass a single variable
#' extract_prm(prm_biol, variables = "KWRR_FVS")
#'
#' # Or multiple variables
#' extract_prm(prm_biol, variables = paste("KWRR", c("FVS", "FPS"), sep = "_"))
#'
#' # Use extract_prm_cohort do extract data for age specific parameters.
#' # They are usually stored in the next line following the parameter tag.
#' extract_prm_cohort(prm_biol, variables = "C_FVS")
#' extract_prm_cohort(prm_biol, variables = paste("C", c("FVS", "FPS"), sep = "_"))

# Use this to document prm_biol.

#' @export
extract_prm <- function(prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- readLines(con = prm_biol, warn = FALSE)

  pos <- vapply(variables, scan_prm, FUN.VALUE = integer(1), chars = prm_biol_new)
  result <- prm_biol_new[pos]
  result <- vapply(result, str_split_twice, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  return(result)
}


#' @export
#' @rdname extract_prm
# Extract value for a specific cohort parameter from a Vector of character strings.
extract_prm_cohort <- function(prm_biol, variables) {
  # Read in parameter file!
  prm_biol_new <- readLines(con = prm_biol, warn = FALSE)

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
