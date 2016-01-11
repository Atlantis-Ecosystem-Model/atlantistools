#' Collection of similar functions which get specific
#' columns from the Atlantis \code{functionalGroups.csv}
#'
#' This collection of functions takes loaded functional
#' groups, via the \code{\link{load_fgs}} function and creates various
#' character strings of group names or acronym names.
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#'
#' @details Currently, the following character strings can be created
#' - get_groups extracts the column "Name"
#' - get_age_groups extracts the column "Name". Selects groups with 10 ageclasses.
#' - get_acronym extracts the column "Code"
#' - get_age_acronym extracts the column "Code". Selects groups with 10 ageclasses.
#' - get_nonage_acronym extracts the columne "Code". Only groups with ageclasses different from 10 are selected.
#' - get_fish_acronyms ectracts the column "Code". Only groups with InvertType equal to "FISH" or "SHARK" are selected.

#' @family get functions
#' @return Character string.

#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' get_age_groups(dir = d, fgs = "functionalGroups.csv")
#' get_nonage_acronyms(dir = d, fgs = "functionalGroups.csv")

#' @export
#' @rdname get_groups
get_groups <- function(dir, fgs){
  fgs <- load_fgs(dir = dir, fgs = fgs)
  result <- fgs$Name
  return(result)
}

#' @export
#' @rdname get_groups
get_age_groups <- function(dir, fgs){
  fgs <- load_fgs(dir = dir, fgs = fgs)
  result <- fgs$Name[fgs$NumCohorts == 10]
  return(result)
}

#' @export
#' @rdname get_groups
get_acronyms <- function(dir, fgs){
  fgs <- load_fgs(dir = dir, fgs = fgs)
  result <- fgs[, names(fgs) == "Code"]
  return(result)
}

#' @export
#' @rdname get_groups
get_age_acronyms <- function(dir, fgs){
  fgs <- load_fgs(dir = dir, fgs = fgs)
  result <- fgs$Code[fgs$NumCohorts == 10]
  return(result)
}

#' @export
#' @rdname get_groups
get_nonage_acronyms <- function(dir, fgs){
  fgs <- load_fgs(dir = dir, fgs = fgs)
  result <- fgs$Code[fgs$NumCohorts != 10]
  return(result)
}

#' @export
#' @rdname get_groups
get_fish_acronyms <- function(dir, fgs){
  fgs <- load_fgs(dir = dir, fgs = fgs)
  # Older models use the column GroupType, newer ones use InvertType.
  supported_columns <- c("InvertType", "GroupType")
  if (!any(is.element(names(fgs), supported_columns))) {
    stop(paste("Column names in fgs do not match any of", supported_columns))
  } else {
    result <- fgs$Code[fgs[, is.element(names(fgs), supported_columns)] %in% c("FISH", "SHARK")]
  }
  return(result)
}
