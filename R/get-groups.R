#' Collection of similar functions which get specific
#' columns from the Atlantis \code{functionalGroups.csv}
#'
#' This collection of functions uses the dataframe of functional
#' groups created with \code{\link{load_fgs}} and creates various
#' character strings of group names or acronym names.
#'
#' @inheritParams load_fgs
#'
#' @details Currently, the following character strings can be created
#' - get_groups: Extract column "Name"
#' - get_age_groups: Extract column "Name". Selects groups with 10 ageclasses.
#' - get_acronym: Extract column "Code"
#' - get_age_acronym: Extract column "Code". Selects groups with 10 ageclasses.
#' - get_nonage_acronym: Extracts columns "Code". Only groups with ageclasses different from 10 are selected.
#' - get_fish_acronyms: Extract column "Code". Only groups with InvertType equal to "FISH" or "SHARK" are selected.

#' @family get functions
#' @return Character string.

#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#'
#' get_age_groups(fgs)
#' get_nonage_acronyms(fgs)

#' @export
#' @rdname get_groups
get_groups <- function(fgs){
  fgs_df <- load_fgs(fgs = fgs)
  result <- fgs_df$Name
  return(result)
}

#' @export
#' @rdname get_groups
get_age_groups <- function(fgs){
  fgs_df <- load_fgs(fgs = fgs)
  supported_columns <- c("InvertType", "GroupType")
  result <- fgs_df$Name[fgs_df$NumCohorts > 2 | (fgs_df$NumCohorts == 2 & grepl(pattern = "FISH", fgs_df[, is.element(names(fgs_df), supported_columns)]))]
  return(result)
}

#' @export
#' @rdname get_groups
get_acronyms <- function(fgs){
  fgs_df <- load_fgs(fgs = fgs)
  result <- fgs_df[, names(fgs_df) == "Code"]
  return(result)
}

#' @export
#' @rdname get_groups
get_age_acronyms <- function(fgs){
  fgs_df <- load_fgs(fgs = fgs)
  supported_columns <- c("InvertType", "GroupType")
  result <- fgs_df$Code[fgs_df$NumCohorts > 2 | (fgs_df$NumCohorts == 2 & grepl(pattern = "FISH", fgs_df[, is.element(names(fgs_df), supported_columns)]))]
  return(result)
}

#' @export
#' @rdname get_groups
get_nonage_acronyms <- function(fgs){
  fgs_df <- load_fgs(fgs = fgs)
  result <- fgs_df$Code[fgs_df$NumCohorts <= 2]
  return(result)
}

#' @export
#' @rdname get_groups
get_fish_acronyms <- function(fgs){
  fgs_df <- load_fgs(fgs = fgs)
  # Older models use the column GroupType, newer ones use InvertType.
  supported_columns <- c("InvertType", "GroupType")
  if (!any(is.element(names(fgs_df), supported_columns))) {
    stop(paste("Column names in fgs do not match any of", supported_columns))
  } else {
    result <- fgs_df$Code[fgs_df[, is.element(names(fgs_df), supported_columns)] %in% c("FISH", "SHARK")]
  }
  return(result)
}


# dir <- "z:/R_codes/Thiebaut/"
# fgs <- "CEP_Groups_onespawn.csv"
# get_age_groups(dir, fgs)
# get_age_acronyms(dir, fgs)


