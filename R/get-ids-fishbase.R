#' Extract fishbase IDs using the package "rfishbase" to generate species specific fishbase URLs
#'
#' This function extracts fishbase IDs using the database provided by the "rifishbase" package.
#' @inheritParams get_growth_fishbase
#' @return named vector with species names and fishbase IDs.
#'
#' @details The function depends on the package "rfishbase" which creates a local copy of the fishbase database.
#' The IDs are needed to generate URLs to scan www.fishbase.se for detailed informations about fish growth for example.
#' @keywords gen
#' @examples
#' fish <- c("Gadus morhua", "Merlangius merlangus", "Clupea harengus")
#' get_ids_fishbase(fish)
#' @export

get_ids_fishbase <- function(fish){
  # Check if every fishname is composed of genus and species!
  if (any(vapply(stringr::str_split(fish, pattern = " "), length, FUN.VALUE = integer(1)) < 2)) stop("Fishnames not complete.")

  fish_data <- rfishbase::fishbase

  ge_sp <- split_species(fish)

  # get fishbase ids
  pos <- purrr::map2(.x = ge_sp$ge, .y = ge_sp$sp, ~fish_data$Genus == .x & fish_data$Species == .y)

  # report species not found in database
  missing <- purrr::map_int(pos, sum) == 0
  if (sum(missing >= 1)) {
    warning(paste("The following species are not part of the fishbase dataframe", paste(fish[missing], collapse = ", ")))
    fish <- fish[!missing]
    pos <- pos[!missing]
  }

  pos <- purrr::map_int(pos, which)
  pos <- fish_data$SpecCode[pos]
  names(pos) <- fish

  return(pos)
}

split_species <- function(fish) {
  result <- stringr::str_split_fixed(fish, pattern = " ", n = 2)
  result <- list(ge = result[, 1], sp = result[, 2])
  return(result)
}


