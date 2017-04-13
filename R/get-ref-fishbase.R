#' Extract the bibliographic info from www.fishbase.se.
#'
#'
#' Extract bibliographic information for growth parameters (linf, k, t0) from www.fishbase.se
#' @inheritParams get_growth_fishbase
#' @param growth_fishbase Dataframe generated with \code{\link{get_growth_fishbase}}.
#' @return Dataframe
#' @export
#'
#' @examples
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' growth_fishbase <- get_growth_fishbase(fish)
#' get_ref_fishbase(growth_fishbase)

get_ref_fishbase <- function(growth_fishbase, mirror = "se") {
  # extract unique reference and species combinations
  clean_ref <- dplyr::select_(growth_fishbase, .dots = c("species", "ref_id")) %>%
    split(., .$species) %>%
    purrr::map(., ~unique(unlist(.$ref_id))) %>%
    purrr::map(., ~.[!is.na(.)])  # remove NAs

  # convert to tibble
  clean_ref <- purrr::map2_df(.x = names(clean_ref), .y = clean_ref, ~tibble::tibble(species = .x, ref_id = .y))

  # Extract data from fishbase.org
  ref <- purrr::map_chr(clean_ref$ref_id, ~paste0("http://www.fishbase.", mirror, "/References/FBRefSummary.php?ID=", .)) %>%
    purrr::map(., xml2::read_html) %>%
    purrr::map(., rvest::html_table) %>%
    purrr::map_chr(., ~.[[1]][1, 2]) # annoying nested list...

  # Add references to datatable.
  if (length(ref) == nrow(clean_ref)) {
    clean_ref$ref <- ref
  } else {
    stop("Some reference IDs are wrong.") # Pretty sure this does never happen...
  }

  return(clean_ref)
}


