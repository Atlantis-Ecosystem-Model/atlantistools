#' Extract reference for diet information from http:://www.fishbase.se
#'
#'
#' This function extracts reference for diet information from http:://www.fishbase.se
#' @inheritParams get_growth_fishbase
#' @return Dataframe with species, country, locality, linf and k.
#' @export
#'
#' @examples
#' \dontrun{
#' # For some reason the examples break with appveyor.
#' fish <- c("Gadus morhua", "Merlangius merlangus", "Maurolicus muelleri")
#' df <- get_growth_fishbase(fish)
#' head(df)
#' }

get_diet_fishbase <- function(fish, mirror = "se") {
  ids <- get_ids_fishbase(fish)

  # Split up Names in species and genus part to generate URLs
  ge_sp <- split_species(names(ids))

  urls <- paste0("http://www.fishbase.", mirror, "/summary/", ge_sp$ge, "-", ge_sp$sp, ".html")

  fishbase <- purrr::map(urls, xml2::read_html)

  diet_urls <- purrr::map(fishbase, ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href")) %>%
    purrr::map(., ~.[grep(pattern = "TrophicEco/DietCompoList.php", x = .)])

  # Fix species without diet information
  diet_urls[purrr::map_lgl(diet_urls, ~length(.) == 0)] <- NA
  diet_urls <- purrr::flatten_chr(diet_urls)

  # Extract data table from fishbase!
  ids <- !is.na(diet_urls)
  for (i in seq_along(ids))
  result <- purrr::map_if(diet_urls, ids, ~xml2::read_html(paste0("http://www.fishbase.", mirror, .))) %>%
    purrr::map_if(., ids, rvest::html_table) %>%
    purrr::map(., ~unique(.[[1]][, c("Country", "Locality", "Ref.")]))

  # First remove Species without Growth information!
  pos_missing <- purrr::map(fishbase, rvest::html_text) %>%
    purrr::map_lgl(., ~grepl("The system found no growth information for the requested specie.", .)) %>%
    which(.)

  # leave function in case no information is present for any species
  if (length(pos_missing) == length(ids)) {
    stop("None of the species have information about growth. Add additional species.")
  } else {
    if (length(pos_missing) >= 1) {
      missing_species <- sort(names(ids)[pos_missing])
      warning(paste("No growth information available for", length(pos_missing), "species:\n"), paste(missing_species, collapse = "\n"))
      ids <- ids[-pos_missing]
      fishbase <- fishbase[-pos_missing]
    }

    # Extract data table from fishbase!
    result <- purrr::map(fishbase, rvest::html_table) %>%
      purrr::map(., 3)
  }
}
