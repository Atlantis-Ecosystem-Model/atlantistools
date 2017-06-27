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
#' diet <- get_diet_fishbase(fish)
#'
#' fish <- c("Gadus morhua", "Merlangius merlangus", "Ammodytes marinus")

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
  result <- purrr::map_if(diet_urls, ids, ~xml2::read_html(paste0("http://www.fishbase.", mirror, .))) %>%
    purrr::map_if(., ids, rvest::html_table) %>%
    purrr::map_if(., ids, ~unique(.[[1]][, c("Country", "Locality", "Ref.")]))

  # Add species names
  df_names <- c("country", "locality", "ref", "species")
  diet_df <- purrr::map2_df(.x = result[ids], .y = fish[ids], ~tibble::add_column(.x, rep(.y, times = nrow(.x)))) %>%
    tibble::as_tibble(.) %>%
    purrr::set_names(., df_names)

  # Add species without diet-info
  if (any(!ids)) {
    nas <- tibble::as_tibble(do.call(rbind, purrr::map(fish[!ids], ~c(rbind(rep(NA, length(df_names) - 1)), .)))) %>%
      purrr::set_names(., df_names)

    diet_df <- dplyr::bind_rows(diet_df, nas)
  }

  # leave function in case no information is present for any species
  if (all(is.na(diet_df$ref))) {
    stop("None of the species have information about diets Add additional species.")
  } else {
    return(diet_df)
  }
}
