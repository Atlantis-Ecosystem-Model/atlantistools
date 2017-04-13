#' Extract growth parameters from www.fishbase.org.
#'
#'
#' This function extracts values for Linf, k  and t0 from www.fishbase.org
#' @param fish Vector of fish species with genus and species information.
#' @return Dataframe with species, country, locality, linf and k.
#'
#' @details Before the actual extraction takes place fishbaseh IDs for every species are extracted using \code{\link{get_ids_fishbase}}.
#' The IDs are needed to generate the urls lateron.
#' @examples
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' df <- get_growth_fishbase(fish)
#' head(df)

#' @export

get_growth_fishbase <- function(fish){
  ids <- get_ids_fishbase(fish)

  # Split up Names in species and genus part to generate URLs
  ge_sp <- split_species(names(ids))

  urls <- paste0("http://fishbase.org/PopDyn/PopGrowthList.php?ID=", ids, "&GenusName=", ge_sp$ge, "&SpeciesName=", ge_sp$sp, "&fc=183")

  fishbase <- lapply(urls, readLines, warn = F, n = 20)

  # First remove Species without Growth information!
  pos_missing <- which(grepl("The system found no growth information for the requested specie.", fishbase))
  # WARNING: The following ids are hard-coded!!!
  pos_missing <- c(pos_missing)
  if (length(pos_missing) >= 1) {
    missing_species <- sort(names(ids)[pos_missing])
    warning("No growth information available:\n", paste(missing_species, collapse = "\n "))
    ids <- ids[-pos_missing]
    fishbase <- fishbase[-pos_missing]
    urls <- urls[-pos_missing]
  }

  # Extract data from fishbase!
  result <- purrr::map(urls, xml2::read_html) %>%
    purrr::map(., rvest::html_table) %>%
    purrr::map(., 3)

  # add names to dataframes
  result <- purrr::map2_df(.x = result, .y = fish, ~tibble::add_column(.x, rep(.y, times = nrow(.x)))) %>%
    purrr::set_names(., c("xxx", "linf", "length_type", "k", "to", "sex", "m", "temp", "lm", "a",
                          "country", "locality", "questionable", "captive", "species"))

  # Cleanup
  # Convert chr columns to numeric if possible.
  result$xxx <- NULL
  result[result == ""] <- NA

  # find reference urls.
  ref_urls <- purrr::map(urls, xml2::read_html) %>%
    purrr::map(., ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href")) %>%
    purrr::map(., ~.[stringr::str_detect(., pattern = "FishPopGrowthSummary")])

  # check if result and urls match.
  count <- agg_data(result, col = "linf", groups = "species", out = "count", fun = length)
  if (all(count$count == purrr::map_int(ref_urls, length))) {
    result$ref_url <- unlist(ref_urls)
    result$ref_id <- purrr::map(result$ref_url, url_to_refid)
  } else {
    warning("ref_urls and final table do not match.")
  }

  return(result)
}

# url <- result$ref_url[1]
url_to_refid <- function(url) {
  # extract links from html
  links <- xml2::read_html(paste0("http://www.fishbase.se/", url)) %>%
    rvest::html_nodes(., "a") %>%
    rvest::html_attr(., "href")

  # extract reference ids
  ids <- links[which(stringr::str_detect(links, pattern = "References"))[1:2]] %>%
    stringr::str_split(., pattern = "ID=") %>%
    purrr::map_int(., ~as.integer(.[2]))

  return(unique(ids))
}


