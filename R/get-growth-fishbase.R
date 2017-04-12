#' Extract growth parameters from www.fishbase.org.
#'
#'
#' This function extracts values for Linf and k from www.fishbase.org
#' @param fish Vector of fish species with genus and species information.
#' @return Dataframe with species, country, locality, linf and k.
#'
#' @details Before the actual extraction takes place fishbaseh IDs for every species are extracted using the function "get_ids_fishbase".
#' The IDs are needed to generate the URLs lateron. At the moment subspecies can only be excluded from the extraction.
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
  result <- list()
  for (i in seq_along(urls)) {
    result[[i]] <- XML::readHTMLTable(doc = urls[i], which = 3, stringsAsFactors = FALSE)
  }

  # add names to dataframes
  for (i in seq_along(result)) {
    result[[i]]$species <- names(ids)[i]
  }

  # Cleanup
  # Convert chr columns to numeric if possible.
  result <- dplyr::bind_rows(result)
  result <- result[, 2:dim(result)[2]]
  result[result == ""] <- NA
  num_cols <- which(purrr::map_lgl(result, ~!any(is.na(suppressWarnings(as.numeric(.[!is.na(.)]))))))
  names(result) <- c("linf", "length_type", "k", "to", "sex", "m", "temp", "lm", "a", "country", "locality",
                     "questionable", "captive", "species")
  result <- dplyr::mutate_at(result, num_cols, as.numeric)

  # find reference urls.
  ref_urls <- purrr::map(urls, xml2::read_html) %>%
    purrr::map(., ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href")) %>%
    purrr::map(., ~.[stringr::str_detect(., pattern = "FishPopGrowthSummary")])

  # check if result and urls match.
  count <- agg_data(result, col = "linf", groups = "species", out = "count", fun = length)
  if (all(count$count == purrr::map_int(ref_urls, length))) {
    result$ref_url <- unlist(ref_urls)
  } else {
    warning("ref_urls and final table do not match.")
  }

  return(result)
}


