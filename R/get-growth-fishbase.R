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
#' extract_linf_k_fishbase(c("Gadus morhua", "Merlangius merlangus"))
#' @export

get_growth_fishbase <- function(fish, exclude_subspecies = T){
  ids <- get_ids_fishbase(fish, exclude_subspecies)

  # Split up Names in species and genus part to generate URLs
  ge <- sapply(stringr::str_split(ids[[2]], pattern = " "),function(x)x[1])
  sp <- sapply(stringr::str_split(ids[[2]], pattern = " "),function(x)x[2])

  urls <- paste0("http://fishbase.org/PopDyn/PopGrowthList.php?ID=", ids[[1]], "&GenusName=", ge, "&SpeciesName=", sp, "&fc=183")

  fishbase <- lapply(urls, readLines, warn = F, n = 20)

  # First remove Species without Growth information!
  pos_missing <- which(grepl("The system found no growth information for the requested specie.", fishbase))
  # WARNING: The following ids are hard-coded!!!
  pos_missing <- c(pos_missing)
  if(length(pos_missing) >= 1){
    missing_species <- sort(ids[[2]][pos_missing])
    warning("No growth information available:\n", paste(missing_species, collapse = "\n "))
    ids <- lapply(ids, function(x)x[-pos_missing])
    fishbase <- fishbase[-pos_missing]
    urls <- urls[-pos_missing]
  }

  # Extract data from fishbase!
  result <- list()
  for(i in seq_along(urls)){
    result[[i]] <- XML::readHTMLTable(doc = urls[i], which = 3)
  }

  # add names to dataframes
  for(i in seq_along(result)){
    result[[i]]$species <- ids[[2]][i]
  }

  result <- do.call(rbind, result)

  # find reference urls.
  ref_urls <- purrr::map(urls, xml2::read_html) %>%
    purrr::map(., ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href")) %>%
    purrr::map(., ~.[stringr::str_detect(., pattern = "FishPopGrowthSummary")])

  # Cleanup
  result_backup <- result
  names(result) <- c("", "linf", "length_type", "k", "to", "sex", "m", "temp", "lm", "a", "country", "locality",
                     "questionable", "captive", "species")
  result <- result[, 2:dim(result)[2]]
  result$linf <- as.numeric(as.character(result$linf))
  result$k <- as.numeric(as.character(result$k))
  result$country <- as.character(result$country)
  result$locality <- as.character(result$locality)

  # check if result and urls match.
  count <- atlantistools::agg_data(result, col = "linf", groups = "species", out = "count", fun = length)
  if (all(count$count == purrr::map_int(ref_urls, length))) {
    result$ref_url <- purrr::flatten(ref_urls)
  } else {
    warning("ref_urls and final table do not match.")
  }

  return(result)
}


