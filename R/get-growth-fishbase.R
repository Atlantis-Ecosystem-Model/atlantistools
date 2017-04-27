#' Extract growth parameters from www.fishbase.se.
#'
#'
#' This function extracts values for Linf, k  and t0 from www.fishbase.se
#' @param fish Vector of fish species with genus and species information.
#' @param mirror Character string defining the url mirror to use. Defaults to \code{se}.
#' In case data extraction is slow use a different mirror. Try to avoid frequently used mirrors
#' like \code{uk} or \code{com}.
#' @return Dataframe with species, country, locality, linf and k.
#'
#' @details Before the actual extraction takes place fishbaseh IDs for every species are extracted using \code{\link{get_ids_fishbase}}.
#' The IDs are needed to generate the urls lateron.
#' @examples
#' \dontrun{
#' # For some reason the examples break with appveyor.
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' df <- get_growth_fishbase(fish)
#' head(df)
#'
#' df <- get_growth_fishbase(fish, mirror = "de")
#' head(df)
#'
#' # Only use for debugging purposes.
#' fish <- read.csv("Z:/my_data_alex/fish_species_names_from_ibts.csv", stringsAsFactors = FALSE)[, 1]
#' url <- get_growth_fishbase(fish)
#' url <- urls$ref_url
#' }

#' @export

get_growth_fishbase <- function(fish, mirror = "se"){
  ids <- get_ids_fishbase(fish)

  # Split up Names in species and genus part to generate URLs
  ge_sp <- split_species(names(ids))

  urls <- paste0("http://fishbase.", mirror, "/PopDyn/PopGrowthList.php?ID=", ids, "&GenusName=", ge_sp$ge, "&SpeciesName=", ge_sp$sp, "&fc=183")

  fishbase <- purrr::map(urls, xml2::read_html)

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

    # add names to dataframes
    result <- purrr::map2(.x = result, .y = names(ids), ~tibble::add_column(.x, rep(.y, times = nrow(.x)))) %>%
      do.call(rbind, args = .) %>% # rbind is necessary due to different col-classes in 'Sex' = 'chr' and 'logical'
      purrr::set_names(., c("xxx", "linf", "length_type", "k", "to", "sex", "m", "temp", "lm", "a",
                            "country", "locality", "questionable", "captive", "species"))

    # Cleanup
    result$xxx <- NULL
    result[result == ""] <- NA

    # find reference urls.
    ref_urls <- purrr::map(fishbase, ~rvest::html_nodes(., "a")) %>%
      purrr::map(., ~rvest::html_attr(., "href")) %>%
      purrr::map(., ~.[stringr::str_detect(., pattern = "FishPopGrowthSummary")])

    # check if result and urls match. Rearrange due to alphabetical ordering in df.
    count <- agg_data(result, col = "linf", groups = "species", out = "count", fun = length)
    count <- count[match(names(ids), count$species), ]
    if (all(count$count == purrr::map_int(ref_urls, length))) {
      result$ref_url <- unlist(ref_urls)
      ref_ids <- purrr::map(result$ref_url, url_to_refid)
      result$main_ref <- purrr::map_int(ref_ids, 1)
      result$data_ref <- purrr::map_int(ref_ids, 2)
    } else {
      warning("ref_urls and final table do not match.")
    }

    # Add missing species
    if (length(pos_missing) >= 1) {
      add_missing <- result[1:length(missing_species), ]
      add_missing[,] <- NA
      add_missing$species <- missing_species
      result <- dplyr::bind_rows(result, add_missing)
    }

    return(result)
  }
}

# url <- result$ref_url[1]
url_to_refid <- function(url, mirror = "se") {
  # extract links from html
  links <- xml2::read_html(paste0("http://www.fishbase.", mirror, "/", url)) %>%
    rvest::html_text(.)

  # this is a bit ugly but it works like a charm.
  p1 <- stringr::str_split_fixed(links, pattern = "Main Ref. :", n = 2)[, 2]
  p2 <- stringr::str_split_fixed(p1, pattern = "Data Ref. :", n = 2)
  p3 <- stringr::str_split_fixed(p2[, 2], pattern = "Data Type :", n = 2)

  # combine main and data ref strings
  refs <- c(p2[, 1], p3[, 1])

  # extract numeric values
  ref_id <- purrr::map_chr(refs, ~paste0(unlist(stringr::str_extract_all(string = ., pattern = "[0-9]")), collapse = ""))
  ref_id <- suppressWarnings(as.integer(ref_id))

  return(ref_id)
}


