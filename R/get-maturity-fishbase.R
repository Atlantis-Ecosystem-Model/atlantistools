#' Extract maturity parameters from http:://www.fishbase.se.
#'
#'
#' This function extracts values for maturity from http:://www.fishbase.se
#' @param fish Vector of fish species with genus and species information.
#' @param mirror Character string defining the url mirror to use. Defaults to \code{se}.
#' In case data extraction is slow use a different mirror. Try to avoid frequently used mirrors
#' like \code{uk} or \code{com}.
#' @return Dataframe with species, country, locality, linf and k.
#'
#' @details Before the actual extraction takes place fishbase IDs for every species are extracted using \code{\link{get_ids_fishbase}}.
#' The IDs are needed to generate the urls later on.
#' @examples
#' \dontrun{
#' # For some reason the examples break with appveyor.
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' df <- get_maturity_fishbase(fish)
#' head(df)
#' }

#' @export

get_maturity_fishbase <- function(fish, mirror = "se"){
  ids <- get_ids_fishbase(fish)
  fcs <- get_fcs_fishbase(fish)

  # Split up Names in species and genus part to generate URLs
  ge_sp <- split_species(names(ids))

  urls <- paste0("http://fishbase.", mirror, "/Reproduction/MaturityList.php?ID=", ids, "&GenusName=", ge_sp$ge, "&SpeciesName=", ge_sp$sp, "&fc=", fcs)

  fishbase <- purrr::map(urls, xml2::read_html)

  # Extract data table from fishbase!
  result <- purrr::map(fishbase, rvest::html_table) %>%
    purrr::map(., 2)

  # Remove Species without maturity information!
  pos_missing <- which(purrr::map_int(result, nrow) == 0)

  # leave function in case no information is present for any species
  if (length(pos_missing) == length(ids)) {
    stop("None of the species have information about growth. Add additional species.")
  } else {
    if (length(pos_missing) >= 1) {
      missing_species <- sort(names(ids)[pos_missing])
      warning(paste("No maturity information available for", length(pos_missing), "species:\n"), paste(missing_species, collapse = "\n"))
      ids <- ids[-pos_missing]
      fishbase <- fishbase[-pos_missing]
      result <- result[-pos_missing]
    }

    # add names to dataframes
    result <- purrr::map2(.x = result, .y = names(ids), ~tibble::add_column(.x, rep(.y, times = nrow(.x)))) %>%
      do.call(rbind, args = .) %>% # rbind is necessary due to different col-classes in 'Sex' = 'chr' and 'logical'
      purrr::set_names(., c("zzz", "lm", "lmin", "xxx", "lmax", "agemin", "yyy", "agemax", "agem", "sex", "country", "locality", "species"))

    # Cleanup
    result$xxx <- NULL
    result$yyy <- NULL
    result$zzz <- NULL
    result[result == ""] <- NA
    result$lm <- purrr::map_dbl(stringr::str_split(string = result$lm, pattern = "[ TL SL]"), ~as.numeric(.[1]))

    # find reference ids.
    ref_urls <- purrr::map(fishbase, ~rvest::html_nodes(., "a")) %>%
      purrr::map(., ~rvest::html_attr(., "href")) %>%
      purrr::map(., ~.[stringr::str_detect(., pattern = "FishMaturitySummary")])

    if (sum(purrr::map_int(ref_urls, length)) != length(unlist(ref_urls))) {
      stop("Please contact package devs.")
    } else {
      ref_urls <- purrr::flatten_chr(ref_urls)
    }

    # Helper function to extract all reference ids.
    # Based on the available data there are three types of references:
    # Main Ref., Age Ref., Length Ref.
    # ref_urls <- ref_urls[1:25]
    get_ref_id <- function(ref_urls, mirror) {
      urls <- paste0("http://fishbase.", mirror, ref_urls)

      # Create a list of reference urls. Each listentry has either 3 or 0 elements
      ref_id <- purrr::map(urls, xml2::read_html) %>%
        purrr::map(., ~rvest::html_nodes(., "a")) %>%
        purrr::map(., ~rvest::html_attr(., "href")) %>%
        purrr::map(., ~.[stringr::str_detect(., pattern = "references")])

      # Helper function to extract ref_ids from urls.
      triple_string_to_id <- function(chr) {
        if (length(chr) == 0) {
          rep(NA, 3)
        } else {
          ids <- stringr::str_extract_all(chr, pattern = "[0-9]")
          as.numeric(purrr::map_chr(ids, paste, collapse = ""))
        }
      }

      ref_ids <- tibble::as_tibble(do.call(rbind, purrr::map(ref_id, triple_string_to_id)))
      ref_ids <- purrr::set_names(ref_ids, c("main_ref", "age_ref", "length_ref"))
      return(ref_ids)
    }

    ref <- get_ref_id(ref_urls = ref_urls, mirror = mirror)
    result <- dplyr::bind_cols(result, ref)

    return(result)
  }
}




