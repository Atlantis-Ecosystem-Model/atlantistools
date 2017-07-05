#' Scan list of references for character string for fish species
#'
#' @inheritParams get_growth_fishbase
#' @param chr Character string to search.
#' @return Dataframe of potentially relevant references.
#' @export
#'
#' @examples
#' \dontrun{
#' # For some reason the examples break with appveyor.
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' df <- scan_reference_fishbase(fish, chr = "diet")
#' df <- scan_reference_fishbase(fish, chr = "xxx")
#' }

scan_reference_fishbase <- function(fish, chr, mirror = "se") {
  ids <- get_ids_fishbase(fish)

  # Split up Names in species and genus part to generate URLs
  ge_sp <- split_species(names(ids))

  urls <- paste0("http://fishbase.", mirror, "/References/SummaryRefList.php?ID=", ids, "&GenusName=", ge_sp$ge, "&SpeciesName=", ge_sp$sp)

  fishbase <- purrr::map(urls, xml2::read_html)

  # Extract reference table
  result <- purrr::map(fishbase, rvest::html_table) %>%
    purrr::map(., 1) %>%
    purrr::map(., ~.[-1, ])

  # add names to dataframes
  result <- purrr::map2(.x = result, .y = names(ids), ~tibble::add_column(.x, rep(.y, times = nrow(.x)))) %>%
    do.call(rbind, args = .) %>% # rbind is necessary due to different col-classes in 'Sex' = 'chr' and 'logical'
    purrr::set_names(., c("ref_id", "ref", "year", "named_used", "page", "species"))

  # Find chr string in the refernce
  pos <- grep(pattern = chr, x = result$ref)
  if (length(pos) > 0) {
    result[pos, ]
  } else {
    warning(paste(chr, "not found in reference table."))
  }
}
