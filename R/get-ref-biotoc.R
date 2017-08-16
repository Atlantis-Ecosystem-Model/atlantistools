#' Extract bibliographic info from www.fishbase.org.
#'
#'
#' Extract bibliographic information for growth parameters (linf, k, t0) from www.fishbase.org
#' @inheritParams get_growth_fishbase
#' @param ref_id vector of reference ids.
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' taxon <- c("Cancer pagurus", "Abra nitida")
#' }

get_ref_biotic <- function(taxon) {
  single_taxon <- function(taxon) {
    # Split taxon to form url
    if (grepl(pattern = " ", taxon)) {
      url <- paste0(stringr::str_split(taxon, pattern = " ")[[1]], collapse = "%20")
    } else {
      url <- taxon
    }
  url <- paste0("http://www.marlin.ac.uk/biotic/browse.php?sp=x&spn=", url)

  ref_raw <- xml2::read_html(url)

  wuwu <- rvest::html_table(ref_raw, fill = TRUE)

  links <- rvest::html_nodes(ref_raw, "a") %>%
    rvest::html_attr(., "href")

  }
  # Read information from Biotic
}

http://www.marlin.ac.uk/biotic/browse.php?sp=x&spn=Cancer%20pagurus
