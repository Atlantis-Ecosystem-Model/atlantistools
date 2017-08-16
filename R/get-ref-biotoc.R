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
#' taxon <- c("Cancer pagurus", "Carcinus maenas")
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

  # Extract reference by category
  ref_df <- rvest::html_table(ref_raw, fill = TRUE)[[1]]
  ref_df <- ref_df[grepl(ref_df[, 1], pattern = "References"), ]
  ref_df <- ref_df[, purrr::map_lgl(ref_df, ~sum(is.na(.)) != nrow(ref_df))]

  wawa <- wuwu[[1]][!is.na(wuwu[[1]])]

  ref_urls <- rvest::html_nodes(ref_raw, "a") %>%
    rvest::html_attr(., "href")
  ref_urls <- ref_urls[grepl(ref_urls, pattern = "references")]

  wewe <- rvest::html_text(ref_raw)

  }
  # Read information from Biotic
}

