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

    # Read in the url as raw html.
    ref_raw <- xml2::read_html(url)

    # Extract reference by category
    ref_df <- rvest::html_table(ref_raw, fill = TRUE)[[1]][, 1:2]
    ref_df <- ref_df[grepl(ref_df[, 1], pattern = "References"), ]

    # Get the urls to the reference metadata (author, year, title)
    ref_urls <- rvest::html_nodes(ref_raw, "a") %>%
      rvest::html_attr(., "href")
    ref_urls <- ref_urls[grepl(ref_urls, pattern = "references")]

    wewe <- rvest::html_text(ref_raw)

    bio_ref <- function(ref_raw) {
      # Get the section headings of "General Biology Additional Information"
      headings <- rvest::html_nodes(ref_raw, "b") %>%
        rvest::html_text(.)
      headings <- headings[headings != "Note"]

      # Split the text into subsections
      biology <- rvest::html_text(ref_raw)
      biol_start <- stringr::str_locate(biology, "General Biology Additional Information")[1, 2]
      biol_end   <- stringr::str_locate(biology, "Biology References")[1, 1]
      biology <- stringr::str_sub(biology, start = biol_start + 1, end = biol_end - 1)

      bio <- tibble::tibble()
      biology <- readLines(url)
      biol_start <- grep(pattern = "General Biology Additional Information", biology)
      biol_end   <- grep(pattern = "Biology References", biology)
      biology <- biology[biol_start:(biol_end - 1)]
    }
  }
  # Read information from Biotic
}

