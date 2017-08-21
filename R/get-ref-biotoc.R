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
    names(ref_df) <- c("cat", "ref")

    # Get the urls to the reference metadata (author, year, title)
    ref_urls <- rvest::html_nodes(ref_raw, "a") %>%
      rvest::html_attr(., "href")
    ref_urls <- ref_urls[grepl(ref_urls, pattern = "references")]

    # wewe <- rvest::html_text(ref_raw)




    ref_df$ref <- purrr::map(ref_df$ref, refstr_to_ref)

  }
  # Read information from Biotic
}


# Extract biology reference by heading
bio_ref <- function(ref_raw, url) {
  # Get the section headings of "General Biology Additional Information"
  headings <- rvest::html_nodes(ref_raw, "b") %>%
    rvest::html_text(.)
  headings <- headings[headings != "Note"]

  # Split the text into subsections
  biology <- readLines(url)
  biol_start <- grep(pattern = "General Biology Additional Information", biology)
  biol_end   <- grep(pattern = "Biology References", biology)
  biology <- biology[biol_start:(biol_end - 5)]

  col2 <- vector(mode = "character", length = length(headings))
  for (i in seq_along(headings)) {
    if (i == 1) { # first headings is first entry in biology
      sec_start <- 1
      sec_end   <- grep(pattern = paste0("<b>", headings[i + 1], "</b>"), x = biology) - 1
    }
    if (i < length(headings) & i != 1) {
      sec_start <- grep(pattern = paste0("<b>", headings[i], "</b>"), x = biology) + 1
      sec_end   <- grep(pattern = paste0("<b>", headings[i + 1], "</b>"), x = biology) - 1
    }
    if (i == length(headings)) {
      sec_start <- grep(pattern = paste0("<b>", headings[i], "</b>"), x = biology) + 1
      sec_end   <- length(biology)
    }
    col2[i] <- paste(biology[sec_start:sec_end], collapse = " ")
  }

  bio <- tibble::tibble(headings, col2)
  return(bio)
}

# Extract reference information from a reference-string in ref_df$ref.
refstr_to_ref <- function(refstr) {
  # Each reference does end with a number indicating the publication year.
  # Therefore we should be able to split the reference at the end of each
  # year entry.
  num_pos <- stringr::str_locate_all(refstr, pattern = "[0-9]")[[1]][, 1]
  num_pos <- num_pos[diff(num_pos) != 1]

  # Iteravtively split the string into substrings
  res <- vector(mode = "character", length = length(num_pos) + 1)
  for (i in seq_along(res)) {
    if (i == 1)                    res[i] <- stringr::str_sub(refstr, end = num_pos[1])
    if (i < length(res) & i != 1)  res[i] <- stringr::str_sub(refstr, start = num_pos[i - 1] + 3, end = num_pos[i])
    if (i == length(res))          res[i] <- stringr::str_sub(refstr, start = num_pos[i - 1] + 3)
  }

  # Remove trailing non integer entries from string
  clean_string <- function(str) {
    nchr <- stringr::str_length(str)
    while (!grepl(pattern = "[0-9]", x = stringr::str_sub(str, start = nchr, end = nchr))) {
      nchr <- nchr - 1
      str <- stringr::str_sub(str, end = nchr)
    }
    return(str)
  }
  purrr::map_chr(res, clean_string)
}
