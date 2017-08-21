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
#' taxon <- "Cancer pagurus"
#' df <- get_ref_biotic(taxon)
#' }

get_ref_biotic <- function(taxon) {
  # Write function for a single taxon.
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
    ref_df$cat <- stringr::str_replace_all(ref_df$cat, pattern = " References", replacement = "")

    # Get the urls to the reference metadata (author, year, title)
    ref_urls <- rvest::html_nodes(ref_raw, "a") %>%
      rvest::html_attr(., "href")
    ref_urls <- ref_urls[grepl(ref_urls, pattern = "references")]

    # Convert reference string to vector of references.
    ref_df$ref <- purrr::map(ref_df$ref, refstr_to_ref)

    # Extract text of General Biology Additional Information paragraph sorted by heading
    bio <- bio_txt(ref_raw = ref_raw, url = url)

    # Assign references found within the bio text and update ref_df
    refs_bio <- ref_df$ref[ref_df$cat == "Biology"][[1]]
    ref_ids <- purrr::map(bio$col2, ~stringr::str_detect(., pattern = refs_bio))
    ref_bio <- tibble::tibble(cat = bio$headings, ref = purrr::map(ref_ids, ~refs_bio[.]))

    # Create output tibble
    res <- dplyr::bind_rows(ref_df, ref_bio)
    res$taxon <- taxon
    return(res)
  }

  # Apply to all taxons
  purrr::map_df(taxon, single_taxon)
}


# Extract biology text by heading
bio_txt <- function(ref_raw, url) {
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

  # Remove html italic code to make reference tags searchable.
  col2 <- stringr::str_replace_all(col2, pattern = "<i>", replacement = "")
  col2 <- stringr::str_replace_all(col2, pattern = "</i>", replacement = "")

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
