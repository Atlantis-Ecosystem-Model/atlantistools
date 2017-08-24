#' Extract bibliographic info from www.marlin.ac.uk/biotic.
#'
#' Extract bibliographic information for growth, diets, distribution for invertebrate species.
#' @param taxon Character vector of taxon names to search.
#' @param test Logical set to \code{TRUE} in case you need to run package development tests. Defaults
#' to \code{FALSE}.
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' taxon <- "Cancer pagurus"
#' taxon <- "Carcinus maenas"
#' taxon <- "xxx yyy"
#' taxon <- "Liocarcinus depurator"
#' taxon <- "Asterias rubens"
#' taxon <- "Henricia oculata"
#' taxon <- "Ensis ensis"
#' taxon <- "Ampelisca spinipes"
#'
#' df <- get_ref_biotic(taxon)
#' }

get_ref_biotic <- function(taxon, test = FALSE) {
  # Write function for a single taxon.
  single_taxon <- function(taxon, test) {
    # Split taxon to form url
    if (grepl(pattern = " ", taxon)) {
      url <- paste0(stringr::str_split(taxon, pattern = " ")[[1]], collapse = "%20")
    } else {
      url <- taxon
    }
    url <- paste0("http://www.marlin.ac.uk/biotic/browse.php?sp=x&spn=", url)

    # Read in the url as raw html.
    if (!test) {
      ref_raw <- xml2::read_html(curl::curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")), timeout = 10000)
    } else {
      # This is a bit messy but does it's job.
      ref_raw <- xml2::read_html(system.file("extdata/biotic-cancer-pagurus.html", package = "atlantistools"))
    }

    # Extract reference by category
    ref_df <- rvest::html_table(ref_raw, fill = TRUE)[[1]]

    # Leave function in case no information is present
    if (all(dim(ref_df) == 1)) {
      res <- tibble::tibble(species = taxon, cat = NA, ref = NA)
    } else {
      # Data is replicated in columns >= 3
      ref_df <- ref_df[, 1:2]
      ref_df <- ref_df[grepl(ref_df[, 1], pattern = "References"), ]
      names(ref_df) <- c("cat", "ref")
      ref_df$cat <- stringr::str_replace_all(ref_df$cat, pattern = " References", replacement = "")

      # Get the urls to the reference metadata (author, year, title)
      ref_urls <- rvest::html_nodes(ref_raw, "a") %>%
        rvest::html_attr(., "href")
      ref_urls <- ref_urls[grepl(ref_urls, pattern = "references")]
      ref_urls <- unique(ref_urls)

      # Convert reference string to vector of references.
      ref_df$ref <- purrr::map(ref_df$ref, refstr_to_ref)

      # Extract text of General Biology Additional Information paragraph sorted by heading
      bio <- bio_txt(url = url, test = test)

      # Assign references found within the bio text and update ref_df.
      refs_bio <- ref_df$ref[ref_df$cat == "Biology"][[1]]
      if (!any(is.na(refs_bio))) {
        # Well this is kinda getting ANNOYING! I will simply hardcode this for now...
        refs_bio <- refs_bio[refs_bio != "b), Nichols & Barker, 1984"]
        # Replace commas and double space entries in reference (THIS IS SO STUPID!!!!!!!)
        refs_bio_fix <- stringr::str_replace_all(refs_bio, pattern = ",", replacement = "")
        refs_bio_fix <- stringr::str_replace_all(refs_bio_fix, pattern = "  ", replacement = " ")

        ref_ids1 <- purrr::map(bio$col2, ~stringr::str_detect(., pattern = refs_bio))
        ref_ids2 <- purrr::map(bio$col2, ~stringr::str_detect(., pattern = refs_bio_fix))
        ref_ids <- purrr::map2(ref_ids1, ref_ids2, ~.x | .y)

        ref_bio <- tibble::tibble(cat = bio$headings, ref = purrr::map(ref_ids, ~refs_bio[.]))
      } else {
        ref_bio <- tibble::tibble(cat = bio$headings, ref = rep(NA, times = length(bio$headings)))
      }
    }
    # Create output tibble
    res <- dplyr::bind_rows(ref_df, ref_bio)
    res$species <- taxon
    res <- dplyr::select_(res, .dots = c("species", "cat", "ref"))
    # Fix NULLs in ref
    nulls <- purrr::map_lgl(res$ref, is.null)
    res$ref[nulls] <- NA

    return(res)
  }

  # Apply to all taxons
  purrr::map_df(taxon, single_taxon, test = test)
}


# Extract biology text by heading
bio_txt <- function(url, test = FALSE) {
  # Split the text into subsections
  if (!test) {
    biology <- readLines(url)
  } else {
    # very sloppy...
    biology <- readLines(system.file("extdata/biotic-cancer-pagurus.html", package = "atlantistools"))
  }
  biol_start <- grep(pattern = "General Biology Additional Information", biology)
  biol_end   <- grep(pattern = "Biology References", biology)
  biology <- biology[biol_start:(biol_end - 5)]

  # Get the section headings of "General Biology Additional Information"
  get_headings <- function(chr) {
    headings_start <- stringr::str_locate_all(pattern = "<b>", chr)[[1]][, 2] + 1
    headings_end <- stringr::str_locate_all(pattern = "</b>", chr)[[1]][, 1] - 1

    # Select headings from string
    stringr::str_sub(string = chr, start = headings_start, end = headings_end)
  }
  headings <- get_headings(paste(biology, collapse = ""))

  col2 <- vector(mode = "character", length = length(headings))
  for (i in seq_along(headings)) {
    if (i == 1) { # first headings is first entry in biology
      sec_start <- 1
      sec_end   <- grep(pattern = paste0("<b>", headings[i + 1], "</b>"), x = biology) - 1
    }
    if (i < length(headings) & i != 1) {
      sec_start <- grep(pattern = paste0("<b>", headings[i], "</b>"), x = biology)
      sec_end   <- grep(pattern = paste0("<b>", headings[i + 1], "</b>"), x = biology) - 1
    }
    if (i == length(headings)) {
      sec_start <- grep(pattern = paste0("<b>", headings[i], "</b>"), x = biology) + 1
      sec_end   <- length(biology)
    }
    col2[i] <- paste(biology[sec_start:sec_end], collapse = " ")
  }

  # Remove html italic code and paranthesis to make reference tags searchable.
  # Code is pretty ugly and hacky.... Nonetheless, it is much easier to find references
  # this way due to non-consistent citing techniques within the text. E.g. Kuris et al., (2002) suggested
  # vs. (Kuris et al., 2002)
  col2 <- stringr::str_replace_all(col2, pattern = "<i>", replacement = "")
  col2 <- stringr::str_replace_all(col2, pattern = "</i>", replacement = "")
  col2 <- stringr::str_replace_all(col2, pattern = "\\(", replacement = "")
  col2 <- stringr::str_replace_all(col2, pattern = "\\)", replacement = "")
  col2 <- stringr::str_replace_all(col2, pattern = "&#243;", replacement = "ó")

  # Cleanup headings
  cleanup <- function(chr) {
    while (grepl(pattern = " ", x = stringr::str_sub(chr, start = stringr::str_length(chr), end = stringr::str_length(chr)))) {
      chr <- stringr::str_sub(chr, end = stringr::str_length(chr) - 1)
    }
    return(chr)
  }

  headings <- purrr::map_chr(headings, cleanup)
  headings <- stringr::str_replace_all(headings, pattern = ":", replacement = "")

  bio <- tibble::tibble(headings, col2)
  return(bio)
}

# Extract reference information from a reference-string in ref_df$ref.
refstr_to_ref <- function(refstr) {
  # Some species have no reference summaries (E.g. Ensis ensis) or only unpublished ones e.g.
  # no integers present inside the reference string.
  if (refstr == "" | !stringr::str_detect(refstr, pattern = "[0-9]")) {
    NA
  } else {
    # Each reference does end with a number indicating the publication year.
    # Therefore we should be able to split the reference at the end of each
    # year entry.
    num_pos <- stringr::str_locate_all(refstr, pattern = "[0-9]")[[1]][, 1]
    lags <- diff(num_pos)
    if (all(lags == 1)) { # Only one reference present!
      res <- refstr
    } else {
      # Iteravtively split the string into substrings
      num_pos <- num_pos[lags != 1]
      res <- vector(mode = "character", length = length(num_pos) + 1)
      for (i in seq_along(res)) {
        if (i == 1)                    res[i] <- stringr::str_sub(refstr, end = num_pos[1])
        if (i < length(res) & i != 1)  res[i] <- stringr::str_sub(refstr, start = num_pos[i - 1] + 3, end = num_pos[i])
        if (i == length(res))          res[i] <- stringr::str_sub(refstr, start = num_pos[i - 1] + 3)
      }
    }
    # Remove trailing non integer and leading non character entries from string
    clean_string <- function(str) {
      nchr <- stringr::str_length(str)
      #  Remove trailing non integer entries from string
      while (!grepl(pattern = "[0-9]", x = stringr::str_sub(str, start = nchr, end = nchr))) {
        nchr <- nchr - 1
        str <- stringr::str_sub(str, end = nchr)
      }
      # Remove leading non character entries from string
      while (!grepl(pattern = "[a-z|A-Z]", x = stringr::str_sub(str, start = 1, end = 1))) {
        nchr <- nchr - 1
        str <- stringr::str_sub(str, start = 2)
      }
      # Insert comma between author and year if missing!
      if (!grepl(pattern = ",", x = str)) {
        num_pos <- stringr::str_locate_all(str, pattern = "[0-9]")[[1]][1]
        str <- paste(stringr::str_sub(str, end = num_pos - 2), stringr::str_sub(str, start = num_pos), sep = ", ")
      }

      return(str)
    }
    purrr::map_chr(res, clean_string)
  }
}
