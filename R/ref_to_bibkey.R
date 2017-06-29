#' Convert reference to bib-tex-key.
#'
#' @param ref_df dataframe with columns author, year, title
#' in case title is missing match is performed based on author and year only.
#' @param bib character string giving the name of the .bib file.
#' @return Character vector.
#' @export

# load("z:/R_codes/gns-data-input/data/refs_find.rda", verbose = TRUE)
# ref_df <- refs_find[1:10, 3:5]
# bib <- "z:/R_codes/gns-data-input/gns-input-data.bib"

ref_to_bibkey <- function(ref_df, bib) {
  ref_bib <- bib_to_df(bib = bib)

  # single_ref <- ref_df[1, ]
  single_bibtex <- function(single_ref, ref_bib) {
    # Match by year
    bibkey <- dplyr::filter_(ref_bib, ~year == single_ref$year)
    if (nrow(bibkey) > 0) {
      # Match by title
      bibkey <- bibkey[purrr::map_lgl(bibkey$author, ~sum(stringr::str_detect(pattern = ., string = single_ref$author), na.rm = T) > 0), ]
      if (nrow(bibkey) > 0 & !is.na(single_ref$title)) {
        # Only use data with matching title.
        words <- stringr::str_split(bibkey$title, pattern = " ")
        # remove punctuation marks and such
        words <- purrr::map(words, stringr::str_replace_all, pattern = "[\\,\\.\\:\\[\\]]", replacement = "")
        counts <- purrr::map(words, ~stringr::str_detect(string = single_ref$title, stringr::coll(., ignore_case = T)))
        matches <- purrr::map_dbl(counts, ~sum(.)/length(.))
        max_match <- which(max(matches) == matches)

        # Extract bibkey in case more than 80% of words match.
        if (length(max_match) == 1 && matches[max_match] > 0.8) return(bibkey$bibkey[max_match])
      } else {
        # Return unique-bibkey in case no title is present but year and author yield one specicif bibkey
        if (nrow(bibkey) == 1) return(bibkey$bibkey)
      }
    }
    return(NA)
  }

  purrr::map_chr(1:nrow(ref_df), ~single_bibtex(single_ref = ref_df[., ], ref_bib = ref_bib))
}

# Convert bib file to tidy dataframe!
# bib2df does use incorrect enconding in readLines call
# Author names are cleaned.
# This might not work in case different bib encondings are used.
bib_to_df <- function(bib) {
  bib_df <- readLines(bib, encoding = "UTF-8")
  block_ids <- grep(pattern = "\\@", x = bib_df)
  single_entries <- vector(mode = "list", length = length(block_ids))
  for (i in 1:(length(single_entries) - 1)) {
    single_entries[[i]] <- bib_df[block_ids[i]:(block_ids[i + 1] - 1)]
  }

  # Add last entry ba hand
  mid <- length(single_entries)
  if (is.null(single_entries[[mid]])) single_entries[[mid]] <- bib_df[block_ids[mid]:length(bib_df)]

  # chr <- single_entries[[11]]
  get_bibkey <- function(chr) {
    out <- chr[grep(pattern = "\\@", x = chr)]
    stringr::str_sub(string = out, start = stringr::str_locate(string = out, pattern = "\\{")[1] + 1, end = stringr::str_length(out) - 1)
  }

  bib_to_chr <- function(chr, pattern) {
    out <- chr[grep(pattern = pattern, x = chr)]
    out <- stringr::str_split_fixed(string = out, pattern = "\\{", n = 2)[, 2]
    stringr::str_split_fixed(string = out, pattern = "\\}", n = 2)[, 1]
  }

  get_authors <- function(chr) {
    authors <- bib_to_chr(chr = chr, pattern = "author")
    authors <- stringr::str_split(authors, pattern = " and ")
    authors <- purrr::map_chr(authors[[1]], ~stringr::str_split_fixed(., pattern = ",", n = 2)[, 1])
    authors[!grepl(pattern = "others", x = authors)]
  }

  get_year <- function(chr) {
    out <- bib_to_chr(chr = chr, pattern = "date =")
    # url date is reported for some bib entries
    if (length(out) == 2) out <- out[2]
    as.integer(stringr::str_split_fixed(out, pattern = "-", n = 2)[, 1])
  }

  get_title <- function(chr) {
    out <- chr[grep(pattern = " title =", x = chr)]
    out <- stringr::str_sub(string = out,
                            start = stringr::str_locate(string = out, pattern = " title = ")[, 2] + 1,
                            end = stringr::str_length(out) - 1)

    # Replace curly braces
    out <- stringr::str_replace_all(string = out, pattern = "\\{", replacement = "")
    out <- stringr::str_replace_all(string = out, pattern = "\\}", replacement = "")
    # native encodnig based on stringi::stri_enc_mark \"\\\\â€™\"
    # out <- stringr::str_replace_all(string = out, pattern = "\\’", replacement = "\\'")
    return(out)
  }

  result <- tibble::tibble(bibkey = purrr::map_chr(single_entries, get_bibkey),
                           author = purrr::map(single_entries, get_authors),
                           year   = purrr::map_int(single_entries, get_year),
                           title  = purrr::map_chr(single_entries, get_title))

  return(result)
}

