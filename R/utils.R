# This function is used to debug the NS model.
# Health care warning: Heavy use of the assign function...
# Variables are created in the GlobalEnvironment...
# debug_ns <- function() {
#   d <- "z:/Atlantis_models/Runs/dummy_01_ATLANTIS_NS/"
#   assign("dir", d, envir = .GlobalEnv)
#   assign("fgs", "functionalGroups.csv", envir = .GlobalEnv)
#   assign("nc", "outputNorthSea.nc", envir = .GlobalEnv)
#   assign("nc_gen", "outputNorthSea.nc", envir = .GlobalEnv)
#   assign("nc_prod", "outputNorthSeaPROD.nc", envir = .GlobalEnv)
#   assign("dietcheck", "outputNorthSeaDietCheck.txt", envir = .GlobalEnv)
#   assign("yoy", "outputNorthSeaYOY.txt", envir = .GlobalEnv)
#   assign("ssb", "outputNorthSeaSSB.txt", envir = .GlobalEnv)
#   assign("specmort", "outputNorthSeaSpecificMort.txt", envir = .GlobalEnv)
#   assign("specpredmort", "outputNorthSeaSpecificPredMort.txt", envir = .GlobalEnv)
#   assign("prm_biol", "NorthSea_biol_fishing.prm", envir = .GlobalEnv)
#   assign("prm_run", "NorthSea_run_fishing_F.prm", envir = .GlobalEnv)
#   assign("bps", load_bps(dir = d, fgs = "functionalGroups.csv", init = "init_NorthSea.nc"), envir = .GlobalEnv)
#   assign("fgs", "functionalGroups.csv", envir = .GlobalEnv)
#   assign("select_groups", get_groups(dir = d, fgs = "functionalGroups.csv"), envir = .GlobalEnv)
#   assign("bboxes", get_boundary(load_box(dir = d, bgm = "NorthSea.bgm")), envir = .GlobalEnv)
#   assign("out", "preprocess-north-sea.rda", envir = .GlobalEnv)
#   assign("save_to_disc", FALSE, envir = .GlobalEnv)
#   assign("check_acronyms", TRUE, envir = .GlobalEnv)
#   assign("report", TRUE, envir = .GlobalEnv)
#   assign("warn_zeros", TRUE, envir = .GlobalEnv)
# }

# split dataframe in list of dataframe based on multiple (or a single) column.
multisplit <- function(df, groups) {
  for (i in seq_along(groups)) {
    # first split results in a list of dataframes!
    if (i == 1) result <- split(df, df[, groups[i]])
    # sucessive splits work with a list of dataframes.
    if (i > 1) {
      result <- purrr::map(result, function(x) split(x, x[, groups[i]]))
      # remove one dimension of the list to allow for the next split to work.
      result <- purrr::flatten(result)
    }
  }
  return(result)
}

# This function is not used anymore.
file_ending <- function(filename, ending = "nc") {
  file_ending <- strsplit(filename, "\\.")[[1]][length(strsplit(filename, "\\.")[[1]])]
  if (file_ending != ending) stop(paste("The file", filename, "does not end in", ending))
}


release_questions <- function() {
  c(
    "Have you compressed the vignettes with tools::compactPDF(gs_quality = 'ebook')",
    "Have you updated the version number",
    "Have you used 'args = --no-build-vignettes' in devtools::release()?",
    "Have you updated the vignette index from the local package installation?",
    "Have you used 'args = '--compact-vignettes=both'' in devtools::release()",
    "Are the vignettes updated",
    "Have you checked the vignette size after devtools::build(args = '--compact-vignettes=both')"
  )
}

# Convert bib file to tidy dataframe!
# bib2df does use incorrect enconding in readLines call
# Author names are cleaned.
# This might not work in case different bib encondings are used.
bib_to_df <- function(bib = "gns-input-data.bib") {
  bib_df <- readLines(bib, encoding = "UTF-8")
  block_ids <- grep(pattern = "@", x = bib_df)
  single_entries <- vector(mode = "list", length = length(block_ids))
  for (i in 1:(length(single_entries) - 1)) {
    single_entries[[i]] <- bib_df[block_ids[i]:(block_ids[i + 1] - 1)]
  }

  # Add last entry ba hand
  mid <- length(single_entries)
  if (is.null(single_entries[[mid]])) single_entries[[mid]] <- bib_df[block_ids[mid]:length(bib_df)]

  # chr <- single_entries[[11]]
  get_bibkey <- function(chr) {
    out <- chr[grep(pattern = "@", x = chr)]
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
    out <- stringr::str_replace_all(string = out, pattern = "\\’", replacement = "\\'")
    return(out)
  }

  result <- tibble(bibkey = purrr::map_chr(single_entries, get_bibkey),
                   author = purrr::map(single_entries, get_authors),
                   year = purrr::map_int(single_entries, get_year),
                   title = purrr::map_chr(single_entries, get_title))

  return(result)
}

# Convert reference information to bib-tex-key
# @param ref_df dataframe with columns author, year, title
# in case title is missing match is performed based on author and year only.
# ref_df <- left_join(linfk_ref, refs_find) %>%
#   select(ref, author, year, title)
ref_to_bibkey <- function(ref_df, bib = "gns-input-data.bib") {
  ref_bib <- bib_to_df(bib = bib)

  # single_ref <- ref_df[91, ]
  single_bibtex <- function(single_ref, ref_bib) {
    # Match by year
    bibkey <- filter(ref_bib, year == single_ref$year)
    if (nrow(bibkey) > 0) {
      # Match by title
      bibkey <- bibkey[purrr::map_lgl(bibkey$author, ~sum(stringr::str_detect(pattern = ., string = single_ref$author), na.rm = T) > 0), ]
      if (nrow(bibkey) > 0 & !is.na(single_ref$title)) {
        # Only use data with matching title.
        words <- str_split(bibkey$title, pattern = " ")
        # remove punctuation marks and such
        words <- map(words, str_replace_all, pattern = "[\\,\\.\\:\\[\\]]", replacement = "")
        counts <- map(words, ~stringr::str_detect(string = single_ref$title, coll(., ignore_case = T)))
        matches <- map_dbl(counts, ~sum(.)/length(.))
        max_match <- which(max(matches) == matches)

        if (length(max_match) == 1 && matches[max_match] > 0.8) return(bibkey$bibkey[max_match])
      } else {
        # Return unique-bibkey in case no title is present but year and author yield one specicif bibkey
        if (nrow(bibkey) == 1) return(bibkey$bibkey)
      }
    }
    return(NA)
  }

  map_chr(1:nrow(ref_df), ~single_bibtex(single_ref = ref_df[., ], ref_bib = ref_bib))
}
