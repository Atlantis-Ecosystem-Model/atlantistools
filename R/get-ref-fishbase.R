#' Extract the bibliographic info from www.fishbase.org.
#'
#'
#' Extract bibliographic information for growth parameters (linf, k, t0) from www.fishbase.org
#' @inheritParams get_growth_fishbase
#' @param growth_fishbase Dataframe generated with \link{get_growth_fishbase}.
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' growth_fishbase <- get_growth_fishbase(fish)
#' get_ref_fishbase(growth_fishbase)
#'
#' growth_fishbase <- get_growth_fishbase("Scyliorhinus canicula")
#' df <- get_ref_fishbase(growth_fishbase)
#' }

get_ref_fishbase <- function(growth_fishbase, mirror = "se") {
  # extract unique reference and species combinations
  clean_ref <- dplyr::select_(growth_fishbase, .dots = c("species", "main_ref", "data_ref"))

  # remove duplicated data_ref entries
  clean_ref$data_ref[clean_ref$data_ref == clean_ref$main_ref] <- NA
  clean_ref <- tidyr::gather_(data = clean_ref, key_col = "ref_type", value_col = "ref_id", gather_cols = c("main_ref", "data_ref"), na.rm = TRUE)

  # calculate perc ref entry and remove duplicates!
  # Complete NA entries (NA in every column) are removed at this point.
  # Thus there is no need to add NA handling to the function.
  clean_ref$perc <- 1
  clean_ref <- agg_data(clean_ref, col = "perc", groups = c("species", "ref_type", "ref_id"), out = "n", fun = sum) %>%
    agg_perc(., col = "n", groups = "species", out = "perc")

  # Extract data from fishbase.org
  ref <- purrr::map_chr(clean_ref$ref_id, ~paste0("http://www.fishbase.", mirror, "/References/FBRefSummary.php?ID=", .)) %>%
    purrr::map(., xml2::read_html) %>%
    purrr::map(., rvest::html_table)

  # Some reference links are broken on fishbase: e.g http://www.fishbase.se/References/FBRefSummary.php?ID=27034
  good_links <- purrr::map_lgl(ref, ~length(.) == 1)
  ref <- purrr::map_if(ref, good_links, ~.[[1]][1, 2])
  ref[!good_links] <- NA
  ref <- purrr::flatten_chr(ref)

  # Add references to datatable.
  if (length(ref) == nrow(clean_ref)) {
    clean_ref$ref <- ref
  } else {
    stop("Some reference IDs are wrong.") # Pretty sure this does never happen...
  }

  # Extract additional information from references
  clean_ref$year <- as.integer(purrr::map_chr(purrr::map_if(clean_ref$ref, ~!is.na(.), str_split_twice), 1))
  clean_ref$author <- stringr::str_sub(clean_ref$ref, end = stringr::str_locate(string = clean_ref$ref, pattern = as.character(clean_ref$year))[, 1] - 3)

  return(clean_ref)
}


