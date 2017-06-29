#' Extract the bibliographic info from www.fishbase.org.
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
#' df <- get_growth_fishbase("Scyliorhinus canicula")
#'
#' df$data_ref[df$data_ref == df$main_ref] <- NA
#' df <- tidyr::gather_(data = df,
#'                      key_col = "ref_type",
#'                      value_col = "ref_id",
#'                      gather_cols = c("main_ref", "data_ref"), na.rm = TRUE)
#' ref_id <- unique(df$ref_id)
#' get_ref_fishbase(ref_id)
#' }

# Modify this. Should be able to extraxt any reference information from fishbase. E.g. growth and diet!

get_ref_fishbase <- function(ref_id, mirror = "se") {
  # calculate perc ref entry and remove duplicates!
  # Complete NA entries (NA in every column) are removed at this point.
  # Thus there is no need to add NA handling to the function.
  # clean_ref$perc <- 1
  # clean_ref <- agg_data(clean_ref, col = "perc", groups = c("species", "ref_type", "ref_id"), out = "n", fun = sum) %>%
  #   agg_perc(., col = "n", groups = "species", out = "perc")

  # Extract data from fishbase.org
  ref <- purrr::map_chr(ref_id, ~paste0("http://www.fishbase.", mirror, "/References/FBRefSummary.php?ID=", .)) %>%
    purrr::map(., xml2::read_html) %>%
    purrr::map(., rvest::html_table)

  # Some reference links are broken on fishbase: e.g http://www.fishbase.se/References/FBRefSummary.php?ID=27034
  good_links <- purrr::map_lgl(ref, ~length(.) == 1)
  ref <- purrr::map_if(ref, good_links, ~.[[1]][1, 2])
  ref[!good_links] <- NA
  ref <- purrr::flatten_chr(ref)

  # Create datatable.
  clean_ref <- tibble::tibble(ref_id, ref)

  # Extract additional information from references
  clean_ref$year <- as.integer(purrr::map_chr(purrr::map_if(clean_ref$ref, ~!is.na(.), str_split_twice), 1))
  clean_ref$author <- stringr::str_sub(clean_ref$ref, end = stringr::str_locate(string = clean_ref$ref, pattern = as.character(clean_ref$year))[, 1] - 3)
  clean_ref$title <- stringr::str_sub(clean_ref$ref, start = stringr::str_locate(string = clean_ref$ref, pattern = paste(clean_ref$author, clean_ref$year, sep = ", "))[, 2] + 3)

  return(clean_ref)
}


