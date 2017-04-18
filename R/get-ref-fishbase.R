#' Extract the bibliographic info from www.fishbase.org.
#'
#'
#' Extract bibliographic information for growth parameters (linf, k, t0) from www.fishbase.org
#' @param growth_fishbase Dataframe generated with \link{get_growth_fishbase}.
#' @return Dataframe
#' @export
#'
#' @examples
#' fish <- c("Gadus morhua", "Merlangius merlangus")
#' growth_fishbase <- get_growth_fishbase(fish)
#' get_ref_fishbase(growth_fishbase)
#' growth_fishbase <- get_growth_fishbase("Scyliorhinus canicula")

get_ref_fishbase <- function(growth_fishbase, mirror = "se") {
  # extract unique reference and species combinations
  clean_ref <- dplyr::select_(growth_fishbase, .dots = c("species", "main_ref", "data_ref"))

  # remove duplicated data_ref entries
  clean_ref$data_ref[clean_ref$data_ref == clean_ref$main_ref] <- NA
  clean_ref <- tidyr::gather_(data = clean_ref, key_col = "ref_type", value_col = "ref_id", gather_cols = c("main_ref", "data_ref"), na.rm = TRUE)

  # calculate perc ref entry and remove duplicates!
  clean_ref$perc <- 1
  clean_ref <- agg_data(clean_ref, col = "perc", groups = c("species", "ref_type", "ref_id"), out = "n", fun = sum) %>%
    agg_perc(., col = "n", groups = c("species"), out = "perc")

  # Extract data from fishbase.org
  ref <- purrr::map_chr(clean_ref$ref_id, ~paste0("http://www.fishbase.", mirror, "/References/FBRefSummary.php?ID=", .)) %>%
    purrr::map(., xml2::read_html) %>%
    purrr::map(., rvest::html_table) %>%
    purrr::map_chr(., ~.[[1]][1, 2]) # annoying nested list...

  # Add references to datatable.
  if (length(ref) == nrow(clean_ref)) {
    clean_ref$ref <- ref
  } else {
    stop("Some reference IDs are wrong.") # Pretty sure this does never happen...
  }

  return(clean_ref)
}


