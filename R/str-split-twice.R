#' Extract numeric values from string.
#'
#' The function splits any character string at each tab and space and returns
#' all (min_only = FALSE) or only the first (min_only = T) numeric value found in the string.
#'
#' @param char Character string.
#' @param min_only Logical specifying if only the first numeric value (\code{TRUE}) or
#' all numeric values (\code{FALSE}) should be returned. Default is \code{TRUE}.
#' @return numeric values forund in the passed string.
#' @export
#'
#' @examples
#' str_split_twice(char = "Hello   15")
#' str_split_twice(char = "flag1 15  16\t15", min_only = FALSE)

# Split any character string multiple times and retrun the first (min_only = T)
# or all (min_only = F) numeric values found.
str_split_twice <- function(char, min_only = TRUE){
  patterns <- c(" ", "\t", ",", "\n", "\r")
  if (all(!stringr::str_detect(string = char, pattern = patterns))) {
    stop("Neither space nor tab present.")
  }
  for (i in seq_along(patterns)) {
    char <- unlist(stringr::str_split(string = char, pattern = patterns[i]))
  }
  char <- suppressWarnings(as.numeric(char))
  if (all(is.na(char))) stop("No numeric value present.")
  char <- char[!is.na(char)]
  if (min_only) char <- char[1]
  return(char)
}
