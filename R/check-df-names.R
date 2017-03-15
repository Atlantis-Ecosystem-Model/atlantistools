#' Function to check the names of a dataframe.
#'
#' This function is used in most plotting routines to check if the correct dataframe
#' is passed.
#'
#' @param data Dataframe to check.
#' @param expect Character vector giving the names of the expected columns
#' @param optional Character vector giving the names of optional columns.
#' Default is \code{NULL}.
#' @export
#'
#' @examples \dontrun{
#' check_df_names(preprocess$biomass_age, expect = c("time", "species", "atoutput", "ages"))
#' }

check_df_names <- function(data, expect, optional = NULL) {
  df_names <- names(data)[!is.element(names(data), optional)]
  wrong_names <- df_names[!is.element(df_names, expect)]

  if (length(wrong_names) >= 1) {
    stop(paste("Wrong column names in data:", paste(wrong_names, collapse = " & ")))
  }
}




