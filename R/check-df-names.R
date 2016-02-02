#' Function to check the names of a dataframe.
#'
#' This function is used in most plotting routines to check if the correct dataframe
#' is passed.
#'
#' @param data Dataframe to check.
#' @param expect Character vector giving the names of the expected columns
#' @export
#'
#' @examples \dontrun{
#' check_df_names(preprocess_setas$biomass_age, expect = c("time", "species", "atoutput", "ages"))
#' }

check_df_names <- function(data, expect) {
  df_names <- names(data)
  wrong_names <- df_names[!is.element(df_names, expect)]

  if (length(wrong_names) > 1) {
    stop(paste("Columns", paste(wrong_names, collapse = " & "), "not found in data."))
  }
}




