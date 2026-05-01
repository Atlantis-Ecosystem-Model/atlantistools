#' Aggregate data using dplyr functionality.
#'
#' This function is a 'wrapper' for the group_by and summarize procedure
#' used in dplyr to aggregate dataframes.
#'
#' @param data Dataframe the aggregation is applied to.
#' @param col Column of the dataframe the summarise function is applied.
#' Default is \code{atoutput}.
#' @param groups Vector of character strings giving the grouping variables.
#' @param out Character string specifying the name of the output column.
#' Default is \code{atoutput}.
#' @param fun Aggregation function to apply.
#' @return grouped datarame with the aggregated data.
#' @export
#'
#' @examples
#' agg_ref_nums <- agg_data(data = ref_nums, groups = c("species", "agecl"), fun = mean)

agg_data <- function(data, col = "atoutput", groups, out = "atoutput", fun) {

  result <- group_data(data, groups = groups) |>
    dplyr::summarise(
      !!out := fun(.data[[col]], na.rm = TRUE),
      .groups = "drop"
    )
  return(result)

}


#' @export
#' @rdname agg_data
agg_perc <- function(data, col = "atoutput", groups, out = "atoutput") {

  result <- group_data(data, groups = groups) |>
    dplyr::mutate(!!out := .data[[col]] / sum(.data[[col]], na.rm = TRUE)) |>
    dplyr::ungroup()
  return(result)

}

#' @export
#' @rdname agg_data
group_data <- function(data, groups) {

  grouped_df <- data |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups)))

  return(grouped_df)
}
