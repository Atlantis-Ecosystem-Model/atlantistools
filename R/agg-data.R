#' Aggregate data using dplyr functionality.
#'
#' This function is a 'wrapper' for the group_by and summarise procedure
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

agg_data <- function(data, col = "atoutput", groups, out = "atoutput", fun){
  result <- group_data(data, groups = groups) %>%
    dplyr::summarise_(.dots = stats::setNames(list(lazyeval::interp(~fun(var, na.rm = TRUE), var = as.name(col))), out))
  return(dplyr::ungroup(result))
}

#' @export
#' @rdname agg_data
agg_perc <- function(data, col = "atoutput", groups, out = "atoutput"){
  result <- group_data(data, groups = groups) %>%
    dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~var/sum(var), var = as.name(col))), out))
  return(dplyr::ungroup(result))
}

#' @export
#' @rdname agg_data
group_data <- function(data, groups) {
  dots = sapply(groups, . %>% {stats::as.formula(paste0('~', .))})
  grouped_df <- dplyr::ungroup(data) %>%
    dplyr::group_by_(.dots = dots)
  return(grouped_df)
}
