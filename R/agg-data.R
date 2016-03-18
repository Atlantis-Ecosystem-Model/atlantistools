#' Aggregate data using dplyr functionality.
#'
#' This function is a 'wrapper' for the group_by and summarise procedure
#' used in dplyr to aggregate dataframes.
#'
#' @param data Dataframe the aggregation is applied to.
#' @param col Column of the dataframe the summarise function is applied.
#' Default is 'atoutput'.
#' @param groups Vector of character strings giving the grouping variables.
#' @param out Character string specifying the name of the output column.
#' @return grouped datarame with the aggregated data.
#' @export
#'
#' @examples
#' agg_ref_nums <- agg_data(data = ref_nums, groups = c("species", "agecl"), fun = mean)

agg_data <- function(data, col = "atoutput", groups, out = "atoutput", fun){
  result <- group_data(data, groups = groups) %>%
    dplyr::summarise_(.dots = setNames(list(lazyeval::interp(~fun(var), var = as.name(col))), out))
  return(result)
}

#' @export
#' @rdname agg_mean
agg_perc <- function(data, col = "atoutput", groups, out = "atoutput"){
  result <- group_data(data, groups = groups) %>%
    dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~var/sum(var), var = as.name(col))), out))
  return(result)
}

#' @export
#' @rdname agg_mean
group_data <- function(data, groups) {
  dots = sapply(groups, . %>% {as.formula(paste0('~', .))})
  grouped_df <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots)
  return(grouped_df)
}


