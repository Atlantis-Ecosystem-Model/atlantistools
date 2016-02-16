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
#' agg_ref_nums <- agg_mean(data = ref_nums, groups = c("species", "agecl"))

agg_mean <- function(data, col = "atoutput", groups, out = "atoutput"){
  dots = sapply(groups, . %>% {as.formula(paste0('~', .))})
  result <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise_(atoutput = lazyeval::interp(~mean(var), var = as.name(col)))
  if (out != "atoutput") names(result)[names(result) == "atoutput"] <- out
  return(result)
}

#' @export
#' @rdname agg_mean
agg_sum <- function(data, col = "atoutput", groups, out = "atoutput"){
  dots = sapply(groups, . %>% {as.formula(paste0('~', .))})
  result <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise_(atoutput = lazyeval::interp(~sum(var), var = as.name(col)))
  if (out != "atoutput") names(result)[names(result) == "atoutput"] <- out
  return(result)
}

#' @export
#' @rdname agg_mean
agg_perc <- function(data, col = "atoutput", groups, out = "atoutput"){
  dots = sapply(groups, . %>% {as.formula(paste0('~', .))})
  result <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::mutate_(atoutput = lazyeval::interp(~var/sum(var), var = as.name(col)))
  if (out != "atoutput") names(result)[names(result) == "atoutput"] <- out
  return(result)
}




