agg_mean <- function(data, col = "atoutput", vars){
  dots = sapply(vars, . %>% {as.formula(paste0('~', .))})
  result <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise_(atoutput = lazyeval::interp(~mean(var), var = as.name(col)))
  return(result)
}

agg_sum <- function(data, col = "atoutput", vars){
  dots = sapply(vars, . %>% {as.formula(paste0('~', .))})
  result <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise_(atoutput = lazyeval::interp(~sum(var), var = as.name(col)))
  return(result)
}
