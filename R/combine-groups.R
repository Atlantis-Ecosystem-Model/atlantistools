#' Combine values from different groups if specific groups only have a low contribution to the overall value.
#'
#' @param data Dataframe whose grouops shall be combined.
#' @param group_col Character string giving the name of the group column in `data`.
#' @param groups Vector of character strings giving the grouping variables.
#' @param combine_thresh Numeric indicating minimum amount to the stomach contribution.
#' Each prey item with a lower contribution as this treshold is assigned to the
#' grpup "Rest". This is necessary for species with a wide variety of food items
#' in their diet. Otherwise the screen will be cluttered with colors in the
#' dietplots. Default is 0.03.
#' @return dataframe with groupgs combined to "Rest" if contribution is low.
#' @export
#'
#' @examples
#' combine_groups(preprocess_setas$diet_dietcheck,
#'    group_col = "prey",
#'    groups = c("time", "pred", "habitat"),
#'    combine_thresh = 0.03)

combine_groups <- function(data, group_col, groups, combine_thresh = 0.03) {
  # Convert values to percent!
  data <- agg_perc(data, groups = groups, out = "test")

  # Get species which always have a low contribution!
  ts <- length(unique(data$time))
  low_contrib <- data %>%
    dplyr::filter_(~test <= combine_thresh) %>%
    group_data(groups = c(groups[groups != "time"], group_col)) %>%
    dplyr::summarise_(count = ~length(unique(time))) %>%
    dplyr::filter_(~count == ts)
  low_contrib$count <- NULL

  # Get position of merge between data and low_contrib
  full_match <- matrix(nrow = nrow(data), ncol = ncol(low_contrib))
  col_names <- names(low_contrib)
  for (i in seq_along(col_names)) {
    full_match[, i] <- !is.na(match(data[, col_names[i]][[1]], low_contrib[, col_names[i]][[1]]))
  }

  # Replace resulting entries with "Rest"
  data[rowSums(full_match) == ncol(full_match), group_col] <- "Rest"

  # data[data$test <= combine_thresh, group_col] <- "Rest"
  data <- agg_data(data, groups = c(groups, group_col), fun = sum)
  return(data)
}
