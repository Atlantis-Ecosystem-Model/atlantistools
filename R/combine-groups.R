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
#' combine_groups(preprocess_setas$diet,
#'    group_col = "prey",
#'    groups = c("time", "pred", "habitat"),
#'    combine_thresh = 0.03)

combine_groups <- function(data, group_col, groups, combine_thresh = 0.03) {
  # Convert values to percent!
  data <- agg_perc(data, groups = groups, out = "test")
  data[data$test <= combine_thresh, group_col] <- "Rest"
  data <- agg_data(data, groups = c(groups, group_col), fun = sum)
  return(data)
}
