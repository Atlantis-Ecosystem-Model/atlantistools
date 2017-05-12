#' Combine values from different groups if specific groups only have a low contribution to the overall value.
#'
#' @param data Dataframe whose groups shall be combined.
#' @param group_col Character string giving the name of the group column in `data`.
#' @param groups Vector of character strings giving the grouping variables.
#' @param combine_thresh Integer indicating the number of groups to display. Default is \code{15}.
#' @return dataframe with groups combined to "Rest" if contribution is low.
#' @export
#' @family combine functions
#'
#' @examples
#' df <- combine_groups(ref_dm, group_col = "prey")
#' df <- combine_groups(ref_dm, group_col = "prey", combine_thresh = 2)

combine_groups <- function(data,
                           group_col,
                           groups = names(data)[!is.element(names(data), c("atoutput", "time", group_col))],
                           combine_thresh = 15) {

  if (combine_thresh <= 1) stop("Minimum value for 'combine_thresh' is 2.")
  # Sum values over timesteps
  comb_grps <- agg_data(data, groups = c(groups, group_col), fun = sum)

  # Arrange by group_col and group by groups to select the 1:combinthethresh
  # group in group_col for each grouping combination.
  if (length(groups) > 0) {
    comb_grps <- comb_grps %>%
      as.data.frame() %>%
      group_data(groups = groups)
  }
  imp_species <- comb_grps %>%
    dplyr::arrange_(~desc(atoutput)) %>%
    dplyr::slice(1:(combine_thresh - 1))
  imp_species$atoutput <- NULL

  # Now we have a dataframe with the most important species (the number is defined
  # by combine_thresh) for each grouping combination! We can use this to anti_join (!!!)
  # with our data!
  low_contrib <- dplyr::anti_join(data, imp_species)

  if (nrow(low_contrib) != 0) { # Only combine groups if necessary!
    low_contrib[, group_col] <- "Rest"

    new_data <- dplyr::inner_join(data, imp_species) %>%
      rbind(low_contrib)

    # This should never happen...
    if (any(dim(data) != dim(new_data))) stop("Please contact package development Team.")

    # Finally aggregate the new dataset to combine values from newly introduced
    # 'Rest' group. This is a bit messy because I assume that each column (excpect
    # 'atoutput') is a grouping column.
    comb_data <- agg_data(new_data, groups = names(data)[!is.element(names(data), "atoutput")], fun = sum)
    return(comb_data)
  } else {
    return(data)
  }
}
