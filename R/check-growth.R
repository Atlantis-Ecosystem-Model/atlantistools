#' This function is used to check the individual growth per group over time.
#'
#' @param data Dataframe with information about individual age based growth over time.
#' This should be generated with \code{preprocess()}. You can test either structural or
#' reserve weight.
#' @param yearly Logical specifying if relative change in individual weight shall be calculated
#' on a yearly basis (\code{TRUE}) ot not (\code{FALSE}). Default is \code{FALSE}.
#' @return Dataframe showing the output of the linear model fit (slope & F-statistic) per
#' group and age.
#' @export
#' @family check functions
#'
#' @examples
#' check_growth(preprocess$structn_age)
#' check_growth(preprocess$resn_age)
#' check_growth(preprocess$resn_age, yearly = TRUE)

check_growth <- function(data, yearly = FALSE) {
  check_df_names(data, expect = c("species", "time", "agecl", "atoutput"))

  # if (!is.null(filter_time) & is.numeric(filter_time)) {
  #   data <- data[data$time <= filter_time, ]
  # }

  cleanup <- function(ls) {
    df <- do.call(rbind, ls)
    row.names(df) <- NULL
    df
  }

  # Divide output with initial value!
  # ref <- data[data$time == min(data$time), ]
  # ref$time <- NULL
  # names(ref)[names(ref) == "atoutput"] <- "atoutput_ref"
  # result <- data %>%
  #   dplyr::left_join(ref) %>%
  #   dplyr::mutate_(atoutput = ~atoutput / atoutput_ref)
  # result$atoutput[result$atoutput_ref == 0] <- 0
  # outcomment in case lm procedure is used! This is a bit messy.
  result <- data

  # Split dataframe into species and age specific subdataframes!
  dfs <- split(result, data$species)
  dfs <- lapply(dfs, function(x) split(x, x$agecl))

  rel_change_year <- function(ls) {
    # split into subyears-data
    rcy <- lapply(ls, function(x) split(x, trunc(x$time)))
    # Extract yearly individual weight values
    for (i in seq_along(rcy)) {
      rcy[[i]] <- lapply(rcy[[i]], function(x) x$atoutput)
      rcy[[i]] <- vapply(rcy[[i]], FUN = function(x) (max(x) - min(x)) / min(x), FUN.VALUE = numeric(1))
    }

    # Combine result to dataframe!
    data.frame(species = ls[[1]]$species[1],
               agecl = as.numeric(unlist(Map(rep, names(rcy), sapply(rcy, length)))),
               time = as.numeric(gsub("^[^.]+.\\s*", "", names(unlist(rcy)))),
               relchange = unlist(rcy), stringsAsFactors = FALSE)
  }

  rel_change_overall <- function(ls) {
    rc <- vapply(ls, FUN = function(x) (max(x$atoutput) - min(x$atoutput)) / min(x$atoutput), FUN.VALUE = numeric(1))

    # Combine result to dataframe!
    data.frame(species = ls[[1]]$species[1],
               agecl = as.numeric(names(rc)),
               relchange = rc, stringsAsFactors = FALSE)
  }

  if (yearly) {
    result <- lapply(dfs, rel_change_year)
  } else {
    result <- lapply(dfs, rel_change_overall)
  }

  # Fit group specific linear model and extract slope and sign test for each age.
  # fit_lm <- function(ls) {
  #   lms <- Map(f = function(x, y) lm(formula = atoutput ~ time - 1, data = x, offset = y),
  #              ls, lapply(ls, function(x) rep(1, times = nrow(x))))
  #   slopes <- vapply(lms, FUN = function(x) x$coefficients, FUN.VALUE = numeric(1))
  #   pvalues <- vapply(lms, FUN = function(x) summary(x)$coefficients[1, 4], FUN.VALUE = numeric(1))
  #   return(data.frame(species = ls[[1]]$species[1],
  #                     agecl = as.numeric(names(slopes)),
  #                     slope = slopes,
  #                     pvalue = pvalues, stringsAsFactors = FALSE))
  # }
  #
  # # Combine output to dataframe.
  # result <- lapply(dfs, fit_lm)

  cleanup(result)
}


