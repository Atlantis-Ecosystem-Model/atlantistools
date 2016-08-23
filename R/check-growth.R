#' This function is used to check the individual growth per group over time.
#'
#' @param data Dataframe with information about individual age based growth over time.
#' This should be generated with \code{preprocess()}. You can test either structural or
#' reserve weight.
#' @return Dataframe showing the output of the linear model fit (slope & F-statistic) per
#' group and age.
#' @export
#' @family check functions
#'
#' @examples
#' check_growth(preprocess_setas$structn_age)
#' check_growth(preprocess_setas$resn_age)

check_growth <- function(data) {
  check_df_names(data, expect = c("species", "time", "agecl", "atoutput"))

  # Divide output with initial value!
  ref <- data[data$time == min(data$time), ]
  ref$time <- NULL
  names(ref)[names(ref) == "atoutput"] <- "atoutput_ref"
  result <- data %>%
    dplyr::left_join(ref) %>%
    dplyr::mutate_(atoutput = ~atoutput / atoutput_ref)
  result$atoutput[result$atoutput_ref == 0] <- 0

  # Split dataframe into species and ages!
  dfs <- split(result, data$species)
  dfs <- lapply(dfs, function(x) split(x, x$agecl))

  # Fit group specific linear model and extract slope and sign test for each age.
  fit_lm <- function(ls) {
    lms <- Map(f = function(x, y) lm(formula = atoutput ~ time - 1, data = x, offset = y),
               ls, lapply(ls, function(x) rep(1, times = nrow(x))))
    slopes <- vapply(lms, FUN = function(x) x$coefficients, FUN.VALUE = numeric(1))
    pvalues <- vapply(lms, FUN = function(x) summary(x)$coefficients[1, 4], FUN.VALUE = numeric(1))
    return(data.frame(species = ls[[1]]$species[1],
                      agecl = as.numeric(names(slopes)),
                      slope = slopes,
                      pvalue = pvalues, stringsAsFactors = FALSE))
  }

  # Combine output to dataframe.
  result <- lapply(dfs, fit_lm)
  result <- do.call(rbind, result)
  row.names(result) <- NULL
  return(result)
}






