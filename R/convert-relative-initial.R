#' Calculate relative timeseries using the initial value as benchmark.
#'
#' @param data Dataframe to apply the transformation to.
#' @param col Character value giving the name of the column to transform.
#' Default is \code{"atoutput"}.
#' @return Dataframe with transformed column 'col'.
#' @export
#'
#' @examples
#' df <- convert_relative_initial(ref_structn)
#' head(df[df$layer == 1, ], n = 15)

convert_relative_initial <- function(data, col = "atoutput") {
  if (!"time" %in% names(data)) stop("Column time is missing in data.")

  # Divide values by reference value (time = min(time))
  ref <- dplyr::ungroup(data)
  ref <- dplyr::filter_(ref, ~time == min(time))
  ref$time <- NULL
  names(ref)[names(ref) == col] <- "atoutput_ref"
  result <- data %>%
    dplyr::left_join(ref, by = names(data)[!names(data) %in% c("time", col)]) %>%
    dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~var / atoutput_ref, var = as.name(col))), col))

  # Replace division by 0 with 0!
  result$atoutput[result$atoutput_ref == 0] <- 0

  # Remove NAs. Some groups have missing values at first time step (carrion)
  result <- result[!is.na(result$atoutput), ]

  result$atoutput_ref <- NULL

  return(result)
}
