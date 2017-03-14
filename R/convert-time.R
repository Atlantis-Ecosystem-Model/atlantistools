#' Convert timestep to actual time!
#
#' @inheritParams load_nc
#' @param col Numeric vector. Usually a column in a dataframe with information about time.
#' Either given as timesteps or days.
#' @return Numeric vector with the time in years.
#' @family convert functions

#' @export

# stock state date is given in days, nc-data is given in timesteps!

convert_time <- function(prm_run, col) {
  if (!is.numeric(col)) stop("Col is not numeric. No time transformation applied.")

  # Check if time is composed of a complete sequence of integers!
  # In this case there is no stockstate data! Remove zero!
  ts <- sort(unique(col))
  ts <- ts[ts != 0]
  # This tests if the timestps are a sequence of integers with at max 4 missing numbers between
  # sucessuve values. E.g. 1 6 7 8 12 is ok, 1 60 120 180 is not! This will break in case of very
  # short output timesteps!
  if (all(diff(ts) %in% 1:4)) {
    toutinc <- extract_prm(prm_biol = prm_run, variables = "toutinc")

    # Convert timesteps to time in years
    data <- col * toutinc / 365
    return(data)
  } else {
    # Check if data is stock-state data!
    tsumout <- extract_prm(prm_biol = prm_run, variables = "tsumout")
    # remove entries in time which represent end of the year values in case tsumount != 365!
    # In addition also remove the last entry as it may neither be an end of the year value
    # nor a multiple of the stock-state timestep!
    ts <- sort(unique(col), decreasing = TRUE)[-1]
    if (tsumout != 365) ts <- ts[ts %% 365 != 0]
    if (all(ts %% tsumout == 0)) { # True stock state data!

      # Convert time in days to time in years
      data <- col / 365
      return(data)
    } else {
      stop("Values are corrput. PLease contact package development Team.")
    }
  }
}
