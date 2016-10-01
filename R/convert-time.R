#' Convert timestep to actual time!
#
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param col Numeric vector. Usually a column in a dataframe with information about time.
#' Either given as timesteps or days.
#' @return Numeric vector with the time in years.
#' @family convert functions
#'
#' @examples
#' # Convert time to years.
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' prm_run <- "VMPA_setas_run_fishing_F_New.prm"
#' unique(convert_time(dir = d, prm_run = prm_run, col = ref_nums$time))
#' unique(convert_time(dir = d, prm_run = prm_run, col = ref_eat$time))

#' @export

# stock state date is given in days, nc-data is given in timesteps!

convert_time <- function(dir = getwd(), prm_run, col) {
  if (!is.numeric(col)) stop("Col is nont numeric. No time transformation applied.")

  # Check if time is composed of a complete sequence of integers!
  # In this case there is no stockstate data! Remove zero!
  ts <- sort(unique(col))
  ts <- ts[ts != 0]
  # This tests if the timestps are a sequence of integers with at max 4 missing numbers between
  # sucessuve values. E.g. 1 6 7 8 12 is ok, 1 60 120 180 is not! This will break in case of very
  # short output timesteps!
  if (all(diff(ts) < 5)) {
    toutinc <- extract_prm(dir = dir, prm_biol = prm_run, variables = "toutinc")

    # Convert timesteps to time in years
    data <- col * toutinc / 365
  } else {
    # Check if data is stock-state data!
    tsumout <- extract_prm(dir = dir, prm_biol = prm_run, variables = "tsumout")
    # remove entries in time which represent end of the year values in case tsumount != 365!
    # In addition also remove the last entry as it may neither be an end of the year value
    # nor a multiple of the stock-state timestep!
    ts <- sort(unique(col), decreasing = TRUE)[-1]
    if (tsumout != 365) ts <- ts[ts %% 365 != 0]
    if (all(ts %% tsumout == 0)) { # True stock state data!

      # Convert time in days to time in years
      data <- col / 365
    } else {
      stop("Provided dataframe has column 'time' but values are corrput. PLease contact package development Team.")
    }
  }
  return(data)
}
