#' Convert timestep to actual time!
#
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param data Dataframe having a column with information about the model timestep.
#' @param as_date Convert the time into a date format (yyyy-mm-dd) \code{TRUE} or
#' not \code{FALSE}. Default is \code{FALSE}.
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}. Only needed in case \code{as_date} is set to \code{TRUE}.
#' @return Dataframe whose column time is converted from timesteps to
#' actual time.
#' @family convert functions
#'
#' @examples
#' # Convert to time given in date format.
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' test <- convert_time(dir = d,
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    data = ref_nums,
#'    as_date = TRUE,
#'    modelstart = "1991-01-01")
#' head(test)
#'
#' # Convert time to years.
#' test <- convert_time(dir = d,
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    data = ref_nums)
#' head(test)

#' @export

# stock state date is given in days, nc-data is given in timesteps!

convert_time <- function(dir = getwd(), prm_run, data, as_date = FALSE, modelstart = NULL) {
  if (any(names(data) == "time")) {

    # Check if time is composed of a complete sequence of integers!
    # In this case there is no stockstate data! Remove zero!
    ts <- sort(unique(data$time))
    ts <- ts[ts != 0]
    # This tests if the timestps is a sequence of integers with at max 4 missing numbers between
    # sucessuve values. E.g. 1 6 7 8 12 is ok, 1 60 120 180 is not!
    if (all(diff(ts) < 5)) {
      toutinc <- extract_prm(dir = dir, prm_biol = prm_run, variables = "toutinc")

      # Convert timesteps to actual time!
      if (as_date) {# convert time to date!
        data$time <- as.Date.numeric(data$time * toutinc, origin = modelstart)
      } else {# convert time to years
        data$time <- data$time * toutinc / 365
      }
    } else {
      # Check if data is stock-state data!
      tsumout <- extract_prm(dir = dir, prm_biol = prm_run, variables = "tsumout")
      # remove entries in time which represent end of the year values in case tsumount != 365!
      # In addition also remove the last entry as it may neither be an end of the year value
      # nor a multiple of the stock-state timestep!
      ts <- sort(unique(data$time), decreasing = TRUE)[-1]
      if (tsumout != 365) ts <- ts[ts %% 365 != 0]
      if (all(ts %% tsumout == 0)) { # True stock state data!

        # Convert time in days to actual time!
        if (as_date) {
          data$time <- as.Date.numeric(data$time, origin = modelstart)
        } else {
          data$time <- data$time / 365
        }
      } else {
        stop("Provided dataframe has column 'time' but values are corrput. PLease contact package development Team.")
      }
    }
  } else {
    warning("No column 'time' present in dataframe. No time conversion applied!")
  }

  return(data)
}
