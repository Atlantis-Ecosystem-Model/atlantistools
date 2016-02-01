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
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}.
#' @param stock_state Logical indicating if a stock_state dataframe is changed.
#' @return Dataframe whose column time is converted from timesteps to
#' actual time.
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' test <- convert_time(dir = d,
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    data = ref_nums,
#'    modelstart = "1991-01-01")
#' head(test)

#' @export

convert_time <- function(dir = getwd(), prm_run, data, modelstart, stock_state = FALSE){
  if (!is.null(dir)) prm_run <- file.path(dir, prm_run)
  prm_run <- readLines(con = prm_run)

  if (!stock_state) {
    toutinc <- extract_param(chars = prm_run, variable = "toutinc")
    if (any(names(data) == "time")) data$time <- with(data, as.Date.numeric(time * toutinc, origin = modelstart))
  } else {
    if (any(names(data) == "time")) data$time <- with(data, as.Date.numeric(time, origin = modelstart))
  }

  return(data)
}
