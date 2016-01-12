#' Convert timestep to actual time!
#
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param data Dataframe with column having information about the model timestep.
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}.
#' @return Dataframe whose column time is converted from timesteps to
#' actual time.
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' test <- convert_time(dir = d,
#'    prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
#'    data = ref_nums,
#'    modelstart = "1991-01-01")
#' head(test)

#' @export

convert_time <- function(dir, prm_run, data, modelstart){
  if (!is.null(dir)) prm_run <- file.path(dir, prm_run)
  prm_run <- readLines(con = prm_run)

  toutinc <- extract_param(chars = prm_run, variable = "toutinc")
  if (any(names(data) == "time")) data$time <- with(data, as.Date.numeric(time * toutinc, origin = modelstart))
  return(data)
}
