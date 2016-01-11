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
#' bps <- load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")
#' bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))
#' test <- load_nc(dir = d, nc = "outputSETAS.nc",
#'   bps = bps,
#'   fgs = "functionalGroups.csv",
#'   select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
#'   select_variable = "Nums",
#'   bboxes = bboxes,
#'   check_acronyms = TRUE)
#' test <- convert_time(dir = d,
#'   prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
#'   data = test,
#'   modelstart = "1991-01-01")

#' @export

convert_time <- function(dir, prm_run, data, modelstart){
  if (!is.null(dir)) prm_run <- file.path(dir, prm_run)
  prm_run <- readLines(con = prm_run)

  toutinc <- extract_param(chars = prm_run, variable = "toutinc")
  if (any(names(data) == "time")) data$time <- with(data, as.Date.numeric(time * toutinc, origin = modelstart))
  return(data)
}
