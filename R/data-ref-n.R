#' Nitrogen.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obersavtion column storing the actual output value.
#' Nitrogen in mg N m-3.}
#' }
#' @source See \code{data-raw/data-create-reference-dfs.R} for further information.
"ref_n"
