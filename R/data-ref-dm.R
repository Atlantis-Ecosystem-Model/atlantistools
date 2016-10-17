#' Dietcheck.
#'
#' @format
#' \describe{
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{pred}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{prey}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{atoutput}{Obersavtion column storing the actual output value.
#' Diet contribution in percentage.}
#' }
#' @source See \code{data-raw/data-preprocess.R} for further information.
"ref_dm"
