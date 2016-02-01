#' Nitrogen.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'Name' in the functionalGroups.csv file.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Timestep given as inter from 1 to the last timestep. To
#' convert to actual time lateron use 'toutinc' in the 'run.prm' file.}
#' \item{atoutput}{Obersavtion column storing the actual output value.
#' Nitrogen in mg N m-3.}
#' }
#' @source See \code{data-raw/data-preprocess.R} for further information.
"ref_n"
