#' Dietmatrix.
#'
#' @format
#' \describe{
#' \item{pred}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{pred_stanza}{Predator stanza. 1 = juvenile; 2 = adult.}
#' \item{prey_stanza}{Prey stanza. 1 = juvenile; 2 = adult.}
#' \item{code}{Flag from the biological parameter file.}
#' \item{prey}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{avail}{Obersavtion column storing the actual output value.
#' Availability ranging from 0 to 1.}
#' \item{prey_id}{Preyid index based on the functional groups file.}
#' }
#' @source See \code{data-raw/data-create-reference-dfs.R} for further information.
"ref_dietmatrix"
