#' Eat.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'Name' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to 10.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{time}{Timestep given as inter from 1 to the last timestep. To
#' convert to actual time lateron use 'toutinc' in the 'run.prm' file.}
#' \item{atoutput}{Obersavtion column storing the actual output value.
#' Grazing in mg N m-3 d-1.}
#' }
#' @source
#' \describe{
#' \item{Function}{load_nc}
#' \item{dir}{system.file("extdata", "setas-model-new-trunk", package = "atlantistools")}
#' \item{nc}{"outputSETASPROD.nc"}
#' \item{bps}{load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")}
#' \item{fgs}{"functionalGroups.csv"}
#' \item{select_groups}{get_groups(dir = d, fgs = "functionalGroups.csv")}
#' \item{select_variable}{"Eat"}
#' \item{bboxes}{get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))}
#' \item{check_acronsm}{TRUE}}
"ref_eat"
