#'  Dietmatrix.
#'
#' See \code{data-raw/data-create-reference-dfs.R} for further information.
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
#' \item{avail}{Obseravtion column storing the actual output value.
#' Availability ranging from 0 to 1.}
#' \item{prey_id}{Preyid index based on the functional groups file.}
#' }
"ref_dietmatrix"

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
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Diet contribution in percentage.}
#' }
"ref_dm"

#' Eat.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Consumption in mg N m-3 d-1.}
#' }
"ref_eat"

#' Grazing.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Grazing in mg N m-3 d-1.}
#' }
"ref_grazing"

#' Growth.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Growth in mg N d-1.}
#' }
"ref_growth"

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
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Nitrogen in mg N m-3.}
#' }
"ref_n"

#' Numbers at age data.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Numbers.}
#' }
"ref_nums"

#' Physical variables.
#'
#' @format
#' \describe{
#' \item{variable}{Name of the physical variable: "salt", "NO3", "NH3", "Temp", "Chl_a" and "Denitrifiction".}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' units are salt = PSU; NO3, NH3 = mg N m-3; Temp = degrees Celcius; Chl_a, Denitrifiction = ?}
#' }
"ref_physics"

#' Reserve nitrogen.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Reserve weight in mg N.}
#' }
"ref_resn"

#' Structural nitrogen.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Structural weight in mg N.}
#' }
"ref_structn"

#' Volume and dz.
#'
#' @format
#' \describe{
#' \item{variable}{Name of the physical variable: "volume" and "dz".}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' volume in m^3 dz in m}
#' }
"ref_vol_dz"

#' Volume.
#'
#' @format
#' \describe{
#' \item{variable}{Name of the physical variable: "volume".}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Volume in m^3}
#' }
"ref_vol"

#' agemat.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{age_mat}{First mature age class.}
#' }
"ref_agemat"

#' Consumed biomass.
#'
#' @format
#' \describe{
#' \item{pred}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{prey}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Consumed biomass in tonnes.}
#' }
"ref_bio_cons"

#' Spatial biomass.
#'
#' @format
#' \describe{
#' \item{species}{Name of the functional groups given as character string.
#' The names match with the column 'LongName' in the functionalGroups.csv file.}
#' \item{agecl}{Ageclass given as integer from 1 to NumCohorts.}
#' \item{polygon}{Boxid starting from 0 to numboxes - 1.}
#' \item{layer}{Layerid starting from 0 to numlayers - 1.}
#' \item{time}{Simulation time in years. Modeltimestep was converted to actual
#' time based on the settings in the 'run.prm' file.}
#' \item{atoutput}{Obseravtion column storing the actual output value.
#' Biomass in tonnes.}
#' }
"ref_bio_sp"



#' fishbase_data
#'
#' A table of all the the species found in Fihsbase, including taxonomic classification.
#'
#' @format A data frame with 33104 rows and 12 variables:
#' \describe{
#'   \item{\code{SpecCode}}{integer. Species code.}
#'   \item{\code{Genus}}{character.}
#'   \item{\code{Species}}{character.}
#'   \item{\code{SpeciesRefNo}}{integer. Reference number.}
#'   \item{\code{FBname}}{character. Fishbase name.}
#'   \item{\code{SubFamily}}{character.}
#'   \item{\code{FamCode}}{integer. Family Code.}
#'   \item{\code{GenCode}}{integer. Genetic Code.}
#'   \item{\code{SubGenCode}}{integer. Sub Genetic Code.}
#'   \item{\code{Family}}{character.}
#'   \item{\code{Order}}{character.}
#'   \item{\code{Class}}{character.}
#' }
#' @source \url{http://www.fishbase.org/} rfishbase::fishbase
"fishbase_data"
