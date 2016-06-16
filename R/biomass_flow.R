#' Calculate the overall biomass flow from group x to group y.
#'
#' Consumed biomass is visualised as Flowdiagram using the circlise package.
#' We calculate the biomass of prey j eaten by predator i per timesetp.
#' Based on the consumed nitrogen per predator, ageclass and timestep
#' (given in preprocess$eat) we calculate the consumed biomass of prey j by predator i
#' for all predator ageclasses and timesteps as:
#' consumed_nitrogen * predator_numbers * stomach_contribution_prey * weight_conversion.
#' The overall consumed biomass of prey j from predator i is simply the sum over all
#'
#' @param preprocess List of the preprocessed Atlantis simulation generated
#' with preprocess().
#'
#' @return ggplot.

#' @keywords gen
#' @export

biomass_flow <- function() {

}

