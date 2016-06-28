#' Calculate the overall biomass flow from group x to group y.
#'
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
#' @return df

#' @keywords gen
#' @export

dir <- file.path("C:", "ATLANTIS_Stuff","Baseruns", "1262_v.15.0.0_ATLANTIS_NS")
load(file = file.path(dir, "preprocess-north-sea.rda"))
preprocess <- result
fgs <- "functionalGroups.csv"

biomass_flow <- function(dir, preprocess, fgs) {
  diet <- preprocess$diet_dietcheck

  # Only use vertebrate prey groups in last time-step
  age_acr <- get_age_acronyms(dir = dir, fgs = fgs)
  diet <- dplyr::filter(diet, is.element(prey, age_acr) & time == max(time))


}

