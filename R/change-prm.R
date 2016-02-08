# #' Change biological parameterfile to simpliyfy automated ATLANTIS calibrations.
# #'
# #'
# #' This functionis is used to help automate the calibration routine for ATLANTIS models.
# #'
# #' @param dir Character string giving the path of the Atlantis model folder.
# #' If data is stored in multiple folders (e.g. main model folder and output
# #' folder) you should use 'NULL' as dir.
# #' @param prm_biol Character string giving the filename of the biological
# #' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
# #' multiple folders for your model files and outputfiles pass the complete
# #' folder/filename string and set dir to 'NULL'.
# #' @param select_acronyms Character vector of funtional groups which shall be read in.
# #' Names have to match the ones used in the *.prm file. Check column "Code" in
# #' "functionalGroups.csv" for clarification.
# #' @param roc Vector of multiplication factors which shall be applied to the old set of parameters.
# #' Please supply one value per selected group. In case relative is FALSE the new absolute values
# #' can be passed as roc.
# #' @param parameter Character value of the model parameter which shall be changed.
# #' Only one parameter can be selected per function call.
# #' @param relative Logical if TRUE values are changed relative to base values. If FALSE new values can
# #' be passed directly.
# #' @param save_to_disc Logical indicating if the resulting prm file should be overwritten
# #' (\code{TRUE}) or not (\code{FALSE}).
# #' @return parameterfile *.prm file with the new parameter values.
# #' @export
# #'
# #' @examples
# #' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
# #' new_prm <- change_prm(dir = d,
# #'                       prm_biol = "VMPA_setas_biol_fishing_New.prm",
# #'                       select_acronyms = c("FPS", "FVS"),
# #'                       roc = c(2,3),
# #'                       parameter = "KWRR",
# #'                       save_to_disc = FALSE)
#
# change_prm <- function(dir = getwd(), prm_biol, select_acronyms, roc, parameter, relative = TRUE, save_to_disc = TRUE) {
#   if (length(parameter) != 1) stop("Please suply only one parameter per function call.")
#
#   if (length(select_acronyms) != length(roc)) {
#     stop("Length of select_acronyms and roc does not match.")
#   }
#
#   # Read in parameter file!
#   prm_biol_new <- convert_path(dir = dir, file = prm_biol)
#   prm_biol_new <- readLines(con = prm_biol_new)
#
#   # Function to update a specific parameter composed of a parameter string
#   # a group acronym and a seperator (by default "_") found in a prm file.
#   update_prm_species <- function(prm_biol, acronym, roc, parameter, relative) {
#     flag <- paste(parameter, acronym, sep = "_")
#     pos <- scan_prm(chars = prm_biol, variable = flag)
#
#     old_value <- str_split_twice(char = prm_biol[pos])
#
#     if (relative) {
#       new_value <- old_value * roc
#     } else {
#       new_value <- roc
#     }
#
#     # Update value. Some pesky expectations have to be added here.
#     if (is.element(parameter, c("mum", "c", "mQ", "mL", "jmL", "jmQ"))) {
#       prm_biol[pos] <- paste(flag, new_value, "T15", sep = "\t")
#     } else {
#       prm_biol[pos] <- paste(flag, new_value, sep = "\t")
#     }
#     return(prm_biol)
#   }
#
#   for (i in seq_along(select_acronyms)) {
#     if (!(roc[i] == 1 & relative)) {
#       prm_biol_new <- update_prm_species(prm_biol = prm_biol_new, acronym = select_acronyms[i], roc = roc[i], parameter = parameter, relative = relative)
#     }
#   }
#
#   if (save_to_disc) {
#     print("Writing new prm file!")
#     writeLines(text = prm_biol_new, con = convert_path(dir = dir, file = prm_biol), sep = "\n")
#   }
#
#   return(prm_biol_new)
# }
#
