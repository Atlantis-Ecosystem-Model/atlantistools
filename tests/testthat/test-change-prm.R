# context("change_prm test parameter update.")
#
# d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#
# prm_old <- readLines(file.path(d, "VMPA_setas_biol_fishing_New.prm"))
#
# prm_new <- change_prm(dir = d,
#                       prm_biol = "VMPA_setas_biol_fishing_New.prm",
#                       select_acronyms = c("FPS", "FVS"),
#                       roc = c(2,3),
#                       parameter = "KWRR",
#                       save_to_disc = FALSE)
#
# test_that("test change_prm", {
#   expect_equal(extract_prm(chars = prm_old, variable = paste("KWRR", "FPS", sep = "_")) * 2,
#                extract_prm(chars = prm_new, variable = paste("KWRR", "FPS", sep = "_")))
#
#   expect_equal(extract_prm(chars = prm_old, variable = paste("KWRR", "FVS", sep = "_")) * 3,
#                extract_prm(chars = prm_new, variable = paste("KWRR", "FVS", sep = "_")))
#
#   expect_equal(sum(prm_old != prm_new), 2)
#
#   expect_equal(which(prm_old != prm_new), c(
#     scan_prm(chars = prm_old, variable = paste("KWRR", "FPS", sep = "_")),
#     scan_prm(chars = prm_old, variable = paste("KWRR", "FVS", sep = "_"))))
# })
#
# supported_parameters <-   c("KWSR", "KWRR", "jack_a", "jack_b", "KUP", "KLP", "Recruit_Period",
#                             "BHbeta", "BHalpha", "jmL", "jmQ", "FSP", "KSPA", "E", "EPlant")
#
# # Different ordering
# # "Recruit_Time", "spawn_period", "Time_Spawn"
#
# # Found multiple times!
# # "mL", "mQ"
#
# test_that("test scan_prm & extract_prm", {
#   expect_error(scan_prm(chars = c("# KWRR_FVS 15"), variable = "KWRR_FVS"), "Variable KWRR_FVS always outcommented.")
#
#   expect_error(scan_prm(chars = c("# KWRR_FVS 15", "# KWRR_FVS 25"), variable = "KWRR_FVS"), "Variable KWRR_FVS always outcommented.")
#
#   expect_error(scan_prm(chars = c("KWRR_FVS 15", "KWRR_FVS 25"), variable = "KWRR_FVS"), "Variable KWRR_FVS found multiple times.")
#
#   expect_equal(extract_prm(chars = c("# KWRR_FVS 15", "KWRR_FVS 25"), variable = "KWRR_FVS"), 25)
#
#   expect_equal(extract_prm(chars = c("KWRR_FVS 15", "mL_FPS 0.01", "jmL_FPS 0.003", "KWRR_FVS 25"), variable = "mL_FPS"), 0.01)
#   expect_equal(extract_prm(chars = c("KWRR_FVS 15", "mL_FPS 0.01", "jmL_FPS 0.003", "KWRR_FVS 25"), variable = "jmL_FPS"), 0.003)
#
#   for (i in seq_along(supported_parameters)) {
#     expect_equal(length(scan_prm(chars = prm_old, variable = paste(supported_parameters[i], "FPS", sep = "_"))), 1)
#   }
# })
#
#
#
#
#
#
#
