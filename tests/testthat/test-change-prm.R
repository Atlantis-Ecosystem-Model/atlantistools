context("change_prm test parameter update.")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

prm_old <- "VMPA_setas_biol_fishing_New.prm"
prm_old_read <- readLines(file.path(d, prm_old))

prm_new <- change_prm(dir = d,
                      prm_biol = "VMPA_setas_biol_fishing_New.prm",
                      select_acronyms = c("FPS", "FVS"),
                      roc = c(2,3),
                      parameter = "KWRR",
                      save_to_disc = FALSE)

test_that("test change_prm", {
  expect_equal(extract_prm(dir = d, prm_biol = prm_old, variable = paste("KWRR", "FPS", sep = "_")), 0.075)

  expect_equal(extract_prm(dir = d, prm_biol = prm_old, variable = paste("KWRR", "FVS", sep = "_")), 7000)

  expect_equal(sum(prm_old_read != prm_new), 2)

  expect_equal(which(prm_old_read != prm_new), c(
    scan_prm(chars = prm_old_read, variable = paste("KWRR", "FPS", sep = "_")),
    scan_prm(chars = prm_old_read, variable = paste("KWRR", "FVS", sep = "_"))))
})

supported_parameters <-   c("KWSR", "KWRR", "jack_a", "jack_b", "KUP", "KLP", "Recruit_Period",
                            "BHbeta", "BHalpha", "jmL", "jmQ", "FSP", "KSPA", "E", "EPlant")

# Different ordering
# "Recruit_Time", "spawn_period", "Time_Spawn"

# Found multiple times!
# "mL", "mQ"
pos <- sapply(paste(supported_parameters, "FPS", sep = "_"), scan_prm, chars = prm_old_read)

test_that("test scan_prm & extract_prm", {
  expect_error(scan_prm(chars = c("# KWRR_FVS 15"), variable = "KWRR_FVS"), "Variable KWRR_FVS always outcommented.")

  expect_error(scan_prm(chars = c("# KWRR_FVS 15", "# KWRR_FVS 25"), variable = "KWRR_FVS"), "Variable KWRR_FVS always outcommented.")

  expect_error(scan_prm(chars = c("KWRR_FVS 15", "KWRR_FVS 25"), variable = "KWRR_FVS"), "Variable KWRR_FVS found multiple times.")

  expect_equal(extract_prm(dir = d, prm_biol = prm_old, variable = "KWRR_FVS"), 7000)

  expect_equal(extract_prm(dir = d, prm_biol = prm_old, variable = "mL_FPS"), 2e-8)
  expect_equal(extract_prm(dir = d, prm_biol = prm_old, variable = "jmL_FPS"), 1e-8)

  expect_equal(length(pos), length(supported_parameters))

  # for (i in seq_along(supported_parameters)) {
  #   print(i)
  #   pm <- scan_prm(chars = prm_old_read, variable = paste(supported_parameters[i], "FPS", sep = "_"))
  #   print(pm)
  #   # expect_equal(length(pm), 1)
  # }
})






