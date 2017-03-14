context("change_prm test parameter update.")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

prm_old <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
prm_old_read <- readLines(prm_old)

prm_new <- change_prm(prm_biol = prm_old,
                      select_acronyms = c("FPS", "FVS"),
                      roc = c(2,3),
                      parameter = "KWRR",
                      save_to_disc = FALSE)

test_that("test change_prm", {
  expect_equal(extract_prm(prm_biol = prm_old, variables = paste("KWRR", "FPS", sep = "_")), 0.075)

  expect_equal(extract_prm(prm_biol = prm_old, variables = paste("KWRR", "FVS", sep = "_")), 7000)

  expect_equal(sum(prm_old_read != prm_new), 2)

  expect_equal(which(prm_old_read != prm_new), c(
    scan_prm(chars = prm_old_read, variable = paste("KWRR", "FPS", sep = "_")),
    scan_prm(chars = prm_old_read, variable = paste("KWRR", "FVS", sep = "_"))))
})

supported_parameters <-   c("KWSR", "KWRR", "FSP", "KSPA", "E", "EPlant")

# Different ordering
# "Recruit_Time", "spawn_period", "Time_Spawn"

# Found multiple times!
# "mL", "mQ"
pos <- sapply(paste(supported_parameters, "FPS", sep = "_"), scan_prm, chars = prm_old_read)

test_that("test scan_prm & extract_prm", {
  expect_error(scan_prm(chars = c("# KWRR_FVS 15"), variable = "KWRR_FVS"), "Variable KWRR_FVS always outcommented.")

  expect_error(scan_prm(chars = c("# KWRR_FVS 15", "# KWRR_FVS 25"), variable = "KWRR_FVS"), "Variable KWRR_FVS always outcommented.")

  expect_error(scan_prm(chars = c("KWRR_FVS 15", "KWRR_FVS 25"), variable = "KWRR_FVS"), "Variable KWRR_FVS found multiple times.")

  expect_equal(extract_prm(prm_biol = prm_old, variables = "KWRR_FVS"), 7000)

  expect_equal(length(pos), length(supported_parameters))

  # for (i in seq_along(supported_parameters)) {
  #   print(i)
  #   pm <- scan_prm(chars = prm_old_read, variable = paste(supported_parameters[i], "FPS", sep = "_"))
  #   print(pm)
  #   # expect_equal(length(pm), 1)
  # }
})

# create dummy FLAGs wich are nested into each other!
dummy <- c("WSK_age_mat 2", "LSK_age_mat 3", "SK_age_mat 4", "jmL_COD 0.001", "mL_COD 0.002", "mum_COD 25", "crit_mum_COD 30")
prm_dummy <- prm_old_read
prm_dummy[sample(1:length(prm_dummy), size = length(dummy), replace = F)] <- dummy

test_that("test flags embedded in other flags", {
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "WSK_age_mat")], dummy[1])
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "LSK_age_mat")], dummy[2])
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "SK_age_mat")], dummy[3])
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "jmL_COD")], dummy[4])
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "mL_COD")], dummy[5])
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "mum_COD")], dummy[6])
  expect_equal(prm_dummy[scan_prm(chars = prm_dummy, variable = "crit_mum_COD")], dummy[7])
})











