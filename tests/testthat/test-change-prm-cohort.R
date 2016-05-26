context("change_prm_cohort test parameter update.")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

prm_old <- readLines(file.path(d, "VMPA_setas_biol_fishing_New.prm"))

prm_new <- change_prm_cohort(dir = d,
                      prm_biol = "VMPA_setas_biol_fishing_New.prm",
                      select_acronyms = c("FPS", "FVS"),
                      roc = matrix(rep(2, times = 20), nrow = 2, ncol = 10),
                      parameter = "C",
                      save_to_disc = FALSE)

prm_new2 <- change_prm_cohort(dir = d,
                             prm_biol = "VMPA_setas_biol_fishing_New.prm",
                             select_acronyms = c("FPS", "FVS"),
                             roc = list(rep(2, times = 10), rep(2, times = 10)),
                             parameter = "C",
                             save_to_disc = FALSE)

pos <- c(scan_prm(chars = prm_old, variable = "C_FPS"),
         scan_prm(chars = prm_old, variable = "C_FVS"))

test_that("test change_prm_cohort", {
  expect_equal(sum(prm_old != prm_new), 2)

  expect_equal(str_split_twice(char = prm_old[pos[1] + 1], min_only = FALSE) * 2,
               str_split_twice(char = prm_new[pos[1] + 1], min_only = FALSE))

  expect_equal(str_split_twice(char = prm_old[pos[1] + 1], min_only = FALSE) * 2,
               str_split_twice(char = prm_new2[pos[1] + 1], min_only = FALSE))

  expect_error(change_prm_cohort(dir = d,
                                 prm_biol = "VMPA_setas_biol_fishing_New.prm",
                                 select_acronyms = c("FPS", "FVS"),
                                 roc = matrix(rep(2, times = 18), nrow = 2, ncol = 9),
                                 parameter = "C",
                                 save_to_disc = FALSE),
               "10 values found but only 9 new values supplied.")
})








