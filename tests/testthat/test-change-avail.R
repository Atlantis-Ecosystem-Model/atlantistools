context("change_avail test parameter update.")

# check_df <- function(dm, values) {
#   dm[apply(apply(dm[, 5:ncol(dm)], MARGIN = 2, function(x) is.element(x, values)), MARGIN = 1, any), ]
# }

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

test_val <- 0.1234

dm1 <- change_avail(dir = d,
                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
                    fgs = "SETasGroups.csv",
                    pred_stanza = 1,
                    prey = "BC",
                    roc = test_val,
                    relative = F)

dm2 <- change_avail(dir = d,
                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
                    fgs = "SETasGroups.csv",
                    prey = "BC",
                    roc = test_val,
                    relative = F)

dm3 <- change_avail(dir = d,
                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
                    fgs = "SETasGroups.csv",
                    pred = "FPL",
                    prey = "BC",
                    roc = test_val,
                    relative = F)

dm4 <- change_avail(dir = d,
                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
                    fgs = "SETasGroups.csv",
                    pred = c("FPL", "FPS"),
                    prey = "BC",
                    roc = test_val,
                    relative = F)

dm5 <- change_avail(dir = d,
                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
                    fgs = "SETasGroups.csv",
                    pred = "FPL",
                    roc = test_val,
                    relative = F)

dm6 <- change_avail(dir = d,
                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
                    fgs = "SETasGroups.csv",
                    pred = c("FPL", "FPO"),
                    roc = test_val,
                    relative = F)

dm1 <- dplyr::arrange(dm1, pred_stanza)

ac <- 72 # switch from pred_stanza == 1 to pred_stanza == 2


test_that("test input setting", {
  # Test predator NULL pred_stanza single value
  expect_true(all(dm1[, "BC"][1:ac] == test_val))
  expect_true(all(dm1[, "BC"][(ac + 1):nrow(dm1)] != test_val))
  expect_true(all(dm1[, names(dm1)[names(dm1) != "BC"]] != test_val))

  # Test predator NULL and pred_stanza NULL
  expect_true(all(dm2[, "BC"] == test_val))
  expect_true(all(dm2[, names(dm2)[names(dm2) != "BC"]] != test_val))

  # Test Only pred_stanza NULL
  expect_true(all(dm3[dm3$pred == "FPL", "BC"] == test_val))
  expect_true(all(dm3[dm3$pred != "FPL", names(dm2)[names(dm2) != "BC"]] != test_val))

  # Test 2 preds no pred_stanza 1 prey
  expect_true(all(dm4[is.element(dm4$pred, c("FPL", "FPS")), "BC"] == test_val))
  expect_true(all(dm4[!is.element(dm4$pred, c("FPL", "FPS")), "BC"] != test_val))

  # Test 2 preds no pred_stanza 3 prey
  expect_true(all(dm4[is.element(dm4$pred, c("FPL", "FPS")), "BC"] == test_val))
  expect_true(all(dm4[!is.element(dm4$pred, c("FPL", "FPS")), "BC"] != test_val))

  # Test 1 pred no pred_stanza no prey
  expect_true(all(dm5[dm5$pred == "FPL", 5:ncol(dm5)] == test_val))
  expect_true(all(dm5[dm5$pred != "FPL", ] != test_val))

  # Test 2 pred no pred_stanza no prey
  expect_true(all(dm6[is.element(dm6$pred, c("FPL", "FPO")), 5:ncol(dm6)] == test_val))
  expect_true(all(dm6[!is.element(dm6$pred, c("FPL", "FPO")), ] != test_val))

})

test_that("Error handling",  {
  # expect_error(change_avail(dir = d,
  #                           prm_biol = "VMPA_setas_biol_fishing_New.prm",
  #                           fgs = "SETasGroups.csv",
  #                           pred = c("FPL", "FPS"),
  #                           prey = c("FPL", "FPS", "BC"),
  #                           roc = test_val,
  #                           relative = F,
  #                           save_to_disc = F), "Parameters roc and prey do not match")

  expect_error(change_avail(dir = d,
                            prm_biol = "VMPA_setas_biol_fishing_New.prm",
                            fgs = "SETasGroups.csv",
                            pred = c("FPL", "FPS"),
                            pred_stanza = c(1, 2, 3),
                            prey = "BC",
                            roc = test_val,
                            relative = F), "Parameters pred and pred_stanza do not match")

  expect_warning(change_avail(dir = d,
                              prm_biol = "VMPA_setas_biol_fishing_New.prm",
                              fgs = "SETasGroups.csv",
                              pred = "FVS",
                              pred_stanza = 2,
                              prey = "FVO",
                              roc = 99999999,
                              relative = T), "availabilities were")


})












