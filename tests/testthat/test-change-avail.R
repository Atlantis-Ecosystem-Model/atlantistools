context("change_avail test parameter update.")

# check_df <- function(dm, values) {
#   dm[apply(apply(dm[, 5:ncol(dm)], MARGIN = 2, function(x) is.element(x, values)), MARGIN = 1, any), ]
# }

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
dm <- load_dietmatrix(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
                      fgs = file.path(d, "SETasGroupsDem_NoCep.csv"))

test_val <- 0.1234

dm1 <- change_avail(dietmatrix = dm, pred_stanza = 1, prey = "CEP", roc = test_val, relative = F)
dm2 <- change_avail(dietmatrix = dm, prey = "BML",roc = test_val, relative = F)
dm3 <- change_avail(dietmatrix = dm, pred = "FPS", prey = "FVS", roc = test_val, relative = F)
dm4 <- change_avail(dietmatrix = dm, pred = c("FVS", "FPS"), prey = "CEP", roc = test_val, relative = F)
dm5 <- change_avail(dietmatrix = dm, pred = "FPS", roc = test_val, relative = F)
dm6 <- change_avail(dietmatrix = dm, pred = c("FPS", "FVS"), roc = test_val,relative = F)

dm1 <- dplyr::arrange(dm1, pred_stanza)

ac <- 72 # switch from pred_stanza == 1 to pred_stanza == 2


test_that("test input setting", {
  # Test predator NULL pred_stanza single value
  expect_true(all(dm1[dm1$pred_stanza == 1, "CEP"] == test_val))
  expect_true(all(dm1[dm1$pred_stanza == 2, "CEP"] != test_val))
  expect_true(all(dm1[, names(dm1)[names(dm1) != "CEP"]] != test_val))

  # Test predator NULL and pred_stanza NULL
  expect_true(all(dm2[, "BML"] == test_val))
  expect_true(all(dm2[, names(dm2)[names(dm2) != "BML"]] != test_val))

  # Test Only pred_stanza NULL
  expect_true(all(dm3[dm3$pred == "FPS", "FVS"] == test_val))
  expect_true(all(dm3[dm3$pred != "FPS", names(dm2)[names(dm2) != "FVS"]] != test_val))

  # Test 2 preds no pred_stanza 1 prey
  expect_true(all(dm4[is.element(dm4$pred, c("FVS", "FPS")), "CEP"] == test_val))
  expect_true(all(dm4[!is.element(dm4$pred, c("FVS", "FPS")), "CEP"] != test_val))

  # Test 2 preds no pred_stanza 3 prey
  expect_true(all(dm4[is.element(dm4$pred, c("FVS", "FPS")), "CEP"] == test_val))
  expect_true(all(dm4[!is.element(dm4$pred, c("FVS", "FPS")), "CEP"] != test_val))

  # Test 1 pred no pred_stanza no prey
  expect_true(all(dm5[dm5$pred == "FPS", 5:ncol(dm5)] == test_val))
  expect_true(all(dm5[dm5$pred != "FPS", ] != test_val))

  # Test 2 pred no pred_stanza no prey
  expect_true(all(dm6[is.element(dm6$pred, c("FPS", "FVS")), 5:ncol(dm6)] == test_val))
  expect_true(all(dm6[!is.element(dm6$pred, c("FPS", "FVS")), ] != test_val))

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

  expect_error(change_avail(dietmatrix = dm,
                            pred = c("FPS", "FVS"),
                            pred_stanza = c(1, 2, 3),
                            prey = "CEP",
                            roc = test_val,
                            relative = F), "Parameters pred and pred_stanza do not match")

  expect_warning(change_avail(dietmatrix = dm,
                              pred = "FVS",
                              pred_stanza = 2,
                              prey = "FPS",
                              roc = 99999999,
                              relative = T), "availabilities were")


})












