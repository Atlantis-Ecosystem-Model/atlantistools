context("load_dietcheck test datastructure")

library("dplyr", warn.conflicts = FALSE)

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

diet <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", report = FALSE)

# test1 <- diet %>%
#   group_by(time, pred, habitat) %>%
#   summarise(check = sum(atoutput))

test_that("test output numbers", {
  # expect_true(all(abs(test1$check - 1) < 0.001))
  expect_equal(dim(diet), c(390, 5))
  expect_is(diet$pred, "character")
  expect_is(diet$prey, "character")
  expect_equal(diet$atoutput[diet$pred == "CEP" & diet$time == 240 & diet$habitat == "WC" & diet$prey == "FPS"], 0.001663077)
  expect_equal(diet$atoutput[diet$pred == "ZM" & diet$time == 480 & diet$habitat == "WC" & diet$prey == "DL"], 9.23446e-13)

})


diet <- load_dietcheck(system.file("extdata", "setas-model-new-trunk", package = "atlantistools"),
                       dietcheck = "outputSETASDietCheck.txt", report = FALSE, version_flag = 2)

test_that("test output numbers trunk", {
  # expect_true(all(abs(test1$check - 1) < 0.001))
  expect_equal(dim(diet), c(3722, 5))
  expect_is(diet$pred, "character")
  expect_is(diet$prey, "character")
  expect_equal(diet$atoutput[diet$pred == "FVS" & diet$time == 570 & diet$agecl == 3 & diet$prey == "FVS"], 0.1192798)
  expect_equal(diet$atoutput[diet$pred == "FPS" & diet$time == 720 & diet$agecl == 6 & diet$prey == "DR"], 0.001334405)
})
