context("load-rec test output structure")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

test <- load_rec(yoy = file.path(d, "outputSETASYOY.txt"),
                 ssb = file.path(d, "outputSETASSSB.txt"),
                 prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"))

ssb_raw <- read.csv(file.path(d, "outputSETASSSB.txt"), sep = " ")
ssb_raw <- sum(sapply(ssb_raw[, 2:3], function(x) sum(unique(x))))

test_that("test output numbers", {
  expect_equal(dim(test), c(12, 4))
  expect_equal(names(test), c("species", "time", "ssb", "rec"))
  expect_true(abs(sum(test$ssb)/ssb_raw - 1) < 0.001)
})
