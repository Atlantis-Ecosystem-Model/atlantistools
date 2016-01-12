context("agg_mean and agg_sum are tested.")

data("ref_nums.rda")

test_that("test agg_mean with reference data for numbers", {
  expect_equal(dim(agg_mean(data = ref_nums, groups = c("agecl")))[1], 10)
})
