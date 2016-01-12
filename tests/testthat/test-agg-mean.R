context("agg_mean and agg_sum are tested.")

data("ref_nums.rda")

test_that("test agg_mean with reference data for numbers", {
  expect_is(agg_sum(data = ref_nums, groups = c("species", "agecl")), "data.frame")
  expect_equal(dim(agg_mean(data = ref_nums, groups = c("agecl")))[1], 10)
  expect_equal(dim(agg_sum(data = ref_nums, groups = c("agecl")))[1], 10)
  expect_equal(agg_mean(data = ref_nums, col = "agecl", groups = "agecl")$atoutput, 1:10)
})


# add commented line
