context("agg_data is tested.")

# data("ref_nums.rda")

test_that("test agg_data with reference data for numbers", {
  expect_is(agg_data(data = ref_nums, groups = c("species", "agecl"), fun = sum), "data.frame")
  expect_equal(dim(agg_data(data = ref_nums, groups = c("agecl"), fun = mean))[1], 10)
  expect_equal(dim(agg_data(data = ref_nums, groups = c("agecl"), fun = sum))[1], 10)
  expect_equal(agg_data(data = ref_nums, col = "agecl", groups = "agecl", fun = mean)$atoutput, 1:10)
})


# add commented line
