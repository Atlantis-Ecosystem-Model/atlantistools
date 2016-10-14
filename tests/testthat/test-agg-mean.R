context("agg_data is tested.")

perc <- agg_perc(data = ref_n, groups = "species", out = "test")

test_that("test agg_data with reference data for numbers", {
  expect_is(agg_data(data = ref_nums, groups = c("species", "agecl"), fun = sum), "data.frame")
  expect_equal(dim(agg_data(data = ref_nums, groups = c("agecl"), fun = mean))[1], 10)
  expect_equal(dim(agg_data(data = ref_nums, groups = c("agecl"), fun = sum))[1], 10)
  expect_equal(agg_data(data = ref_nums, col = "agecl", groups = "agecl", fun = mean)$atoutput, 1:10)
  expect_equal(names(agg_data(data = ref_nums, groups = "species", out = "test", fun = sum)), c("species", "test"))
  expect_equal(names(perc), c("polygon", "layer", "species", "time", "atoutput", "test"))
  expect_equal(agg_data(perc, col = "test", groups = "species", fun = sum)$atoutput, rep(1, times = 6))
})

test_that("test data grouping", {
  expect_is(group_data(ref_nums, groups = c("species", "agecl")), "grouped_df")
})

# add commented line
