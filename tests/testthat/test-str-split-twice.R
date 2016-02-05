context("str_split_twice")

test_that("test str_split_twice", {
  expect_equal(str_split_twice(char = "test    20", min_only = TRUE), 20)
  expect_equal(str_split_twice(char = "test\t\t\t25", min_only = TRUE), 25)
  expect_equal(str_split_twice(char = "test\t\t    15", min_only = TRUE), 15)

  expect_error(str_split_twice(char = "dadsadadsadadsad", min_only = FALSE), "Neither space nor tab present.")
  expect_error(str_split_twice(char = "da ds\tdd", min_only = FALSE), "No numeric value present.")

  expect_equal(length(str_split_twice(char = "\t4   6\t12   23\t\t", min_only = FALSE)), 4)
})

