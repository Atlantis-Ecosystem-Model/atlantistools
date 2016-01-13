context("convert_path test equality of calls")

test_that("Test if paths are identical", {
  expect_equal(convert_path(dir = file.path("c:", "atlantis-model"), file = "functionalGroups.csv"),
               convert_path(dir = NULL, file = file.path("c:", "atlantis-model", "functionalGroups.csv")))
})
