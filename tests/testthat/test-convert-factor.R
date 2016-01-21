context("convert_factor tests")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
file <- "functionalGroups.csv"

fgs <- load_fgs(dir = d, fgs = file)

newcol <- convert_factor(data_fgs = fgs, col = fgs$Name)
newcol2 <- convert_factor(data_fgs = fgs, col = fgs$Code)

test_that("test convert_factor", {
  expect_is(fgs$Name, "character")
  expect_is(fgs$Code, "character")
  expect_is(fgs$LongName, "character")
  expect_is(newcol, "factor")
  expect_equal(newcol, as.factor(fgs$LongName))
  expect_equal(newcol2, as.factor(fgs$LongName))
  expect_error(convert_factor(data_fgs = fgs, col = fgs$LongName), "match the entries in parameter col!")
})
