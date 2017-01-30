context("load_fgs with setas sample files")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
file <- "SETasGroupsDem_NoCep.csv"

test_that("test format of species names", {
  expect_is(load_fgs(dir = d, fgs = file)$Name, "character")
  expect_is(load_fgs(dir = d, fgs = file)$Code, "character")
})

test_that("test file dimensions", {
  expect_equal(dim(load_fgs(dir = d, fgs = file))[1], 8)
  expect_equal(dim(load_fgs(dir = d, fgs = file))[2], 25)
})


test_that("test acronym extractions", {
  expect_equal(get_nonage_acronyms(d, file), c("CEP", "BML", "PL", "DL", "DR", "DC"))
  expect_equal(get_fish_acronyms(d, file), c("FPS", "FVS"))
})


