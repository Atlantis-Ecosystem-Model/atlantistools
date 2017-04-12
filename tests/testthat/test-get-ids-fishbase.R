context("test id extraction from fishbase.")

test_that("split fish name in genus and species part", {
  expect_equal(split_species("xxx yyy"), list(ge = "xxx", sp = "yyy"))
  expect_equal(split_species("xxx yyy zzz"), list(ge = "xxx", sp = "yyy zzz"))
})

test1 <- suppressWarnings(get_ids_fishbase(c("Gadus morhua", "xxx yyy")))

test_that("test fishbase id extraction", {
  expect_equivalent(get_ids_fishbase("Gadus morhua"), 69)
  expect_equivalent(get_ids_fishbase(c("Gadus morhua", "Clupea harengus")), c(69, 24), label = NULL)
  expect_error(get_ids_fishbase("Gadus"), "Fishnames not complete.")
  expect_warning(get_ids_fishbase(c("Gadus morhua", "xxx yyy")), "The following species are not part of the fishbase dataframe")
  expect_equivalent(test1, 69)
})

