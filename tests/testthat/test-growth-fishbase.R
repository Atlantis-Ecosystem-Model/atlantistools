context("test extraction of data from fishbase.")

library("stringr")

df1 <- get_growth_fishbase("Sprattus sprattus")

test_that("test extraction for sprat", {
  expect_equal(dim(df1), c(25, 16))
  expect_equivalent(df1$linf[5], 13.4)
  expect_equivalent(df1$k[12],  0.298)
  expect_equivalent(df1$locality[23], "Kattegat")
})

df2 <- get_growth_fishbase("Anguilla anguilla")

test_that("test extraction for eel", {
  expect_equal(dim(df2), c(43, 16))
  expect_equivalent(df2$linf[10], 50.1)
  expect_equivalent(df2$k[19],  0.078)
  expect_equivalent(df2$locality[4], "Monaci lagoon")
})


df3 <- get_ref_fishbase(df1)

test_that("test extraction of references for sprat", {
  expect_true(str_detect(df3$ref[df3$ref_id == 312], "A preliminary compilation of fish length growth parameters."))
  expect_true(str_detect(df3$ref[df3$ref_id == 56764], "Some Biological characteristics of sprat"))
  expect_true(str_detect(df3$ref[df3$ref_id == 1771], "Growth and age composition of sprat stock"))
})

