context("Convert reference to bibkey using build in dataframes")

bibchr <- system.file("extdata/ref_bib.bib", package = "atlantistools")

bib_df <- bib_to_df(bib = bibchr)

test_that("Test *.bib conversion to tidy dataframe", {
  expect_equal(bib_df$bibkey, c("chang_age_1951", "gislason_does_2008", "daan_growth_1974"))
  expect_equal(bib_df$author, list("Chang", c("Gislason", "Daan", "Rice", "Pope"), "Daan"))
  expect_equal(bib_df$year, c(1951, 2008, 1974))
  expect_equal(bib_df$title, c("Age and Growth of Callionymus Lyra L.",
                               "Does Natural Mortality Depend on Individual Size",
                               "Growth of North Sea Cod, Gadus Morhua"))
})

test_that("Test bibkey assignment", {
  expect_equal(ref_to_bibkey(ref_lit, bibchr), c("gislason_does_2008", "daan_growth_1974", "chang_age_1951"))
})
