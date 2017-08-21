context("test reference extraction from BIOTIC.")

# Let's test 3 different function calls
df1 <- get_ref_biotic("Cancer pagurus")
df2 <- get_ref_biotic("Carcinus maenas")

test_that("Test output of dataframes", {
  expect_equal(dim(df1), c(6, 3))
  expect_equal(purrr::map_int(df1$ref, length), c(2, 11, 3, 7, 5, 4))
  expect_equal(df1$cat, c("Taxonomy", "Biology", "Distribution", "Reproduction", "Feeding", "Growth"))

  expect_equal(dim(df2), c(8, 3))
  # NOTE: there is a spelling mistake in the refs Scott-Fordsman & Depledge, 1993 vs Scott-Fordsmand & Depledge, 1993
  # Tried to fix this with agrep (Approximate String Matching), however not that easy due to string pattern matches
  # e.g. "Crothers", 1967 & "Crothers, 1968"
  # NOTE: ref in diet text but not listed (Rangley & Thomas, 1987)
  # NOTE: ref in Parasites text but not listed (Thompson, 1985)
  expect_equal(purrr::map_int(df2$ref, length), c(4, 13, 7, 6, 2, 2, 4, 4))
  expect_equal(df2$cat, c("Taxonomy", "Biology", "Distribution", "Reproduction", "General", "Growth", "Diet", "Parasites"))

})


