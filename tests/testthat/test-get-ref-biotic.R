context("test reference extraction from BIOTIC.")

# Let's test 5 different function calls
# BIOTIC seems to bit quite unstable. Tests may fail due to connection timeouts.
# try to add xml2::read_html(curl::curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
# df1 <- get_ref_biotic("Cancer pagurus")

# Use hard copy of biotic html content to simplify testing.
df1 <- get_ref_biotic("Cancer pagurus", test = TRUE)
# df3 <- get_ref_biotic(c("Cancer pagurus", "Carcinus maenas"))
# df4 <- get_ref_biotic(c("Cancer pagurus", "xxx yyy"))
df5 <- get_ref_biotic("xxx yyy")

test_that("Test output of dataframes", {
  expect_equal(dim(df1), c(6, 3))
  expect_equal(purrr::map_int(df1$ref, length), c(2, 11, 3, 7, 5, 4))
  expect_equal(df1$cat, c("Taxonomy", "Biology", "Distribution", "Reproduction", "Feeding", "Growth"))
  #
  # expect_equal(dim(df2), c(8, 3))
  # # NOTE: there is a spelling mistake in the refs Scott-Fordsman & Depledge, 1993 vs Scott-Fordsmand & Depledge, 1993
  # # Tried to fix this with agrep (Approximate String Matching), however not that easy due to string pattern matches
  # # e.g. "Crothers", 1967 & "Crothers, 1968"
  # # NOTE: ref in diet text but not listed (Rangley & Thomas, 1987)
  # # NOTE: ref in Parasites text but not listed (Thompson, 1985)
  # expect_equal(purrr::map_int(df2$ref, length), c(4, 13, 7, 6, 2, 2, 4, 4))
  # expect_equal(df2$cat, c("Taxonomy", "Biology", "Distribution", "Reproduction", "General", "Growth", "Diet", "Parasites"))
  #
  # expect_equal(dim(df3), c(14, 3))
  expect_equal(dim(df5), c(1, 3))
})

test <- "author, 2000"

test_that("Test refstr_to_ref helper function", {
  expect_equal(refstr_to_ref("ax, 2000, ay, 2001, az, 2002"), c("ax, 2000", "ay, 2001", "az, 2002"))
  # Add in comma
  expect_equal(refstr_to_ref("author 2000"), test)
  # Remove trailing non integers
  expect_equal(refstr_to_ref("author, 2000 strings"), test)
  # Remove leading non characters
  expect_equal(refstr_to_ref("   author, 2000"), test)
  # All together
  expect_equal(refstr_to_ref("   author 2000 strings"), test)
})
