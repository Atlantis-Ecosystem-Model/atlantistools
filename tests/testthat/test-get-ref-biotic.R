context("test reference extraction from BIOTIC.")

# Let's test 5 different function calls
# BIOTIC seems to bit quite unstable. Tests may fail due to connection timeouts.
# try to add xml2::read_html(curl::curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
# df1 <- get_ref_biotic("Cancer pagurus")

# Use hard copy of biotic html content to simplify testing.
df1 <- get_ref_biotic("Cancer pagurus", test = TRUE)
# df3 <- get_ref_biotic(c("Cancer pagurus", "Carcinus maenas"))
# df4 <- get_ref_biotic(c("Cancer pagurus", "xxx yyy"))
# df5 <- get_ref_biotic("xxx yyy")

cats <- c("Taxonomy", "Biology", "Distribution", "Reproduction", "Feeding", "Growth")

test_that("Test output of dataframes", {
  expect_equal(nrow(df1), sum(c(2, 11, 3, 7, 5, 4)))
  expect_equal(ncol(df1), 4)
  expect_true(all(df1$cat %in% cats))
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
  # expect_equal(dim(df5), c(1, 3))
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


d1 <- add_ref_url(refs = "ax 2001", ref_urls = "www.this.is.a.ref.ax+2001")
d2 <- add_ref_url(refs = c("ax 2001", "ay 2003"), ref_urls = c("www.this.is.a.ref.ax+2001", "www.another.url.ay+2003"))
d3 <- add_ref_url(refs = "ax et al. 2001", ref_urls = "www.this.is.a.ref.ax+2001")
d4 <- add_ref_url(refs = "ax     et al. 2001", ref_urls = "www.this.is.a.ref.ax+2001")
d5 <- add_ref_url(refs = c("ax 2001", "ay 2003"), ref_urls = c("www.this.is.a.ref.ay+2001", "www.another.url.ay+2004"))

test_that("Test add_ref_url helper function", {
  expect_equal(d1$ref, "ax 2001")
  expect_equal(d1$ref_url, "www.this.is.a.ref.ax+2001")
  expect_equal(dim(d2), c(2, 2))
  expect_equal(d3$ref_url, "www.this.is.a.ref.ax+2001")
  expect_equal(d4$ref_url, "www.this.is.a.ref.ax+2001")
  expect_true(all(is.na(d5$ref_url)))
})
