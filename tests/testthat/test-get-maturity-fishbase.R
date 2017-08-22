context("Extract maturity parameters from fishbase")

# Test different function calls (Some are only here to run code)
# Single species call
df1 <- get_maturity_fishbase("Callionymus lyra")

# Multiple species call
df2 <- get_maturity_fishbase(c("Callionymus lyra", "Squalus acanthias"))

# Multiple species call not available
df3 <- get_maturity_fishbase(c("Callionymus lyra", "Callionymus maculatus"))

test_that("Test values for Callionymus lyra", {
  # Some of these are really specific
  expect_true(all(dim(df1) == c(1, 13)))
  expect_equal(df1$lm, 17.4)
  expect_equal(df1$species, "Callionymus lyra")
  expect_equal(df1$main_ref, 796)
})

test_that("Test values for Squalus acanthias", {
  # Some of these are really specific.
  # Remember to add one index due to Callionymus lyra having 1 row.
  expect_true(all(dim(df2) == c(22, 13)))
  expect_equal(df2$main_ref[8], 6871)
  expect_equal(df2$length_ref[15], 9725)
  expect_true(is.na(df2$lm[5]))
  expect_equal(df2$lm[9], 58)
})

test_that("Test ERROR/WARNING handling", {
  expect_true(all.equal(df1, df3))
  expect_warning(get_maturity_fishbase(c("Callionymus lyra", "Callionymus maculatus")),
                 "No maturity information available for 1 species")
  expect_error(get_maturity_fishbase("Callionymus maculatus"),
               "None of the species have information about maturity")
})

