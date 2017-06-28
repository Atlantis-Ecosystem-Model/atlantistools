context("test extraction of diet references from fishbase.")

#' fish <- c("Gadus morhua", "Merlangius merlangus", "Maurolicus muelleri")
#' diet <- get_diet_fishbase(fish)
#'
#' fish <- c("Gadus morhua", "Merlangius merlangus", "Ammodytes marinus")

# diet information available for all species
df1 <- get_diet_fishbase(c("Gadus morhua", "Merlangius merlangus", "Maurolicus muelleri"))

# No diet information available for "Ammodytes marinus"
df2 <- get_diet_fishbase(c("Gadus morhua", "Merlangius merlangus", "Ammodytes marinus"))

# No diet for any species. NOT WORKING!
# get_diet_fishbase("Ammodytes marinus")

test_that("test extraction for different inputs", {
  expect_equal(dim(df1), c(49, 4))
  expect_equal(sum(is.na(df2$ref_id)), 1)
  # Not working
  # expect_warning(get_diet_fishbase(fish = "xxx yyy"), " The following species are not part of the fishbase dataframe")
})





