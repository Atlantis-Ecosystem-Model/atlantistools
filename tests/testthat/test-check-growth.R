context("check_growth is tested.")

df <- expand.grid(c("sp1", "sp2"), c(0:2), (1:2), stringsAsFactors = FALSE)
names(df) <- c("species", "time", "agecl")
df$atoutput <- 1

df1 <- check_growth(df)
df2 <- check_growth(df, yearly = TRUE)

test_that("test output of check_growth with dummy data", {
  expect_equal(dim(df1), c(4, 3))
  expect_equal(dim(df2), c(12, 4))
  expect_equal(unique(df1$relchange), 0)
  expect_equal(unique(df2$relchange), 0)
  expect_equal(sapply(df1, class), c(species = "character", agecl = "numeric", relchange = "numeric"))
  expect_equal(sapply(df2, class), c(species = "character", agecl = "numeric", time = "numeric", relchange = "numeric"))
})


