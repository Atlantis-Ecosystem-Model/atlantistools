context("plot-spatial")

df <- data.frame(time = seq(0, 5, length.out = 11))
df2 <- data.frame(time = round(seq(0, 5, length.out = 10), digits = 1))

test_that("test utility functions - select_time", {
  expect_equal(select_time(df)$time, c(0, 5))
  expect_equal(select_time(df, timesteps = 2)$time, c(0, 5))
  expect_equal(select_time(df, timesteps = 3)$time, c(0, 2.5, 5))
  expect_equal(select_time(df, timesteps = 4)$time, c(0, 1.5, 3, 5))
  expect_equal(select_time(df2)$time, c(0, 5))
  expect_equal(select_time(df2, timesteps = 2)$time, c(0, 5))
  expect_equal(select_time(df2, timesteps = 3)$time, c(0, 2.8, 5))
  expect_equal(select_time(df2, timesteps = 4)$time, c(0, 1.7, 3.3, 5))
})


df <- expand.grid(species = c("sp1", "sp2", "sp3"), layer = 0:3, polygon = 0:3, stringsAsFactors = FALSE)

test_that("test utility functions - split_dfs", {
  expect_error(split_dfs(df, cols = c("species", "atoutput")), "Column names in df do not match")
  expect_equal(length(split_dfs(df, cols = "species")), 3)
  expect_equal(unique(sapply(split_dfs(df, cols = c("layer", "polygon")), length)), 3)
})



