context("combine_runs tests")

suppressMessages(library("dplyr"))

test <- list(preprocess_setas, preprocess_setas, preprocess_setas)
test_error <- list(preprocess_setas, preprocess_setas[1:13])

out <- combine_runs(outs = test, runs = c("run1", "run2", "run3"))

# Some simple calculations!
test2 <- out[[2]] %>%
  group_by(species, agecl, time) %>%
  summarise(atoutput = mean(atoutput))

test12 <- out[[10]] %>%
  group_by(species, time, agecl) %>%
  summarise(atoutput = mean(atoutput))

test_that("test combine_runs", {
  expect_error(combine_runs(test, runs = c("a", "b")), "Number of outs and runs does not match.")
  expect_equal(sapply(out, dim)[1,], sapply(preprocess_setas, dim)[1,] * 3)
  expect_equal(preprocess_setas[[2]], test2)
  expect_equal(preprocess_setas[[10]], test12)
  expect_error(combine_runs(outs = test_error, runs = c("a", "b")), "Different number of dataframes")
})
