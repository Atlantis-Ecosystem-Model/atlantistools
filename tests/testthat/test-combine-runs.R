context("combine_runs tests")

suppressMessages(library("dplyr"))

test <- list(preprocess_setas, preprocess_setas, preprocess_setas)

out <- combine_runs(outs = test, runs = c("run1", "run2", "run3"))

# Some simple calculations!
test2 <- out[[2]] %>%
  group_by(species, agecl, time) %>%
  summarise(atoutput = mean(atoutput))

test12 <- out[[12]] %>%
  group_by(species, time, agecl) %>%
  summarise(atoutput = mean(atoutput))

test_that("test combine_runs", {
  expect_error(combine_runs(test, runs = c("a", "b")), "Number of outs and runs does not match.")
  expect_equal(sapply(out, dim)[1,], sapply(preprocess_setas, dim)[1,] * 3)
  expect_equal(preprocess_setas[[2]], test2)
  expect_equal(preprocess_setas[[12]], test12)
})
