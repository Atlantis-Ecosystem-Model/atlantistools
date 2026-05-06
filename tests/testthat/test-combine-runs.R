context("combine_runs tests")

#suppressMessages(library("dplyr"))

test_list <- list(ref_nums, ref_n, ref_structn)

test <- list(test_list, test_list, test_list)
test_error <- list(test_list, test_list[1:2])

out <- combine_runs(outs = test, runs = c("run1", "run2", "run3"))

# Some simple calculations!
test1 <- out[[1]] |>
  dplyr::group_by(dplyr::across(-c(run, atoutput))) |>
  dplyr::summarise(atoutput = mean(atoutput), .groups = "drop") |>
  dplyr::left_join(
    test_list[[1]],
    by = dplyr::setdiff(colnames(out[[1]]), c("run", "atoutput"))
  )

test3 <- out[[3]] |>
  dplyr::group_by(dplyr::across(-c(run, atoutput))) |>
  dplyr::summarise(atoutput = mean(atoutput), .groups = "drop") |>
  dplyr::left_join(
    test_list[[3]],
    by = dplyr::setdiff(colnames(out[[3]]), c("run", "atoutput"))
  )

test_that("test combine_runs", {
  expect_error(
    combine_runs(test, runs = c("a", "b")),
    "Number of outs and runs does not match."
  )
  expect_equal(sapply(out, dim)[1, ], sapply(test_list, dim)[1, ] * 3)
  expect_true(all(test1$atoutput.x == test1$atoutput.y))
  expect_true(all(test3$atoutput.x == test3$atoutput.y))
  expect_error(
    combine_runs(outs = test_error, runs = c("a", "b")),
    "Different number of dataframes"
  )
})
