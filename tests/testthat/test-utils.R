context("test utility functions")

file_ending("C:/Users/ryan.morse/Documents/GitHub/atneus_RM/RMinit_2.nc")
file_ending("C:/Users/ryanmorse/Documents/GitHub/atneus_RM/RMinit_2.nc")
file_ending("C:/Users/ryanmorse/Documents/GitHub/atneus_RM/RMinit_2.csv", ending = "csv")

test_that("test different file endings", {
  expect_error(file_ending("C:/Users/ryanmorse/Documents/GitHub/atneus_RM/RMinit_2.csv"), "does not end in nc")
})

# Single split
r1 <- multisplit(df = ref_nums, groups = "species")
# Double split
r2 <- multisplit(df = ref_nums, groups = c("species", "agecl"))
# Triple split
r3 <- multisplit(df = ref_nums, groups = c("species", "agecl", "layer"))

test_that("test result of multisplit calls", {
  expect_equal(length(r1), length(unique(ref_nums$species)))
  expect_true(all(purrr::map_int(r1, ~length(unique(.$species))) == 1))
  expect_true(all(purrr::map_chr(r1, class) == "data.frame"))

  expect_equal(length(r2), length(unique(ref_nums$species)) * length(unique(ref_nums$agecl)))
  expect_true(all(purrr::map_int(r2, ~length(unique(.$species))) == 1))
  expect_true(all(purrr::map_int(r2, ~length(unique(.$agecl))) == 1))
  expect_true(all(purrr::map_chr(r2, class) == "data.frame"))

  expect_equal(length(r3), length(unique(ref_nums$species)) * length(unique(ref_nums$agecl)) * length(unique(ref_nums$layer)))
  expect_true(all(purrr::map_int(r3, ~length(unique(.$species))) == 1))
  expect_true(all(purrr::map_int(r3, ~length(unique(.$agecl))) == 1))
  expect_true(all(purrr::map_int(r3, ~length(unique(.$layer))) == 1))
  expect_true(all(purrr::map_chr(r3, class) == "data.frame"))
})
