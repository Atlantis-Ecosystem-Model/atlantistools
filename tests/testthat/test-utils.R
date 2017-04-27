context("test utility functions")

file_ending("C:/Users/ryan.morse/Documents/GitHub/atneus_RM/RMinit_2.nc")
file_ending("C:/Users/ryanmorse/Documents/GitHub/atneus_RM/RMinit_2.nc")
file_ending("C:/Users/ryanmorse/Documents/GitHub/atneus_RM/RMinit_2.csv", ending = "csv")

test_that("test different file endings", {
  expect_error(file_ending("C:/Users/ryanmorse/Documents/GitHub/atneus_RM/RMinit_2.csv"), "does not end in nc")
})


