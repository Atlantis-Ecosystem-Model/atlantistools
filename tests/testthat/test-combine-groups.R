context("combine_groups tests")

wuwu <- combine_groups(ref_dm, group_col = "pred", combine_thresh = 2)
wawa <- wuwu %>%
  dplyr::group_by(prey, agecl) %>%
  dplyr::summarise(count = dplyr::n_distinct(pred))

# no grouping variable!
wewe <- combine_groups(ref_n, group_col = "species", groups = NULL, combine_thresh = 2)

test_that("test combine_groups", {
  expect_error(combine_groups(ref_dm,
                              group_col = "prey",
                              groups = c("pred", "habitat"),
                              combine_thresh = 0),
               "Minimum value for")
  expect_equal(combine_groups(ref_dm,
                              group_col = "pred",
                              groups = c("prey", "agecl"),
                              combine_thresh = 20),
               ref_dm)
  expect_true(all(wawa$count <= 2))
  expect_equal(length(unique(wewe$species)), 2)
})

# test_that("test combine_groups", {
#   expect_equal(length(unique(combine_groups(diet, group_col = "prey", groups = c("pred", "habitat"), combine_thresh = 1)$pred)), 10)
# })




