context("combine_groups tests")

wuwu <- combine_groups(preprocess_setas$diet_specmort, group_col = "pred", combine_thresh = 2)
wawa <- wuwu %>%
  dplyr::group_by(prey, agecl) %>%
  dplyr::summarise(count = dplyr::n_distinct(pred))

# no grouping variable!
wewe <- combine_groups(preprocess_setas$biomass, group_col = "species", combine_thresh = 2)

test_that("test combine_groups", {
  expect_error(combine_groups(preprocess_setas$diet_dietcheck,
                              group_col = "prey",
                              groups = c("pred", "habitat"),
                              combine_thresh = 0),
               "Minimum value for")
  expect_equal(combine_groups(preprocess_setas$diet_dietcheck,
                              group_col = "pred",
                              groups = c("prey", "habitat"),
                              combine_thresh = 20),
               preprocess_setas$diet_dietcheck)
  expect_true(all(wawa$count <= 2))
  expect_equal(length(unique(wewe$species)), 2)
})

# test_that("test combine_groups", {
#   expect_equal(length(unique(combine_groups(diet, group_col = "prey", groups = c("pred", "habitat"), combine_thresh = 1)$pred)), 10)
# })




