context("combine_groups tests")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

wuwu <- combine_groups(preprocess_setas$diet_specmort, group_col = "pred", combine_thresh = 1)
wawa <- wuwu %>%
  dplyr::group_by(prey, agecl) %>%
  dplyr::summarise(count = dplyr::n_distinct(pred))

# test_that("test combine_groups", {
#   expect_equal(combine_groups(data = diet, group_col = "prey", groups = c("time", "pred", "habitat"), combine_thresh = 0), diet)
#   expect_is(combine_groups(data = preprocess_setas$biomass, group_col = "species", groups = "time"), "grouped_df")
#   expect_equal(nrow(combine_groups(data = preprocess_setas$biomass_age, group_col = "species", groups = c("time", "agecl"), combine_thresh = 1)), 40)
#   expect_equal(unique(combine_groups(data = preprocess_setas$biomass_age, group_col = "species", groups = c("time", "agecl"), combine_thresh = 1)$species), "Rest")
# })

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
})




