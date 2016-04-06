context("combine_groups tests")

d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

diet <- load_dietcheck(dir = d, dietcheck = "outputSETASDietCheck.txt", report = FALSE)
diet <- subset(diet, prey == "FPS")

# test_that("test combine_groups", {
#   expect_equal(combine_groups(data = diet, group_col = "prey", groups = c("time", "pred", "habitat"), combine_thresh = 0), diet)
#   expect_is(combine_groups(data = preprocess_setas$biomass, group_col = "species", groups = "time"), "grouped_df")
#   expect_equal(nrow(combine_groups(data = preprocess_setas$biomass_age, group_col = "species", groups = c("time", "agecl"), combine_thresh = 1)), 40)
#   expect_equal(unique(combine_groups(data = preprocess_setas$biomass_age, group_col = "species", groups = c("time", "agecl"), combine_thresh = 1)$species), "Rest")
# })

test_that("test combine_groups", {
  expect_equal(length(unique(combine_groups(diet, group_col = "prey", groups = c("pred", "habitat"), combine_thresh = 1)$pred)), 10)
})




