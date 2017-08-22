context("plots-visual")

# Add plots for visual testing here
p1 <- plot_line(preprocess$biomass)
p2 <- function() plot_consumed_biomass(ref_bio_cons)

# General roadmap from INDperform: How to implement a visual test
# 1. Add new refernce with (svg-file is created in tests/ffigs/subfolder)
#    vdiffr::validate_cases()
#
# 2. Check tests with
#    vdiffr::validate_cases()
#    vdiffr::validate_cases(cases = vdiffr::collect_cases(filter = "plots-visual"))
#       N = New visual case
#       X = Failed doppelganger
#       o = Convincing doppelganger
#
# 3. Use the shiny app to identify problems with
#    vdiffr::manage_cases(filter = "plots-visual")
#       Toggle: Left-klick to switc between new & old version
#       Slide: Left-klick + move to identify specific differences
#       Diff: Black = match, white = no match

test_that("check visually", {
  vdiffr::expect_doppelganger("line plot preprocess$biomass", p1)
  vdiffr::expect_doppelganger("plot consumed biomass ref_bio_cons", p2)
})
