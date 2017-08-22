context("plot-species")

# Add plots for visual testing here
p1 <- plot_line(preprocess$biomass)

# General roadmap from INDperform
# How to implement test after the function is written:
# 1. Define result and test if function call is identical
# 2. Use different input objects (e.g. with NAs, without NAs, with Zeros, differemt object types (vector vs. array))
# 3. Test the object structure of the output with typeof or class
# 4. Test column classes in a tibble/dataframe of the returned object.
# 5. Test every warning() and stop() command with expect_warning(), expect_error().

# p <- p + ggplot2::theme(legend.position = "none")
# p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
# p <- p + ggplot2::theme(strip.text.x = ggplot2::element_blank())
# p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

test_that("check visually", {
  vdiffr::expect_doppelganger("line plot preprocess$biomass", p1)
  vdiffr::expect_doppelganger("plot consumed biomass ref_bio_cons", plot_consumed_biomass(ref_bio_cons))
})
