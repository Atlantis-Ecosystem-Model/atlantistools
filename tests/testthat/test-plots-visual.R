context("plot-species")

# Add some etsting due to failures of change_theme()
p <- plot_line(preprocess$biomass)

# p <- p + ggplot2::theme(legend.position = "none")
# p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
# p <- p + ggplot2::theme(strip.text.x = ggplot2::element_blank())
# p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

test_that("check visually", {
  vdiffr::expect_doppelganger("line plot preprocess$biomass", p)
})
