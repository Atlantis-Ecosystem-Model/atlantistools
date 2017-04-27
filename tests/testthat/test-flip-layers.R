context("flip layers for different test datasets")

df1 <- expand.grid(polygon = 0, layer = 0:7)
df2 <- expand.grid(polygon = 1, layer = 0:4)
df3 <- expand.grid(polygon = 2, layer = 0:2)
df4 <- expand.grid(polygon = 3, layer = c(0:3, 7))

test_that("test add_fliped_layers", {
  expect_equal(add_fliped_layers(df1, 7)$layer_fliped, c(7:1, 8))
  expect_equal(add_fliped_layers(df2, 7)$layer_fliped, 5:1)
  expect_equal(add_fliped_layers(df3, 7)$layer_fliped, 3:1)
  expect_equal(add_fliped_layers(df4, 7)$layer_fliped, c(4:1, 8))
})





