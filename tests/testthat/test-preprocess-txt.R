context("preprocess_txt check structure and output")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

bboxes <- get_boundary(boxinfo = load_box(file.path(d, bgm = "VMPA_setas.bgm")))

df <- data.frame(code = letters[1:10], age1 = 1:10, age2 = 11:20, stringsAsFactors = FALSE)

test_that("Error handling", {
  expect_error(preprocess_txt(df, into = "code"), "Multiple ageclass columns")
})

df1 <- data.frame(code = rep(letters[1:20], times = 5), age = rep(1:10, times = 10),
                 time = runif(n = 100, min = 1, max = 2), atoutput = runif(n = 100), stringsAsFactors = FALSE)
df2 <- df1
df2$time[25:50] <- 0

df3 <- df2
df3$atoutput[25:50] <- 0

test_that("Zero handling", {
  expect_equal(dim(preprocess_txt(df1, into = "code")), c(100, 4))
  expect_equal(dim(preprocess_txt(df2, into = "code")), c(100, 4))
  expect_equal(dim(preprocess_txt(df3, into = "code")), c(74, 4))
})


