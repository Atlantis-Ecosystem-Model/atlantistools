context("load_nc check structure and values in output dataframe")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
bps <- load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")
bboxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))

# d2 <- system.file("data", package = "atlantistools")
#
# load(file.path(d2, "ref_eat.rda"))

# load(c("ref_eat.rda", "ref_grazing.rda", "ref_n.rda", "ref_nums.rda"))

# Test numbers!
data <- load_nc(dir = d,
                nc = "outputSETAS.nc",
                bps = bps,
                fgs = "functionalGroups.csv",
                select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish"),
                select_variable = "Nums",
                bboxes = bboxes,
                check_acronyms = TRUE,
                report = FALSE)
#
test_that("test column names", {
  expect_equal(names(data), names(ref_nums))
})

test <- merge(data, ref_nums, all = TRUE, by = c("species", "agecl", "polygon", "layer", "time"))
test$check <- test$atoutput.x / test$atoutput.y

test_that("test output numbers", {
  expect_equal(dim(data), dim(ref_nums))
  expect_equal(sum(is.na(test$atoutput.x)) + sum(is.na(test$atoutput.y)), 0)
  expect_true(sd(test$check[!is.na(test$check)]) < 0.0000001)
})
#
# Test nitrogen!

data <- load_nc(dir = d,
                nc = "outputSETAS.nc",
                bps = bps,
                fgs = "functionalGroups.csv",
                select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Megazoobenthos", "Diatom", "Zoo", "Lab_Det", "Ref_Det"),
                select_variable = "N",
                bboxes = bboxes,
                check_acronyms = TRUE,
                report = FALSE)

test_that("test column names", {
  expect_equal(names(data), names(ref_n))
})

test <- merge(data, ref_n, all = TRUE, by = c("species", "polygon", "layer", "time"))
test$check <- test$atoutput.x / test$atoutput.y

test_that("test output nitrogen", {
  expect_equal(dim(data), dim(ref_n))
  expect_equal(sum(is.na(test$atoutput.x)) + sum(is.na(test$atoutput.y)), 0)
  expect_true(sd(test$check[!is.na(test$check)]) < 0.0000001)
})

# Test Grazing!

data <- load_nc(dir = d,
                nc = "outputSETASPROD.nc",
                bps = bps,
                fgs = "functionalGroups.csv",
                select_groups = c("Cephalopod", "Megazoobenthos", "Diatom", "Zoo", "Lab_Det", "Ref_Det"),
                select_variable = "Grazing",
                bboxes = bboxes,
                check_acronyms = TRUE,
                report = FALSE)

test_that("test column names", {
  expect_equal(names(data), names(ref_grazing))
})

test <- merge(data, ref_grazing, all = TRUE, by = c("species", "agecl", "polygon", "time"))
test$check <- test$atoutput.x / test$atoutput.y

test_that("test output nitrogen", {
  expect_equal(dim(data), dim(ref_grazing))
  expect_equal(sum(is.na(test$atoutput.x)) + sum(is.na(test$atoutput.y)), 0)
  expect_true(sd(test$check[!is.na(test$check)]) < 0.0000001)
})

# Test Eat!

data <- load_nc(dir = d,
                nc = "outputSETASPROD.nc",
                bps = bps,
                fgs = "functionalGroups.csv",
                select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish"),
                select_variable = "Eat",
                bboxes = bboxes,
                check_acronyms = TRUE,
                report = FALSE)

test_that("test column names", {
  expect_equal(names(data), names(ref_eat))
})

test <- merge(data, ref_eat, all = TRUE, by = c("species", "agecl", "polygon", "time"))
test$check <- test$atoutput.x / test$atoutput.y

test_that("test output nitrogen", {
  expect_equal(dim(data), dim(ref_eat))
  expect_equal(sum(is.na(test$atoutput.x)) + sum(is.na(test$atoutput.y)), 0)
  expect_true(sd(test$check[!is.na(test$check)]) < 0.0000001)
})




