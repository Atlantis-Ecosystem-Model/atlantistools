context("load_nc check structure and values in output dataframe")

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

nc1 <- file.path(d, "outputSETAS.nc")
nc2 <- file.path(d, "outputSETASPROD.nc")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
bps <- load_bps(fgs = fgs, init = init)

bboxes <- get_boundary(boxinfo = load_box(bgm = file.path(d, "VMPA_setas.bgm")))

# d2 <- system.file("data", package = "atlantistools")
#
# load(file.path(d2, "ref_eat.rda"))

# load(c("ref_eat.rda", "ref_grazing.rda", "ref_n.rda", "ref_nums.rda"))

# Test numbers!
data <- load_nc(nc = nc1, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes, report = FALSE,
                select_groups = c("Planktiv_S_Fish", "Pisciv_S_Fish"),
                select_variable = "Nums")

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
data <- load_nc(nc = nc1, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes, report = FALSE,
                select_groups = c("Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det"),
                select_variable = "N")

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
data <- load_nc(nc = nc2, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes, report = FALSE,
                select_groups = c("Cephalopod", "Megazoobenthos", "Diatom", "Lab_Det", "Ref_Det"),
                select_variable = "Grazing")

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
data <- load_nc(nc = nc2, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes, report = FALSE,
                select_groups =  c("Planktiv_S_Fish", "Pisciv_S_Fish"),
                select_variable = "Eat")

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


data <- load_nc_physics(nc = nc1, bboxes = bboxes, prm_run = prm_run,
                        select_physics = c("hdsource", "hdsink", "eflux", "vflux"))

# add some antarctic debugging
# dir <- "c:/Users/alexanderke/Dropbox/Antarctic Atlantis/"
# bgm <- "Antarctica_28.bgm"
# fgs <- "AntarcticGroups.csv"
# init <- "input.nc"
# nc <- "output2/output.nc"
# prm_run <- "SO28_run.prm"
#
# select_variable <- "N"
# bboxes <- get_boundary(boxinfo = load_box(dir = dir, bgm = bgm))
# bps <- load_bps(dir = dir, fgs = fgs, init = init)
# groups <- get_groups(dir, fgs)
# groups_age <- get_age_groups(dir, fgs)
# select_groups <- groups[!groups %in% groups_age]
# check_acronyms <- TRUE
# warn_zeros <- FALSE
# report <- TRUE
#
# df <- load_nc(dir, nc, fgs, bps, select_groups, select_variable, prm_run, bboxes)
#
# at_out <- RNetCDF::open.nc(con = file.path(dir, nc))
# at_in <- RNetCDF::open.nc(con = file.path(dir, init))
# at_data <- vector(mode = "list", length = length(select_groups))
# at_init <- at_data
# for (i in seq_along(at_data)) {
#   at_data[[i]] <- RNetCDF::var.get.nc(ncfile = at_out, variable = paste0(select_groups, "_N")[i])
#   at_init[[i]] <- RNetCDF::var.get.nc(ncfile = at_in, variable = paste0(select_groups, "_N")[i])
# }

# get_epi_array_dim <- function(nc, groups) {
#   nc_read <- RNetCDF::open.nc(con = nc)
#   variables <- paste0(groups, "_N")
#   ncs <- lapply(variables, RNetCDF::var.get.nc, ncfile = nc_read)
#   ids <- sapply(ncs, function(x) length(dim(x)))
#   groups[ids == min(ids)]
# }
#
#
# get_epi_array_attr <- function(nc, groups) {
#   nc_read <- RNetCDF::open.nc(con = nc)
#   variables <- paste0(groups, "_N")
#   ids <- lapply(variables, RNetCDF::var.inq.nc, ncfile = nc_read)
#   ids <- sapply(ids, function(x) x$ndims)
#   groups[ids == min(ids)]
# }
#
# # Get epibenthic groups for Antarctic model
# dir <- "c:/Users/alexanderke/Dropbox/Antarctic Atlantis"
# groups <- get_groups(dir, fgs = "AntarcticGroups.csv")
#
# get_epi_array_dim(file.path(dir, "output2/output.nc"), groups)
# get_epi_array_dim(file.path(dir, "input.nc"), groups)
#
# get_epi_array_attr(file.path(dir, "output2/output.nc"), groups)
# get_epi_array_attr(file.path(dir, "input.nc"), groups)
#
# load_bps(dir, fgs = "AntarcticGroups.csv", init = "input.nc")
#
# # Get epibenthic groups for GNS model. This will not work on your machine!
# dir <- "z:/Atlantis_models/baserun"
# groups <- get_groups(dir, fgs = "functionalGroups.csv")
# get_epi_array_dim(file.path(dir, "outputNorthSea.nc"), groups)
# get_epi_array_dim(file.path(dir, "init_NorthSea.nc"), groups)
#
# get_epi_array_attr(file.path(dir, "outputNorthSea.nc"), groups)
# get_epi_array_attr(file.path(dir, "init_NorthSea.nc"), groups)
#
# load_bps(dir, fgs = "functionalGroups.csv", init = "init_NorthSea.nc")










