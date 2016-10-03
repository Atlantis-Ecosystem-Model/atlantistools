context("sc_init applied to dummy dataframes")

dir <- system.file("extdata", "gns", package = "atlantistools")
fgs <- "functionalGroups.csv"
init <- "init_simple_NorthSea.nc"
prm_biol <- "NorthSea_biol_fishing.prm"
bboxes <- get_boundary(load_box(dir = dir, bgm = "NorthSea.bgm"))
mult_mum <- c(2:3)
mult_c <- c(2:3)
no_avail <- FALSE
save_to_disc <- FALSE

data1 <- sc_init(dir, init, prm_biol, fgs, bboxes, save_to_disc = FALSE)

plot_sc_init(df = data1, mult_mum, mult_c)
plot_sc_init(df = data1, mult_mum, mult_c, pred = "Cod")

data2 <- sc_init(dir, init, prm_biol, fgs, bboxes, pred = "Cod", save_to_disc = FALSE)
plot_sc_init(df = data2, mult_mum, mult_c)

