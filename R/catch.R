setwd("c:/backup_z/Atlantis_models/Runs/dummy_02_ATLANTIS_NS/")
catch <- load_nc(nc = "outputNorthSeaCATCH.nc", fgs = "functionalGroups.csv", bps = load_bps(fgs = "functionalGroups.csv", init = "init_NorthSea.nc"), select_groups = get_groups(fgs = "functionalGroups.csv"), select_variable = "Catch", bboxes = get_boundary(load_box(bgm = "NorthSea.bgm")))

at_out <- RNetCDF::open.nc("outputNorthSeaCATCH.nc")

var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_out)$nvars - 1),
                         function(x) RNetCDF::var.inq.nc(at_out, x)$name)
#
n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
n_boxes     <- RNetCDF::dim.inq.nc(at_out, 1)$length
n_layers    <- RNetCDF::dim.inq.nc(at_out, 2)$length

cod1 <- RNetCDF::var.get.nc(ncfile = at_out, variable = "cod1_Catch")
cod2 <- RNetCDF::var.get.nc(ncfile = at_out, variable = "cod2_Catch")

lapply(catch[, 1:ncol(catch)-1], unique)




