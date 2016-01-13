d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

preprocess_setas <- preprocess(dir = d,
   nc_gen = "outputSETAS.nc",
   nc_prod = "outputSETASPROD.nc",
   prm_biol = "VMPA_setas_biol_fishing_Trunk.prm",
   prm_run = "VMPA_setas_run_fishing_F_Trunk.prm",
   bps = load_bps(dir = d, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc"),
   fgs = "functionalGroups.csv",
   select_groups = get_groups(dir = d, fgs = "functionalGroups.csv"),
   bboxes = get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm")),
   check_acronyms = TRUE,
   modelstart = "1991-01-01",
   out = "preprocess.Rda",
   report = TRUE,
   save_to_disc = FALSE)



devtools::use_data(preprocess_setas, pkg = "atlantistools")

