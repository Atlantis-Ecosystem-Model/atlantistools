d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

boundary_boxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))
epibenthic_groups <- load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc")
groups <- c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Megazoobenthos", "Diatom", "Zoo", "Lab_Det", "Ref_Det")

ref_eat <- load_nc(dir = d,
                   nc = "outputSETASPROD.nc",
                   bps = epibenthic_groups,
                   fgs = "SETasGroups.csv",
                   select_groups = groups,
                   select_variable = "Eat",
                   bboxes = boundary_boxes,
                   check_acronyms = TRUE)

devtools::use_data(ref_eat, pkg = "atlantistools", overwrite = TRUE)


ref_grazing <- load_nc(dir = d,
                       nc = "outputSETASPROD.nc",
                       bps = epibenthic_groups,
                       fgs = "SETasGroups.csv",
                       select_groups = groups,
                       select_variable = "Grazing",
                       bboxes = boundary_boxes,
                       check_acronyms = TRUE)

devtools::use_data(ref_grazing, pkg = "atlantistools", overwrite = TRUE)


ref_n <- load_nc(dir = d,
                 nc = "outputSETAS.nc",
                 bps = epibenthic_groups,
                 fgs = "SETasGroups.csv",
                 select_groups = groups,
                 select_variable = "N",
                 bboxes = boundary_boxes,
                 check_acronyms = TRUE)

devtools::use_data(ref_n, pkg = "atlantistools", overwrite = TRUE)


ref_nums <- load_nc(dir = d,
                    nc = "outputSETAS.nc",
                    bps = epibenthic_groups,
                    fgs = "SETasGroups.csv",
                    select_groups = groups,
                    select_variable = "Nums",
                    bboxes = boundary_boxes,
                    check_acronyms = TRUE)

devtools::use_data(ref_nums, pkg = "atlantistools", overwrite = TRUE)


preprocess_setas <- preprocess(dir = d,
   nc_gen = "outputSETAS.nc",
   nc_prod = "outputSETASPROD.nc",
   dietcheck = "outputSETASDietCheck.txt",
   yoy = "outputSETASYOY.txt",
   ssb = "outputSETASSSB.txt",
   specmort = "outputSETASSpecificPredMort.txt",
   prm_biol = "VMPA_setas_biol_fishing_New.prm",
   prm_run = "VMPA_setas_run_fishing_F_New.prm",
   bps = epibenthic_groups,
   fgs = "SETasGroups.csv",
   select_groups = groups,
   bboxes = boundary_boxes,
   check_acronyms = TRUE,
   modelstart = "1991-01-01",
   out = "preprocess.Rda",
   report = TRUE,
   save_to_disc = FALSE)

devtools::use_data(preprocess_setas, pkg = "atlantistools", overwrite = TRUE)

rm(list = ls())

