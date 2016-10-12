d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")

boundary_boxes <- get_boundary(boxinfo = load_box(dir = d, bgm = "VMPA_setas.bgm"))
epibenthic_groups <- load_bps(dir = d, fgs = "SETasGroups.csv", init = "init_vmpa_setas_25032013.nc")
groups <- c("Planktiv_S_Fish", "Pisciv_S_Fish", "Cephalopod", "Megazoobenthos", "Diatom", "Zoo", "Lab_Det", "Ref_Det")
groups_age <- groups[1:2]
groups_rest <- groups[3:length(groups)]

ref_eat <- load_nc(dir = d,
                   nc = "outputSETASPROD.nc",
                   bps = epibenthic_groups,
                   fgs = "SETasGroups.csv",
                   select_groups = groups_age,
                   select_variable = "Eat",
                   prm_run = "VMPA_setas_run_fishing_F_New.prm",
                   bboxes = boundary_boxes,
                   check_acronyms = TRUE)

devtools::use_data(ref_eat, overwrite = TRUE)


ref_grazing <- load_nc(dir = d,
                       nc = "outputSETASPROD.nc",
                       bps = epibenthic_groups,
                       fgs = "SETasGroups.csv",
                       select_groups = groups_rest,
                       select_variable = "Grazing",
                       prm_run = "VMPA_setas_run_fishing_F_New.prm",
                       bboxes = boundary_boxes,
                       check_acronyms = TRUE)

devtools::use_data(ref_grazing, overwrite = TRUE)


ref_n <- load_nc(dir = d,
                 nc = "outputSETAS.nc",
                 bps = epibenthic_groups,
                 fgs = "SETasGroups.csv",
                 select_groups = groups_rest,
                 select_variable = "N",
                 prm_run = "VMPA_setas_run_fishing_F_New.prm",
                 bboxes = boundary_boxes,
                 check_acronyms = TRUE)

devtools::use_data(ref_n, overwrite = TRUE)


ref_nums <- load_nc(dir = d,
                    nc = "outputSETAS.nc",
                    bps = epibenthic_groups,
                    fgs = "SETasGroups.csv",
                    select_groups = groups_age,
                    select_variable = "Nums",
                    prm_run = "VMPA_setas_run_fishing_F_New.prm",
                    bboxes = boundary_boxes,
                    check_acronyms = TRUE)

devtools::use_data(ref_nums, overwrite = TRUE)

ref_structn <- load_nc(dir = d,
                       nc = "outputSETAS.nc",
                       bps = epibenthic_groups,
                       fgs = "SETasGroups.csv",
                       select_groups = groups_age,
                       select_variable = "StructN",
                       prm_run = "VMPA_setas_run_fishing_F_New.prm",
                       bboxes = boundary_boxes,
                       check_acronyms = TRUE)

devtools::use_data(ref_structn, overwrite = TRUE)

ref_resn <- load_nc(dir = d,
                    nc = "outputSETAS.nc",
                    bps = epibenthic_groups,
                    fgs = "SETasGroups.csv",
                    select_groups = groups_age,
                    select_variable = "ResN",
                    prm_run = "VMPA_setas_run_fishing_F_New.prm",
                    bboxes = boundary_boxes,
                    check_acronyms = TRUE)

devtools::use_data(ref_resn, overwrite = TRUE)

preprocess_setas <- preprocess(dir = d,
   nc_gen = "outputSETAS.nc",
   nc_prod = "outputSETASPROD.nc",
   dietcheck = "outputSETASDietCheck.txt",
   yoy = "outputSETASYOY.txt",
   ssb = "outputSETASSSB.txt",
   specmort = "outputSETASSpecificMort.txt",
   specpredmort = "outputSETASSpecificPredMort.txt",
   prm_biol = "VMPA_setas_biol_fishing_New.prm",
   prm_run = "VMPA_setas_run_fishing_F_New.prm",
   bps = epibenthic_groups,
   fgs = "SETasGroups.csv",
   select_groups = groups,
   bboxes = boundary_boxes,
   check_acronyms = TRUE,
   out = "preprocess.Rda",
   report = TRUE,
   save_to_disc = FALSE)

devtools::use_data(preprocess_setas, overwrite = TRUE)

rm(list = ls())

