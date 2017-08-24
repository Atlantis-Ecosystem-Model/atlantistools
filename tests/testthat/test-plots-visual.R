context("plots-visual")

# Code used to create plots
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
fgs <- file.path(d, "SETasGroups.csv")
df <- load_spec_mort(specmort, prm_run, fgs, version_flag = 1)
plots <- plot_diet_bec_dev(df, wrap_col = "agecl")
plots <- plots[[4]]
ex_data <- read.csv(file.path(d, "setas-ssb-rec.csv"), stringsAsFactors = FALSE)
sp_overlap <- calculate_spatial_overlap(ref_bio_sp, ref_dietmatrix, ref_agemat)
ex_bio <- preprocess$biomass
ex_bio$atoutput <- ex_bio$atoutput * runif(n = nrow(ex_bio), 0, 1)
ex_bio$model <- "test"

dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
fgs <- file.path(dir, "SETasGroupsDem_NoCep.csv")
init <- file.path(dir, "INIT_VMPA_Jan2015.nc")
prm_biol <- file.path(dir, "VMPA_setas_biol_fishing_Trunk.prm")
bboxes <- get_boundary(load_box(bgm = file.path(dir, "VMPA_setas.bgm")))
no_avail <- FALSE
save_to_disc <- FALSE
data1 <- sc_init(init = init, prm_biol = prm_biol, fgs = fgs, bboxes = bboxes)

# Add plots for visual testing here
p1 <- plot_line(preprocess$biomass)
p2 <- function() plot_consumed_biomass(ref_bio_cons)
p3 <- function() gridExtra::grid.arrange(plots$grobs[[2]])
p4 <- function() gridExtra::grid.arrange(plots$grobs[[3]])

# Does it work with any call to gridExtra functions?
dummy <- gridExtra::arrangeGrob(grobs = list(p1, p1),  heights = grid::unit(c(0.5, 0.5), units = "npc"))
p5 <- function() gridExtra::grid.arrange(dummy)

p6 <- plot_bar(preprocess$nums_age, fill = "agecl", wrap = "species")
p7 <- plot_rec(preprocess$ssb_rec, ex_data)
p8 <- plot_spatial_overlap(sp_overlap[11])
p9 <- plot_line(preprocess$biomass[preprocess$biomass$species == "Carrion3", ])
p9 <- plot_add_range(p9, ex_bio[ex_bio$species == "Carrion3", ])
p10 <- plot_sc_init(df = data1, seq(0.5, 10, by = 1), seq(0.5, 10, by = 1))

# General roadmap from INDperform: How to implement a visual test
# 1. Add new refernce with (svg-file is created in tests/ffigs/subfolder)
#    vdiffr::validate_cases()
#
# 2. Check tests with
#    vdiffr::validate_cases()
#    vdiffr::validate_cases(cases = vdiffr::collect_cases(filter = "plots-visual"))
#       N = New visual case
#       X = Failed doppelganger
#       o = Convincing doppelganger
#
# 3. Use the shiny app to identify problems with
#    vdiffr::manage_cases(filter = "plots-visual")
#       Toggle: Left-klick to switc between new & old version
#       Slide: Left-klick + move to identify specific differences
#       Diff: Black = match, white = no match
#
# 4. Collect orphaned cases from time to time and remove reference plots
#    which aren't used anymore.
#    vdiffr::collect_orphaned_cases(cases = vdiffr::collect_cases(filter = "plots-visual"))

test_that("check visually", {
  vdiffr::expect_doppelganger("p1 plot_line(preprocess$biomass)", p1)
  vdiffr::expect_doppelganger("p2 plot_consumed_biomass(ref_bio_cons)", p2)
  # Not working due to arrangeGrob call
  # vdiffr::expect_doppelganger("plot diet bec dev outputSETASSpecificPredMort upper", p3)
  # vdiffr::expect_doppelganger("plot diet bec dev outputSETASSpecificPredMort lower", p4)
  # vdiffr::expect_doppelganger("line plot preprocess$biomass twice", p5)
  vdiffr::expect_doppelganger("p6 plot_bar(preprocess$nums_age)", p6)
  vdiffr::expect_doppelganger("p7 plot_rec(preprocess$ssb_rec, ex_data)", p7)
  vdiffr::expect_doppelganger("p8 plot_spatial_overlap(sp_overlap[11])", p8)

  # For some reason gem_rug results in rerendering...
  # vdiffr::expect_doppelganger("plot_add_range(p1, ex_bio)", p9)

  vdiffr::expect_doppelganger("p10 plot_sc_init(df = data1, mult_mum, mult_c)", p10)
})
