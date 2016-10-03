context("plots")

# plot-consumed-biomass.R -------------------------------------------------------------------------
df1 <- expand.grid(pred = c("sp1", "sp2"), agecl = 1:3, polygon = 0:2,
                   time = 0:3, prey = c("sp1", "sp2"), stringsAsFactors = FALSE)
df1$atoutput <- runif(n = nrow(df1), min = 0, max = 1)

# plot_consumed_biomass(df1, select_time = 1, show = 0.95)


# plot-diet-bec-dev.R -----------------------------------------------------------------------------
plots <- plot_diet_bec_dev(preprocess_setas$diet_specmort, wrap_col = "agecl")

# plot-species.R ----------------------------------------------------------------------------------
plot <- plot_species(preprocess_setas, species = "Shallow piscivorous fish")


