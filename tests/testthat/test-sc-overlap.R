context("Calculation of Schoner index")

dummy <- data.frame(agecl = 1, polygon = rep(1:2, 2), layer = rep(1:2, each = 2),
                    time = 1, stringsAsFactors = FALSE, species_stanza = 1)
dummy1 <- dummy
dummy1$species <- "cod"
dummy2 <- dummy
dummy2$species <- "herring"

df_avail <- data.frame(pred = "cod", pred_stanza = 1, prey_stanza = 1, prey = "herring", avail = 1, stringsAsFactors = FALSE)

# Perfect negative overlap!
dummy1$perc_bio <- c(rep(0.5, 2), rep(0, 2))
dummy2$perc_bio <- c(rep(0, 2),   rep(0.5, 2))
df_bio1 <- rbind(dummy1, dummy2)

# Perfect positive overlap!
dummy1$perc_bio <- c(rep(0.5, 2), rep(0, 2))
dummy2$perc_bio <- c(rep(0.5, 2), rep(0, 2))
df_bio2 <- rbind(dummy1, dummy2)

# 1/2 positive overlap!
dummy1$perc_bio <- c(c(0.75, 0.25), rep(0, 2))
dummy2$perc_bio <- c(c(0.25, 0.75), rep(0, 2))
df_bio3 <- rbind(dummy1, dummy2)


test_that("test schoener calculations", {
  expect_equal(schoener(predgrp = "cod", ageclass = 1, biomass = df_bio1, avail = df_avail)[[2]]$si, 0)
  expect_equal(schoener(predgrp = "cod", ageclass = 1, biomass = df_bio2, avail = df_avail)[[2]]$si, 1)
  expect_equal(schoener(predgrp = "cod", ageclass = 1, biomass = df_bio3, avail = df_avail)[[2]]$si, 0.5)
})

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

dietmatrix <- load_dietmatrix(prm_biol, fgs, convert_names = TRUE)
agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs,
                    group = get_age_acronyms(fgs = fgs),
                    parameter = "age_mat")

sp_overlap <- calculate_spatial_overlap(biomass_spatial = ref_bio_sp,
                                        dietmatrix = dietmatrix,
                                        agemat = agemat)

# Unnest nested list to simplify test calculations
sp_list <- purrr::flatten(sp_overlap)

test_that("test spatial overlap calculations", {
  expect_equal(length(sp_overlap), 22)
  # each list entry itself is a list with a entries (species-specific overlap and overall overlap)
  expect_true(all(purrr::map_int(sp_overlap, length) == 2))
  # All df entries >= 0 and <= 1 (thus probabilities)
  expect_equal(length(sp_list), 44)
  expect_true(all(purrr::map_lgl(sp_list, ~all(.$si >= 0))))
  expect_true(all(purrr::map_lgl(sp_list, ~all(.$si <= 1))))
  expect_true(all(purrr::map_lgl(sp_list, ~all(class(.) == c("tbl_df", "tbl", "data.frame")))))
})


