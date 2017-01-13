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

