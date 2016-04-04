dir <- "c:/backup_c/ATLANTIS_Stuff/Baseruns/1235_v.13.0.0_ATLANTIS_NS/"

plot_mort <- function(data) {
  mort <- load_txt(dir = dir, file = "outputNorthSeaSpecificMort.txt")
  mort <- tidyr::separate_(mort, col = "code", into = c("species", "agecl", "notsure", "mort"), convert = TRUE)

  if (length(unique(mort$notsure)) == 1) mort$notsure <- NULL

  mort$agecl <- mort$agecl + 1



}
