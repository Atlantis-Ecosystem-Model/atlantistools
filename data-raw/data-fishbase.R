# A table of all the the species found in FishBase, including taxonomic classification and the
# Species Code (SpecCode) by which the species is identified in FishBase.
# Carl Boettiger carl@ropensci.org
# FishBase.org
fishbase_data <- rfishbase::fishbase

devtools::use_data(fishbase_data, overwrite = TRUE)

