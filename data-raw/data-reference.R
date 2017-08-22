# Dataframes related to refernce extraction.

ref_lit <- tibble::tribble(
  ~year, ~author,                                           ~title,
   2008,  "Gislason, H., N. Daan, J.C. Rice and J.G. Pope",  "Does natural mortality depend on individual size?. ICES CM 2008/F:16.",
   1974,  "Daan, N.",                                        "Growth of the North Sea cod, Gadus morhua. Neth. J. Sea Res. 8(1):27-48.",
   1951,  "Chang, H.-W.",                                    "Age and growth of Callionymus lyra L. J. Mar. Biol. Asoc. U.K. 30(2):281-296."
)

# Read in BIOTIC data to enable tests (Most of the time the website does not work thus interactive testing become a hassle.)
url <- paste0(stringr::str_split("Cancer pagurus", pattern = " ")[[1]], collapse = "%20")
url <- paste0("http://www.marlin.ac.uk/biotic/browse.php?sp=x&spn=", url)
cancer_pagurus_biotic_xml2 <- xml2::read_html(url)
cancer_pagurus_biotic_base <- readLines(url)

devtools::use_data(ref_lit, cancer_pagurus_biotic_xml2, cancer_pagurus_biotic_base, overwrite = TRUE)

rm(list = ls())



