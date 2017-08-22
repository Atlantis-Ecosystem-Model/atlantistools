# Dataframes related to refernce extraction.

ref_lit <- tibble::tribble(
  ~year, ~author,                                           ~title,
   2008,  "Gislason, H., N. Daan, J.C. Rice and J.G. Pope",  "Does natural mortality depend on individual size?. ICES CM 2008/F:16.",
   1974,  "Daan, N.",                                        "Growth of the North Sea cod, Gadus morhua. Neth. J. Sea Res. 8(1):27-48.",
   1951,  "Chang, H.-W.",                                    "Age and growth of Callionymus lyra L. J. Mar. Biol. Asoc. U.K. 30(2):281-296."
)

devtools::use_data(ref_lit, overwrite = TRUE)

rm(list = ls())

