library("rmarkdown")

dir <- file.path(getwd(), "vignettes")

render(input = file.path(dir, "model-preprocess.Rmd"))

preprocess <- result

devtools::use_data(preprocess, overwrite = TRUE)

file.remove(file.path(dir, "model-preprocess.html"))

rm(list = ls())

