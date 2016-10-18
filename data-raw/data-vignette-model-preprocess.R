library("rmarkdown")

d <- system.file("vignettes", package = "atlantistools")

render(input = file.path(d, "model-preprocess.Rmd"))

preprocess <- result

devtools::use_data(preprocess, overwrite = TRUE)

file.remove(file.path(d, "model-preprocess.html"))

rm(list = ls())

