# based on: https://github.com/lionel-/vdiffr/blob/master/tests/testthat/helper-fontconfig.R
on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}
on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

