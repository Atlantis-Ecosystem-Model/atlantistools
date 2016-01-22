#' Transform data from bgm-file to map dataframe.
#'
#' @param dir Path of the Atlantis model folder.
#' @param bgm Character string giving the name of the atlantis bgm file.

#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' convert_bgm(dir = d, bgm = "VMPA_setas.bgm")

convert_bgm <- function(dir, bgm) {
  box <- load_box(dir = dir, bgm = bgm)

  return(box)
}
