#' Extract conversion factor used to transform data from nitrogen in mg to
#' biomass in tonnes.
#
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @return Conversion factor as numeric value.
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' get_conv_mgnbiot(dir = d, prm_biol = "VMPA_setas_biol_fishing_New.prm")

#' @export

get_conv_mgnbiot <- function(dir = getwd(), prm_biol){
  if (!is.null(dir)) prm_biol <- file.path(dir, prm_biol)
  prm_biol <- readLines(con = prm_biol)

  x_cn <- extract_prm(chars = prm_biol, variable = "X_CN")
  k_wetdry <- extract_prm(chars = prm_biol, variable = "k_wetdry")
  conv <- x_cn * k_wetdry / 1000000000
  return(conv)
}


