#' Extract conversion factor used to transform data from nitrogen in mg to
#' biomass in tonnes.
#
#' @inheritParams extract_prm
#' @return Conversion factor as numeric value.
#' @family get functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#'
#' get_conv_mgnbiot(prm_biol)

#' @export

get_conv_mgnbiot <- function(prm_biol){
  x_cn <- extract_prm(prm_biol = prm_biol, variables = "X_CN")
  k_wetdry <- extract_prm(prm_biol = prm_biol, variables = "k_wetdry")
  conv <- x_cn * k_wetdry / 1000000000
  return(conv)
}


