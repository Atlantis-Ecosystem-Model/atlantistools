#' Load mortality information from outputMort.txt
#'
#' Loads Mort.txt file and partitions mortality based on fishing (F) and other mortality (M)
#' Note: As the Atlantis manual states "This file is currenlty only useful for looking
#' at \strong{relative M vs F values} for a species, as it does not give accurate mortalities".
#' Also if a species is set as isImpacted in the \emph{functional_group.csv}, it will have some F value
#' even if it is not explicity targeted by fishing.
#'
#' @inheritParams load_nc
#' @inheritParams load_fgs
#' @inheritParams load_dietmatrix
#' @param mortFile Character string giving the path to the Mort.txt file.
#' The filename usually contains \code{Mort} and ends in \code{.txt}".
#' @return Dataframe with information about sources of mortality (M, F).
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' mortFile <- file.path(d, "outputSETASSpecificPredMort.txt")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
#' fgs <- file.path(d, "SETasGroups.csv")
#'
#' df <- load_mort(mortFile, prm_run, fgs, version_flag = 1)
#' head(df)
#'
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' mortFile <- file.path(d, "outputSETASSpecificPredMort.txt")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#'
#' df <- load_spec_mort(mortFile, prm_run, fgs)
#' head(df)

#BJS 7/15/16 add version_flag and make compatible with trunk output
load_mort <- function(mortFile, prm_run, fgs) {

  mort <- load_txt(file = mortFile, id_col = c("Time"))

  # separate species code.mortalityType into two columns
  mort <- mort %>%
    tidyr::separate(code,into = c("code","source"),sep = "\\.") %>%
    tibble::as_tibble()


  # First time step only has 0s as entry!
  # ***This will cause a potentially unnoticed bug when this issue in the output file gets fixed
  mort <- mort[mort$time != 0, ]

  # Convert time
  mort$time <- convert_time(prm_run = prm_run, col = mort$time)


  return(mort)
}
