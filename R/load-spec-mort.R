#' Load mortality information from specificMort.txt
#'
#' Reads in the specificMort.txt file. Three values of instantaneous mortality for each functional group,
#' age group, and stock. Predation (M2), other natural mortality (M1), Fishing (F)
#'
#' @inheritParams load_nc
#' @inheritParams load_fgs
#' @inheritParams load_dietmatrix
#' @param mortFile Character string giving the path to the specificMort.txt file.
#' The filename usually contains \code{SpecificMort} and ends in \code{.txt}".
#' @return Data frame with information about sources of mortality (M1, M2, F).
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' specmort <- file.path(d, "outputSETASSpecificMort.txt")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
#' fgs <- file.path(d, "SETasGroups.csv")
#'
#' df <- load_spec_mort(specmort, prm_run, fgs)
#' head(df)
#'
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' specmort <- file.path(d, "outputSETASSpecificMort.txt")
#' prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
#' fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
#'
#' df <- load_spec_mort(specmort, prm_run, fgs)
#' head(df)

#BJS 7/15/16 add version_flag and make compatible with trunk output
load_spec_mort <- function(mortFile, prm_run, fgs, convert_names = FALSE) {

  df <- load_txt(file = mortFile)
  mort <- preprocess_txt(df_txt = df, into = c("code", "agecl", "empty_col", "mort")) %>%
    tibble::as_tibble()

  # Convert species codes to longnames!
  if (convert_names) {
    data_fgs <- load_fgs(fgs=fgs)
    mort <- mort %>%
      dplyr::left_join(.,data_fgs[,c("Code","LongName")], by = c("code"="Code")) %>%
      dplyr::rename(species = LongName)
  }

  # Convert time
  mort$time <- convert_time(prm_run = prm_run, col = mort$time)

  return(mort)
}







