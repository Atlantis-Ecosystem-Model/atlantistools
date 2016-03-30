#' Combine ageclasses to juvenile and adult stanza according to age at maturity.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param data Dataframe whose ageclasses shall be combined.
#' @param col Character string giving the name of the group column in \code{data}.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @return dataframe with ageclasses combined to stanzas.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' combine_ages(dir = d, data = preprocess_setas$diet_specmort, group_col = "species", prm_biol = "VMPA_setas_biol_fishing_New.prm")

combine_ages <- function(dir = getwd(), data, col, prm_biol) {
  # Combine ageclasses to stanzas based on maturity!
  # Get agebased predators. We could extract the data from the functional
  # groups file. However, doing so would add an additional parameter to the function call...
  # Stanzas are introcued during the file loading procedure to reduce the final file size on the HDD.
  preds <- data %>%
    dplyr::group_by_(col) %>%
    dplyr::summarise_(count = ~length(unique(agecl))) %>%
    dplyr::filter(count == 10)
  preds <- preds[[1]]

  age_mat <- convert_path(dir = dir, file = prm_biol)
  age_mat <- readLines(con = age_mat)

  age_mat <- data.frame(pred = preds,
                        age_mat = vapply(paste0(preds, "_age_mat"), extract_prm, chars = age_mat, FUN.VALUE = numeric(1)),
                        stringsAsFactors = FALSE)

  # Finally set stanzas!
  data_stanza <- dplyr::left_join(data, age_mat)
  data_stanza$stanza <- ifelse(data_stanza$agecl < data_stanza$age_mat, "juvenile", "adult")
  data_stanza$stanza[is.na(data_stanza$age_mat)] <- "none"

  return(data_stanza)
}
