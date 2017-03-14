#' Load information for SSB and Recruits from an Atlantis model run.
#'
#' @inheritParams extract_prm
#' @param yoy Character string giving the connection of the YOY file.
#' The filename usually contains \code{outputYOY} and ends in \code{.txt}".
#' @param ssb Character string giving the connection of the YOY file.
#' The filename usually contains \code{outputSSB} and ends in \code{.txt}".
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' yoy <- file.path(d, "outputSETASYOY.txt")
#' ssb <- file.path(d, "outputSETASSSB.txt")
#' prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
#'
#' load_rec(yoy, ssb, prm_biol)

load_rec <- function(yoy, ssb, prm_biol) {
  # Read in files
  ssb <- load_txt(file = ssb)
  yoy <- load_txt(file = yoy)

  # Remove cohort information in yoy
  yoy$code <- gsub(pattern = ".0", replacement = "", x = yoy$code)

  # Remove zeros and duplicate values in ssb and yoy and combine!
  yoy <- yoy[yoy$atoutput != 0, ]
  yoy <- yoy[c(1, which(diff(yoy$atoutput, lag = 1) != 0) + 1), ]
  result <- dplyr::inner_join(x = yoy, y = ssb, by = c("time", "code"))

  # Extract info about recruit weights from the biological parameterfile!
  acr <- unique(result$code)
  kwrr <- lapply(paste("KWRR", acr, sep = "_"), extract_prm, prm_biol = prm_biol)
  kwsr <- lapply(paste("KWSR", acr, sep = "_"), extract_prm, prm_biol = prm_biol)
  rec_weights <- data.frame(code = acr, rec_weights = unlist(kwrr) + unlist(kwsr), stringsAsFactors = F)

  # Combine with recruitment data and convert units!
  result <- dplyr::inner_join(x = result, y = rec_weights, by = "code")
  bio_conv <- get_conv_mgnbiot(prm_biol = prm_biol)
  result$atoutput.x <- ((result$atoutput.x / bio_conv) / result$rec_weights) / 1000

  # Final data transformations
  names(result)[names(result) == "atoutput.x"] <- "rec"
  names(result)[names(result) == "atoutput.y"] <- "ssb"
  names(result)[names(result) == "code"] <- "species"
  result <- result[, c("species", "time", "ssb", "rec")]

  return(result)
}



