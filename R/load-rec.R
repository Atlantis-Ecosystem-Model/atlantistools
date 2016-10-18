#' Load information for SSB and Recruits from an Atlantis model run.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param yoy Character string of the YOY.txt file. Usually
#' 'output[...]YOY.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case. Biomass in tonnes per spawning event summed over the total model domain.
#' @param ssb Character string of the SSB.txt file. Usually
#' 'output[...]SSB.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#' @family load functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' load_rec(dir = d,
#'    yoy = "outputSETASYOY.txt",
#'    ssb = "outputSETASSSB.txt",
#'    prm_biol = "VMPA_setas_biol_fishing_Trunk.prm")

load_rec <- function(dir = getwd(), yoy, ssb, prm_biol) {
  # Read in files
  ssb <- load_txt(dir = dir, file = ssb)
  yoy <- load_txt(dir = dir, file = yoy)

  # Remove cohort information in yoy
  yoy$code <- gsub(pattern = ".0", replacement = "", x = yoy$code)

  # Remove zeros and duplicate values in ssb and yoy and combine!
  yoy <- yoy[yoy$atoutput != 0, ]
  yoy <- yoy[c(1, which(diff(yoy$atoutput, lag = 1) != 0) + 1), ]
  result <- dplyr::inner_join(x = yoy, y = ssb, by = c("time", "code"))

  # Extract info about recruit weights from the biological parameterfile!
  # string_prm_biol <- readLines(con = convert_path(dir = dir, file = prm_biol))
  acr <- unique(result$code)
  kwrr <- lapply(paste("KWRR", acr, sep = "_"), extract_prm, dir = dir, prm_biol = prm_biol)
  kwsr <- lapply(paste("KWSR", acr, sep = "_"), extract_prm, dir = dir, prm_biol = prm_biol)
  rec_weights <- data.frame(code = acr, rec_weights = unlist(kwrr) + unlist(kwsr), stringsAsFactors = F)

  # Combine with recruitment data and convert units!
  result <- dplyr::inner_join(x = result, y = rec_weights)
  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)
  result$atoutput.x <- ((result$atoutput.x / bio_conv) / result$rec_weights) / 1000

  # Final data transformations
  names(result)[names(result) == "atoutput.x"] <- "rec"
  names(result)[names(result) == "atoutput.y"] <- "ssb"
  names(result)[names(result) == "code"] <- "species"
  result <- result[, c("species", "time", "ssb", "rec")]

  return(result)
}



