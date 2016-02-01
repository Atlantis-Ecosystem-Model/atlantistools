#' Load information for SSB and Recruits from an Atlantis model run.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param yoy Character string of the YOY.txt file. Usually
#' 'output[...]YOY.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param ssb Character string of the SSB.txt file. Usually
#' 'output[...]SSB.txt'. In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string as nc. In addition set dir to 'NULL' in this
#' case.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param prm_run Character string giving the filename of the run
#' parameterfile. Usually "[...]run_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param modelstart Character string giving the start of the model run
#' in the format \code{'yyyy-mm-dd'}.
#' @return Dataframe with information about ssb in tonnes and recruits in
#' thousands.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' load_rec(dir = d,
#'    yoy = "outputSETASYOY.txt",
#'    ssb = "outputSETASSSB.txt",
#'    fgs = "SETasGroups.csv",
#'    prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'    prm_run = "VMPA_setas_run_fishing_F_New.prm",
#'    modelstart = "1991-01-01")

load_rec <- function(dir = getwd(), yoy, ssb, fgs, prm_biol, prm_run, modelstart) {
  load_txt <- function(dir, file) {
    file <- convert_path(dir = dir, file = file)
    data <- read.table(file, header = T)
    data <- tidyr::gather_(data, key_col = "code", value_col = "atoutput", gather_cols = names(data)[2:length(names(data))])
    data$code <- as.character(data$code)
    names(data) <- tolower(names(data))
    return(data)
  }

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
  string_prm_biol <- readLines(con = convert_path(dir = dir, file = prm_biol))
  acr <- get_age_acronyms(dir = dir, fgs = fgs)
  kwrr <- lapply(paste("KWRR", acr, sep = "_"), extract_param, chars = string_prm_biol)
  kwsr <- lapply(paste("KWSR", acr, sep = "_"), extract_param, chars = string_prm_biol)
  rec_weights <- data.frame(code = acr, rec_weights = unlist(kwrr) + unlist(kwsr), stringsAsFactors = F)

  # Combine with recruitment data and convert units!
  result <- dplyr::inner_join(x = result, y = rec_weights)
  bio_conv <- get_conv_mgnbiot(dir = dir, prm_biol = prm_biol)
  result$atoutput.y <- ((result$atoutput.y / bio_conv) / result$rec_weights) / 1000

  # Final data transformations
  result$species <- convert_factor(data_fgs = load_fgs(dir = dir, fgs = fgs), col = result$code)
  result <- convert_time(dir = dir, prm_run = prm_run, data = result, modelstart = modelstart, stock_state = TRUE)
  names(result)[names(result) == "atoutput.x"] <- "ssb"
  names(result)[names(result) == "atoutput.y"] <- "rec"
  result <- result[, c("species", "time", "ssb", "rec")]

  return(result)
}



