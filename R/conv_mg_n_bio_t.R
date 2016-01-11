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
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' conv_nmg_to_biomt(dir = d, prm_biol = "VMPA_setas_biol_fishing_Trunk.prm")

#' @export

conv_mg_n_bio_t <- function(dir, prm_biol){
  x_cn <- extract_from_prm(dir = dir, file = prm_biol, variable = "X_CN")
  k_wetdry <- extract_from_prm(dir = dir, file = prm_biol, variable = "k_wetdry")
  conv <- x_cn * k_wetdry / 1000000000
  return(conv)
}

# Extract value for a specific parameter from any parameter file.
# Only one paramter can be passed per function call. the
# parameter-flag and the value have to be in the same row.
extract_from_prm <- function(dir, file, variable){
  if (!is.null(dir)) file <- file.path(dir, file)
  result <- readLines(con = file)
  pos <- grep(pattern = variable, x = result)
  if (length(pos) == 0) {
    stop(paste("Variable", variable, "not found in", file))
  } else {
    if (length(pos) > 1) {
      stop(paste("Variable", variable, "found multiple times in", file))
    } else {
      result <- result[pos]
      result <- str_split_twice(char = result, min_only = TRUE)
      return(result)
    }
  }
}

# Split any character string multiple times and retrun the first (min_only = T)
# or all (min_only = F) numeric values found.
str_split_twice <- function(char, min_only){
  patterns <- c(" ", "\t")
  if (all(!stringr::str_detect(string = char, pattern = patterns))) {
    stop("Neither space nor tab present in variable char (string)!")
  }
  for (i in seq_along(patterns)) {
    char <- unlist(stringr::str_split(string = char, pattern = patterns[i]))
  }
  char <- suppressWarnings(as.numeric(char))
  if (all(is.na(char))) stop("No numeric value in variable char (string)!")
  if (min_only) char <- char[min(which(!is.na(char)))]
  return(char)
}

