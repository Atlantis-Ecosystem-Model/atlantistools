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

get_conv_mgnbiot <- function(dir, prm_biol){
  if (!is.null(dir)) prm_biol <- file.path(dir, prm_biol)
  prm_biol <- readLines(con = prm_biol)

  x_cn <- extract_param(chars = prm_biol, variable = "X_CN")
  k_wetdry <- extract_param(chars = prm_biol, variable = "k_wetdry")
  conv <- x_cn * k_wetdry / 1000000000
  return(conv)
}

# Extract value for a specific parameter from a Vector of character strings.
extract_param <- function(chars, variable){
  pos <- grep(pattern = variable, x = chars)
  if (length(pos) == 0) {
    stop(paste("Variable", variable, "not found."))
  } else {
    if (length(pos) > 1) {
      stop(paste("Variable", variable, "found multiple times."))
    } else {
      result <- chars[pos]
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
    stop("Neither space nor tab present.")
  }
  for (i in seq_along(patterns)) {
    char <- unlist(stringr::str_split(string = char, pattern = patterns[i]))
  }
  char <- suppressWarnings(as.numeric(char))
  if (all(is.na(char))) stop("No numeric value present.")
  if (min_only) char <- char[min(which(!is.na(char)))]
  return(char)
}

