#' Extract the dietmatrix from the biological parameterfile
#'
#' Extracts the diet matrix as long dataframe from the biological paremeter file
#' of any ATLANTIS simulation.
dir <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
prm_biol <- "VMPA_setas_biol_fishing_New.prm"
fgs <- "SETasGroups.csv"

load_dietmatrix <- function(dir = getwd(), prm_biol, fgs) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  acr <- fgs_data$Code[fgs_data$isPredator == 1]
  agecl <- fgs_data$NumCohorts[fgs_data$isPredator == 1]
  pstring <- "pPREY"

  coh10 <- acr[agecl == 10]
  coh2 <- acr[agecl != 10 & agecl != 1]
  coh1 <- acr[agecl == 1]

  if (length(c(coh10, coh2, coh1)) != length(acr)) stop("Incomplete functional groups file.")

  # Create vector of diet matrix strings available in prm_biol
  diet_strings <- c(
    # groups with NumCohorts == 10
    as.vector(t(outer(X = as.vector(outer(X = paste0(pstring, 1:2), Y = coh10, FUN = paste0)),
                      Y = 1:2, FUN = paste0))),
    # groups with 1 < NumCohorts < 10
    as.vector(t(outer(X = paste0(pstring, coh2), Y = 1:2, FUN = paste0))),
    # groups with NumCohorts == 1
    paste0(pstring, coh1))

  # Extract data from the biological parameter file.
  dietmatrix <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = diet_strings)

  # Extract predator, predator-stanzas and prey-stanzas.
  prey_stanza <- suppressWarnings(as.integer(substr(rownames(dietmatrix), start = nchar(pstring) + 1, stop = nchar(pstring) + 1)))
  prey_stanza[is.na(prey_stanza)] <- 1
  pred_stanza <- suppressWarnings(as.integer(substr(rownames(dietmatrix), start = nchar(rownames(dietmatrix)), stop = nchar(rownames(dietmatrix)) + 1)))
  pred_stanza[is.na(pred_stanza)] <- 1
  pred <- c(rep(coh10, each = 4), rep(coh2, each = 2), coh1)
  if (length(pred) != nrow(dietmatrix)) stop("Incomplete rows in diet data.")

  # Extract preys
  acronyms <- get_acronyms(dir = dir, fgs = fgs)
  prey <- c(acronyms, paste0(acronyms[(length(acronyms) - 2):length(acronyms)], "sed"))
  if (length(prey) != ncol(dietmatrix)) stop("Incomplete columns in diet data")

  result <- as.data.frame(dietmatrix, row.names = FALSE)
  names(result) <- prey

  names(result) <- c(get_)
  result <- cbind(result, pred, pred_stanza, prey_stanza)
  result$code <- rownames(dietmatrix)

  return(result)
}


