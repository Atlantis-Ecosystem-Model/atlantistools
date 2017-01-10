#' Extract the dietmatrix from the biological parameterfile
#'
#' Extracts the diet matrix as long dataframe from the biological paremeter file
#' of any ATLANTIS simulation.
#'
#' @param dir Character string giving the path of the Atlantis model folder.
#' If data is stored in multiple folders (e.g. main model folder and output
#' folder) you should use 'NULL' as dir.
#' @param prm_biol Character string giving the filename of the biological
#' parameterfile. Usually "[...]biol_fishing[...].prm". In case you are using
#' multiple folders for your model files and outputfiles pass the complete
#' folder/filename string and set dir to 'NULL'.
#' @param fgs Character string giving the filename of 'functionalGroups.csv'
#' file. In case you are using multiple folders for your model files and
#' outputfiles pass the complete folder/filename string as fgs.
#' In addition set dir to 'NULL' in this case.
#' @param transform Boolean indicating if the returned dataframe is displayed in
#' "long" (\code{transform = TRUE, default}) or "wide" (\code{transform = FALSE})
#' format. You should use the "wide" format in case you aim to change your
#' diet matrix entries.
#' @param convert_names Logical indicating if group codes are transformed to LongNames (\code{TRUE})
#' or not (default = \code{FALSE}).
#' @param dietmatrix Dataframe in 'long' format containing information about availabilities
#' with columns 'pred', 'prey', 'pred_stanza', 'prey_stanza', 'code', 'prey_id' and
#' 'avail'. The dataframe should be generated with \code{load_dietmatrix()}.
#' @param version_flag The version of atlantis that created the output files. 1 for bec_dev, 2 for trunk.
#' @return dataframe of the availability matrix in long format with columns
#' pred, pred_stanza (1 = juvenile, 2 = adult), prey_stanza, prey, avail, code.
#' @export

#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' dm <- load_dietmatrix(dir = d,
#'                       prm_biol = "VMPA_setas_biol_fishing_Trunk.prm",
#'                       fgs = "SETasGroupsDem_NoCep.csv")
#' head(dm, n = 10)
#'
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' dm <- load_dietmatrix(dir = d,
#'                       prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'                       fgs = "SETasGroups.csv")
#' head(dm, n = 10)

load_dietmatrix <- function(dir = getwd(), prm_biol, fgs, transform = TRUE, convert_names = FALSE, version_flag = 2) {
  fgs_data <- load_fgs(dir = dir, fgs = fgs)
  acr <- fgs_data$Code[fgs_data[, names(fgs_data)[names(fgs_data) %in% c("isPredator", "IsPredator")]] == 1]
  agecl <- fgs_data$NumCohorts[fgs_data[, names(fgs_data)[names(fgs_data) %in% c("isPredator", "IsPredator")]] == 1]
  pstring <- "pPREY"

  coh10 <- acr[agecl > 2]
  coh2 <- acr[agecl == 2]
  coh1 <- acr[agecl == 1]

  if (version_flag == 2) {
    coh10 <- c(coh10, coh2)
    coh2 <- NULL
  }

  if (length(c(coh10, coh2, coh1)) != length(acr)) stop("Incomplete functional groups file.")

  # Create vector of diet matrix strings available in prm_biol
  diet_strings <- c(
    # groups with NumCohorts > 2
    as.vector(t(outer(X = as.vector(outer(X = paste0(pstring, 1:2), Y = coh10, FUN = paste0)),
                      Y = 1:2, FUN = paste0))),
    # groups with NumCohorts == 1
    paste0(pstring, coh1))
    # groups with NumCohorts == 2
    if (length(coh2 > 0)) diet_strings <- c(diet_strings, as.vector(t(outer(X = paste0(pstring, coh2), Y = 1:2, FUN = paste0))))

  # Extract data from the biological parameter file.
  dietmatrix <- extract_prm_cohort(dir = dir, prm_biol = prm_biol, variables = diet_strings)
  if (length(unique(sapply(dietmatrix, length))) != 1) {
    stop("Number of entries in dietmatrix are not equal. Check your dietmatrix.")
  } else {
    dietmatrix <- do.call(rbind, dietmatrix)
  }

  # Extract predator, predator-stanzas and prey-stanzas.
  prey_stanza <- suppressWarnings(as.integer(substr(rownames(dietmatrix), start = nchar(pstring) + 1, stop = nchar(pstring) + 1)))
  prey_stanza[is.na(prey_stanza)] <- 2
  pred_stanza <- suppressWarnings(as.integer(substr(rownames(dietmatrix), start = nchar(rownames(dietmatrix)), stop = nchar(rownames(dietmatrix)) + 1)))
  pred_stanza[is.na(pred_stanza)] <- 2
  pred <- c(rep(coh10, each = 4),  coh1, rep(coh2, each = 2))
  if (length(pred) != nrow(dietmatrix)) stop("Incomplete rows in diet data.")

  # Extract preys. Add sediment prey to Carrion and Det groups!
  acronyms <- get_acronyms(dir = dir, fgs = fgs)
  car_det <- fgs_data$Code[grep(pattern = "(\\_DET|CARRION)", x = fgs_data[, names(fgs_data) %in% c("InvertType", "GroupType")])]
  prey <- c(acronyms, paste0(car_det, "sed"))
  if (length(prey) != ncol(dietmatrix)) stop("Incomplete columns in diet data")

  # Convert to dataframe.
  result <- as.data.frame(dietmatrix, row.names = FALSE)
  names(result) <- prey
  result <- cbind(result, pred, pred_stanza, prey_stanza, stringsAsFactors = FALSE)
  result$code <- rownames(dietmatrix)
  result <- result[, c("pred", "pred_stanza", "prey_stanza", "code", prey)]

  # Transform to long format
  if (transform) {
    result <- tidyr::gather_(data = result, key = "prey", value = "avail",
                             names(result)[!is.element(names(result), c("pred", "pred_stanza", "prey_stanza", "code"))])
    prey_order <- data.frame(prey = prey, prey_id = 1:length(prey), stringsAsFactors = FALSE)
    result <- dplyr::left_join(result, prey_order)
    if (convert_names) {
      result <- dplyr::mutate_at(result, .cols = c("pred", "prey"), .funs = convert_factor, data_fgs = fgs_data)
    }
  }

  return(result)
}


#' @export
#' @rdname load_dietmatrix
# Write dietmatrix dataframe in wide format to hdd.
write_diet <- function(dir = getwd(), dietmatrix, prm_biol) {
  # Find dietmatrix in biological parameterfile!
  pstring <- "pPREY"
  biol <- convert_path(dir = dir, file = prm_biol)
  biol <- readLines(biol)
  pos <- grep(pattern = pstring, x = biol)

  # Remove explanatory rows
  pos <- pos[pos != vapply(c("pPREY1FY1", "pPREY1FY2"), FUN = grep, FUN.VALUE = integer(1), x = biol)]

  # Define start and end of dietmatrix
  lags <- diff(pos)
  if (any(lags > 3)) {
    # Some models overwrite invertebrate availabilities.
    warning(paste0("Multiple linesbreaks present in ", prm_biol,
                   ". End of dietmatrix might not be detected correctly. Please check your parameter file."))
    dm_ids <- min(pos):(pos[which(lags >= 4)] + 1)
  } else {
    dm_ids <- min(pos):(max(pos) + 1)
  }

  # Write dietmatrix to prm_biol
  if (length(dm_ids) < (2 * nrow(dietmatrix))) {
    stop(paste0("Dietmatrix does not fit into ", prm_biol, ". Parameters are lost!"))
  } else {
    breaks <- length(dm_ids) - 2 * nrow(dietmatrix)
    avails <- apply(X = dietmatrix[, 5:ncol(dietmatrix)], FUN = paste, MARGIN = 1, collapse = "\t")
    tags <- paste(dietmatrix[, "code"], ncol(dietmatrix) - 4, sep = "\t")
    dm_paste <- unlist(Map(f = c, tags, avails, USE.NAMES = F))
    dm_paste <- c(dm_paste, rep("", times = breaks))

    if (length(dm_paste) == length(dm_ids)) {
      biol[dm_ids] <- dm_paste
      print("Writing new prm file!")
      writeLines(biol, convert_path(dir = dir, file = prm_biol))
    } else {
      stop("Dimensions do not match. Dietmatrix not updated!")
    }
  }
}



