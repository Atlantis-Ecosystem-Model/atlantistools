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
    dm_ids <- min(pos) : (pos[which(lags >= 4)] + 1)
  } else {
    dm_ids <- min(pos) : (max(pos) + 1)
  }

  # Write dietmatrix to prm_biol
  if (length(dm_ids) < (2 * nrow(dietmatrix))) {
    stop(paste0("Dietmatrix does not fit into ", prm_biol, ". Parameters are lost!"))
  } else {
    breaks <- length(dm_ids) - 2 * nrow(dietmatrix)
    avails <- apply(X = dietmatrix[, 5:ncol(dietmatrix)], FUN = paste, MARGIN = 1, collapse = "\t")
    tags <- dietmatrix[, "code"]
    dm_paste <- unlist(Map(f = c, tags, avails, USE.NAMES = F))
    dm_paste <- c(dm_paste, rep("", times = breaks))

    if (length(dm_paste) == length(dm_ids)) {
      biol[dm_ids] <- dm_paste
      writeLines(biol, convert_path(dir = dir, file = prm_biol))
    } else {
      stop("Dimensions do not match. Dietmatrix not updated!")
    }
  }
}


