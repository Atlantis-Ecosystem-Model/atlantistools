#' Change the availability matrix to simpliyfy automated ATLANTIS calibrations.
#'
#' Change the availability of predator XXX on specific preygroups.
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
#' @param pred Character vector of predator Acronyms (see \code{get_acronyms()}).
#' Selecting \code{NULL} as pred results in all predators being selected. This
#' can be helpful if you want to increase the feeding pressure on a specific
#' prey item by all groups.
#' @param pred_stanza Integer vector indicating if the predator is juvenile (= 1) or
#' adult (= 2). \code{pred} and \code{pred_stanza} need to be of the same length.
#' In rare instances, e.g. pred_stanza being \code{NULL} or one single integer
#' either all pred_stanzas are selected or the single pred_stanza is applied to all
#' predators.
#' @param prey List of character vectors of prey Acronyms (see \code{get_acronyms()}).
#' \code{pred} and \code{prey} need to be of the same length. Selecting \code{NULL} as
#' prey results in all prey groups being selected. This
#' can be helpful if you want to increase the available prey for a specific predator
#' overall.
#' @param roc Vector of multiplication factors which shall be applied to the old set of parameters.
#' Please supply one value per selected group. In case relative is FALSE the new absolute values
#' can be passed as roc.
#' @param relative Logical if TRUE values are changed relative to base values. If FALSE new values can
#' be passed directly.
#' @param save_to_disc Logical indicating if the resulting prm file should be overwritten
#' (\code{TRUE}) or not (\code{FALSE}).
#' @return parameterfile *.prm file with the new parameter values.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' dm <- change_avail(dir = d,
#'                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'                    fgs = "SETasGroups.csv",
#'                    pred = "FPL",
#'                    pred_stanza = 1,
#'                    prey = "FPL",
#'                    roc = 0.1234,
#'                    relative = F,
#'                    save_to_disc = FALSE)
#' # Show only rows with availability of 0.1234
#' dm[apply(apply(dm[, 5:ncol(dm)], MARGIN = 2, function(x) x == 0.1234), MARGIN = 1, any), ]
#'
#' dm <- change_avail(dir = d,
#'                    prm_biol = "VMPA_setas_biol_fishing_New.prm",
#'                    fgs = "SETasGroups.csv",
#'                    pred = c("FPL", "FPO"),
#'                    pred_stanza = c(1, 2),
#'                    prey = list(c("FPL", "FPO"), c("FPS", "FVD")),
#'                    roc = list(c(0.1111, 0.2222), c(0.3333, 0.4444)),
#'                    relative = F,
#'                    save_to_disc = FALSE)
#'
#' # Show only rows with availability of 0.1111, 0.2222, 0.3333 or 0.4444
#' dm[apply(apply(dm[, 5:ncol(dm)], MARGIN = 2, function(x) is.element(x,  c(0.1111, 0.2222, 0.3333, 0.4444))), MARGIN = 1, any), ]

change_avail <- function(dir = getwd(), prm_biol, fgs, pred = NULL, pred_stanza = NULL,
                         prey = NULL, roc, relative = TRUE, save_to_disc = TRUE) {
  # Set variables in case no predators are selected!
  ff <- load_fgs(dir = dir, fgs = fgs)

  # No predator selectd --> select all
  if (is.null(pred)) {
    pred <- ff$Code[ff$isPredator == 1]
    if (length(pred_stanza) == 1) { # and one pred_stanza selected --> set constant for all predators
      pred_stanza <- rep(pred_stanza, times = length(pred))
    }
    if (is.null(pred_stanza)) { # and no pred_stanza selected --> select all!
      pred_stanza <- rep(c(1, 2), each = length(pred))
      pred <- rep(pred, times = 2)
    }
  }

  # Predator selectd but no pred_stanza?
  if (!is.null(pred) & is.null(pred_stanza)){
    pred_stanza <- rep(c(1, 2), times = length(pred))
    pred <- rep(pred, each = 2)
  }

  # Predator select and only one or none species?
  if (length(pred) != length(prey)) {
    dummy <- vector(mode = "list", length = length(pred))
    dummy2 <- dummy
    for (i in seq_along(dummy)) {
      if (!is.null(prey)) dummy[[i]] <- prey
      if (!is.null(roc)) dummy2[[i]] <- roc
    }
    prey <- dummy
    roc <- dummy2
  }

  if (length(pred) != length(pred_stanza)) stop("Parameters pred and pred_stanza do not match.")
  if (length(prey) != length(roc)) stop("Parameters roc and prey do not match.")
  if (length(pred) != length(prey)) stop("Parameters pred and prey do not match.")

  dm <- load_dietmatrix(dir = dir, prm_biol = prm_biol, fgs = fgs, transform = TRUE)

  # Create dataframe of rocs!
  roc_df <- vector(mode = "list", length = length(pred))
  for (i in seq_along(pred)) {
    if (is.null(prey[[i]])) prey[[i]] <- unique(dm$prey)
    # if (length(roc[[i]]) == 1)
    roc_df[[i]] <- data.frame(pred[i], pred_stanza[i], prey[[i]], roc[[i]], stringsAsFactors = FALSE)
  }
  roc_df <- do.call(rbind, roc_df)
  names(roc_df) <- c("pred", "pred_stanza", "prey", "roc")

  dm <- dplyr::left_join(dm, roc_df, by = c("pred", "pred_stanza", "prey")) # used to suppress message
  na_roc <- is.na(dm$roc)
  if (relative) {
    dm$avail[!na_roc] <- dm$avail[!na_roc] * dm$roc[!na_roc]
  } else {
    dm$avail[!na_roc] <- dm$roc[!na_roc]
  }
  dm$roc <- NULL
  id <- dm$avail > 0.9
  if (sum(id) >= 1) {
    warning(paste(sum(id), "availabilities were > 0.9 after the calculations. Changed to 0.9."))
    dm$avail[id] <- 0.9
  }

  # Convert to wide dataframe
  dm <- tidyr::spread(dm, key = "prey", value = "avail")

  if (save_to_disc) {
    print("Writing new prm file!")
    write_diet(dir = dir, dietmatrix = dm, prm_biol = prm_biol)
  }

  invisible(dm)
}

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


