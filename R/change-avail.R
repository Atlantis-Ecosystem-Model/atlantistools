#' Change the availability matrix to simpliyfy automated ATLANTIS calibrations.
#'
#' Change the availability of predator XXX on specific preygroups.
#'
#' @param dietmatrix Dataframe in 'long' format containing information about availabilities
#' with columns 'pred', 'prey', 'pred_stanza', 'prey_stanza', 'code', 'prey_id' and
#' 'avail'. The dataframe should be generated with \code{load_dietmatrix()}.
#' @param pred Character vector of predator Acronyms (see \code{get_acronyms()}).
#' Selecting \code{NULL} as pred results in all predators being selected. This
#' can be helpful if you want to increase the feeding pressure on a specific
#' prey item by all groups. Default is \code{NULL}.
#' @param pred_stanza Integer vector indicating if the predator is juvenile (= 1) or
#' adult (= 2). \code{pred} and \code{pred_stanza} need to be of the same length.
#' In rare instances, e.g. pred_stanza being \code{NULL} or one single integer
#' either all pred_stanzas are selected or the single pred_stanza is applied to all
#' predators. Default is \code{NULL}.
#' @param prey List of character vectors of prey Acronyms (see \code{get_acronyms()}).
#' \code{pred} and \code{prey} need to be of the same length. Selecting \code{NULL} as
#' prey results in all prey groups being selected. This
#' can be helpful if you want to increase the available prey for a specific predator
#' overall. Default is \code{NULL}.
#' @param roc Vector of multiplication factors which shall be applied to the old set of parameters.
#' Please supply one value per selected group. In case relative is FALSE the new absolute values
#' can be passed as roc.
#' @param relative Logical if TRUE values are changed relative to base values. If FALSE new values can
#' be passed directly. Default is \code{NULL}.
#' @param consecutive Boolean indicating if multiple calls to change_avail are performed one after
#' another \code{TRUE}. Default is \code{FALSE}.
#' @return parameterfile *.prm file with the new parameter values.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' dm <- load_dietmatrix(prm_biol = file.path(d, "VMPA_setas_biol_fishing_Trunk.prm"),
#'                       fgs = file.path(d, "SETasGroupsDem_NoCep.csv"))
#'
#' dm1 <- change_avail(dietmatrix = dm,
#'                     pred = "FPS",
#'                     pred_stanza = 1,
#'                     prey = "CEP",
#'                     roc = 0.1234,
#'                     relative = FALSE)
#' # Show only rows with availability of 0.1234
#' dm1[apply(apply(dm1[, 5:ncol(dm1)], MARGIN = 2, function(x) x == 0.1234), MARGIN = 1, any), ]
#'
#' dm2 <- change_avail(dietmatrix = dm,
#'                     pred = c("FPS", "FVS"),
#'                     pred_stanza = c(1, 2),
#'                     prey = list(c("FPS", "FVS"), c("FPS", "FVS")),
#'                     roc = list(c(0.1111, 0.2222), c(0.3333, 0.4444)),
#'                     relative = FALSE)
#'
#' # Show only rows with availability of 0.1111, 0.2222, 0.3333 or 0.4444
#' dm2[apply(apply(dm2[, 5:ncol(dm2)], MARGIN = 2,
#' function(x) is.element(x,  c(0.1111, 0.2222, 0.3333, 0.4444))), MARGIN = 1, any), ]

change_avail <- function(dietmatrix, pred = NULL, pred_stanza = NULL, prey = NULL, roc, relative = TRUE, consecutive = FALSE) {
  # Set variables in case no predators are selected! Need to update!
  dm <- dietmatrix
  prey_ordered <- unique(dm$prey[order(dm$prey_id)])


  # No predator selectd --> select all
  if (is.null(pred)) {
    # pred <- ff$Code[ff$isPredator == 1]
    pred <- unique(dm$pred)
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
  if (!consecutive) {
    dm$prey_id <- NULL
    dm <- tidyr::spread_(dm, key_col = "prey", value_col = "avail")
    dm <- dplyr::select_(dm, .dots = c(names(dm)[1:4], prey_ordered, "DLsed", "DRsed", "DCsed"))
  }

  invisible(dm)
}

