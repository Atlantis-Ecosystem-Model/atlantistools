#' Get boundary boxes from Atlantis box information.
#'
#' Use the output from \code{\link{load_box}} and obtain a \code{vector}
#' specifying which boxes are along the boundary.
#'
#' @family get functions
#' @seealso \code{\link{load_box}}
#' @author Kelli Faye Johnson
#'
#' @param boxinfo A \code{list} as returned from \code{\link{load_box}}.
#'
#' @return A \code{vector} specifying which boxes are on the boundary.
#' @export
#' @family get functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' boxes <- load_box(bgm = file.path(d, "VMPA_setas.bgm"))
#' get_boundary(boxinfo = boxes)

get_boundary <- function(boxinfo) {
  new <- vector()
#   boxes <- sapply(boxinfo$boxes, "[[", "vert")
  iface <- sapply(boxinfo$boxes, "[[", "iface")
  facepoints <- do.call("rbind",lapply(lapply(boxinfo$faces, "[", , 1:2),
                                       apply, 2, paste, collapse = "_"))
  vertices <- apply(boxinfo$bnd_vert, 1, paste, collapse = "_")
  for (ii in seq(dim(facepoints)[1])) {
    # Check how many points of a given box (ii) have points that are also
    # listed in the boundary box points
    temp <- which(sapply(facepoints[ii, ],
                         function(x) x %in% vertices))
    # Check that the box has more than one point in the boundary conditions
    # because some boxes can have just one point and not be a boundary box
    temp <- ifelse(length(temp) == 2, ii - 1, NA)
    new <- unique(c(new, temp))
  }
  # new is a vector of which faces are in the boundary points
  new <- new[order(new)]
  new <- new[!is.na(new)]

  hasextface <- sapply(iface, function(x) x %in% new)
  out <- names(hasextface[sapply(hasextface, function(x) all(x == TRUE))])
  out <- as.numeric(out)
  return(out)
}
