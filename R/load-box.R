#' Load the box specification file for an Atlantis scenario
#'
#' Read in the \code{.bgm} file that specifies the the box coordinates
#' for an Atlantis scenario.
#'
#' @family load functions
#' @author Kelli Faye Johnson
#'
#' @param bgm Character string giving the connection to the atlantis bgm file.
#' The filename ends in \code{.bgm}.
#'
#' @return A list of information regarding boxes for an Atlantis scenario.
#' @export
#' @examples
#' d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
#' bgm <- file.path(d, "VMPA_setas.bgm")
#'
#' boxes <- load_box(bgm)

load_box <- function(bgm) {
  if (!file.exists(bgm)) {
    stop(paste("The file", bgm, "does not exist"))
  }

  data <- readLines(bgm)
  proj <- gsub("projection[[:space:]]+", "", grep("projection", data, value = TRUE))
  nbox <- as.numeric(gsub("nbox|[[:space:]]", "", grep("nbox", data, value = TRUE)))
  nface <- as.numeric(gsub("nface|[[:space:]]", "", grep("nface", data, value = TRUE)))
  maxwcbotz <- as.numeric(gsub("maxwcbotz|[[:space:]]", "",
                               grep("maxwcbotz", data, value = TRUE)))

  bnd_vert <- grep("bnd_vert", data, value = TRUE)
  bnd_vert <- gsub("bnd_vert[[:space:]]+", "", bnd_vert)
  bnd_vert <- strsplit(bnd_vert, "[[:space:]]+")
  bnd_vert <- do.call("rbind", bnd_vert)
  bnd_vert <- apply(bnd_vert, 2, as.numeric)

  get_box <- function(num, data) {
    info <- grep(paste0("box", num, "\\."), data, value = TRUE)
    out <- list()
    gsr <- function(data, get) {
      gsrout <- grep(get, data, value = TRUE)
      gsrout <- strsplit(gsrout, "[[:space:]]+|\t")
      if (length(gsrout) == 1) {
        gsrout <- as.numeric(gsrout[[1]][-1])
      } else {
        gsrout <- do.call("rbind", gsrout)
        gsrout <- apply(gsrout[, -1], 2, as.numeric)
      }
      return(gsrout)
    }

    out$label <- strsplit(grep("\\.label", info, value = TRUE),
                          "[[:space:]]+")[[1]][2]
    out$inside <- gsr(info, "inside")
    out$nconn <- gsr(info, "\\.nconn")
    out$iface <- gsr(info, "\\.iface")
    out$ibox <- gsr(info, "\\.ibox")
    out$area <- gsr(info, "\\.area")
    out$vertmix <- gsr(info, "\\.vertmix")
    out$horizmix <- gsr(info, "\\.horizmix")
    out$vert <- gsr(info, "vert[[:space:]]")
    out$botz <- gsr(info, "\\.botz")
    return(out)
  }
  get_face <- function(num, data) {
    info <- grep(paste0("face", num, "\\."), data, value = TRUE)
    info <- strsplit(info, "[[:space:]]+|\t")
    info[[3]][3] <- NA
    info <- do.call("rbind", info)
    info <- t(info[, -1])
    colnames(info) <- c("p1", "p2", "length", "cs", "lr")
    info <- apply(info, 2, as.numeric)
    return(info)
  }

  boxes <- lapply(0:(nbox - 1), get_box, data = data)
  names(boxes) <- 0:(nbox - 1)
  faces <- lapply(0:(nface - 1), get_face, data = data)
  names(faces) <- 0:(nface - 1)

  invisible(list("projection" = proj, "nbox" = nbox, "nface" = nface,
                 "maxwcbotz" = maxwcbotz, "bnd_vert" = bnd_vert, "boxes" = boxes,
                 "faces" = faces))
}
