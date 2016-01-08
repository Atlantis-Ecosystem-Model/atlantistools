#' Load the box specification file for an Atlantis scenario
#'
#' Read in the \code{.bgm} file that specifies the the box coordinates
#' for an Atlantis scenario.
#'
#' @family load functions
#' @author Kelli Faye Johnson
#'
#' @template dir
#' @template file_bgm
#'
#' @return A list of information regarding boxes for an Atlantis scenario.
#' @export
#' @examples
#' scenario <- "INIT_VMPA_Jan2015"
#' d <- system.file("extdata", scenario, package = "atlantisom")
#' boxes <- load_box(dir = d, file_bgm = "VMPA_setas.bgm")
#'
load_box <- function(dir = getwd(), file_bgm) {
  if (is.null(dir)) {
    file.bgm <- file_bgm
  } else {
    file.bgm <- file.path(dir, file_bgm)
  }
  if (!file.exists(file.bgm)) {
    stop(paste("The file", file.bgm, "does not exist in the specified dir\n",
               "please check the arguments dir and file_bgm in load_box."))
  }

  data <- readLines(file.bgm)
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
