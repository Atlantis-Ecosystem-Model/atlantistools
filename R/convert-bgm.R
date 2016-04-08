#' Transform data from bgm-file to map dataframe.
#'
#' @param dir Path of the Atlantis model folder.
#' @param bgm Character string giving the name of the atlantis bgm file.
#' @export
#' @family convert functions
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' bgm <- convert_bgm(dir = d, bgm = "VMPA_setas.bgm")
#' head(bgm)

convert_bgm <- function(dir = getwd(), bgm) {
  box <- load_box(dir = dir, bgm = bgm)

  # Get info of projection used! Some models don't use '+' to split their
  # arguments in the projection. So we add them here ;)
  proj_in <- box$projection
  if (!any(grepl(pattern = "[+]", proj_in))) {
    proj_in <- strsplit(proj_in, "[[:space:]]")
    for (i in seq_along(proj_in)) {
      proj_in[[i]] <- paste0("+", proj_in[[i]])
    }
    proj_in <- paste(unlist(proj_in), collapse = " ")
  }

  n_boxes <- box$nbox

  # Extract appearance if boxes.
  result <- list()
  for (i in 1:n_boxes) {
    result[[i]] <- data.frame(box$boxes[[i]]$vert)
    names(result[[i]]) <- c("lat", "long")
    result[[i]]$inside_lat <- box$boxes[[i]]$inside[1]
    result[[i]]$inside_long <- box$boxes[[i]]$inside[2]
    result[[i]]$box <- i - 1
  }
  result <- do.call(rbind, result)

  # Convert coordinates to map-coordinates!
  lat_long <- proj4::project(result[, 1:2], proj = proj_in, inverse = T)
  result$lat <- lat_long$x
  result$long <- lat_long$y
  lat_long <- proj4::project(result[, 3:4], proj = proj_in, inverse = T)
  result$inside_lat <- lat_long$x
  result$inside_long <- lat_long$y

  return(result)
}


