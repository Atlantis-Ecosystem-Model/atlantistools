#' flip layers for visualisation
#'
#' @param data dataframe with columns polygon and layer. layer id is based on
#' atlantis output (0 = layer closest to the sediment)
#' @return dataframe with fliped layerids. 1 = surface.
#' @export
#'
#' @examples
#' data <- rbind(expand.grid(species = "sp1", polygon = 0, layer = 0:7),
#'               expand.grid(species = "sp1", polygon = 1, layer = 0:4),
#'               expand.grid(species = "sp1", polygon = 2, layer = 0:2),
#'               expand.grid(species = "sp1", polygon = 3, layer = c(0:3, 7)))
#' data$atoutput <- runif(nrow(data), min = 0, max = 2)
#' flip_layers(data)

flip_layers <- function(data) {
  if (any(!c("polygon", "layer") %in% names(data))) stop("Columns polygon and layer not present in data.")
  df <- unique(dplyr::select_(data, .dots = c("polygon", "layer")))
  sediment <- max(df$layer)
  layers <- split(df, df$polygon)

  # for(i in seq_along(layers)) {
  #   if()
  # }
  #
  # , function(x) sort(x$layer))

  df$layer_fliped <- unlist(lapply(layers, function(x) rev(x) + 1))

  data_fliped <- data %>%
    dplyr::left_join(df, by = "polygon") %>%
    dplyr::rename_(.dots = c("layer" = "layer.y"))
  data_fliped$layer.x <- NULL
  return(data_fliped)
}

add_fliped_layers <- function(df, sed) {
  df <- dplyr::arrange_(df, ~layer)
  if (any(df$layer == sed)) {
    layer_fliped <- c(rev(df$layer[df$layer != sed]), sed) + 1
  } else {
    layer_fliped <- rev(df$layer) + 1
  }
  df$layer_fliped <- layer_fliped
  return(df)
}


