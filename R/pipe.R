#' Pipeoperator
#'
#' Atlantistools makes heavy use of dplyr data transformations.
#' Therefore it is advisable to import the pipeoperator \code{\%>\%}
#' from magrittr.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
