# utilities used with ggplot2

#' Rotate the labels on the x-axis of a graph
#'
#' @param ... Extra arguments passed on to element_text
#'
#' @export
rotate_x_labels <- function(...) {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, ...))
}
