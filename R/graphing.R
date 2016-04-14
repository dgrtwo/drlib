# utilities used with ggplot2

#' Rotate the labels on the x-axis of a graph
#'
#' @param ... Extra arguments passed on to element_text
#'
#' @export
rotate_x_labels <- function(...) {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, ...))
}


#' Blank theme: used to come with ggplot2
#'
#' @param ... Arguments passed to theme_bw
#'
#' @export
theme_blank <- function(...) {
  ret <- theme_bw(...)
  ret$line <- element_blank()
  ret$rect <- element_blank()
  ret$strip.text <- element_blank()
  ret$axis.text <- element_blank()
  ret$plot.title <- element_blank()
  ret$axis.title <- element_blank()
  ret$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
  ret
}
