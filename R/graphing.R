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
  ret <- ggplot2::theme_bw(...)
  ret$line <- ggplot2::element_blank()
  ret$rect <- ggplot2::element_blank()
  ret$strip.text <- ggplot2::element_blank()
  ret$axis.text <- ggplot2::element_blank()
  ret$plot.title <- ggplot2::element_blank()
  ret$axis.title <- ggplot2::element_blank()
  ret$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
  ret
}


#' Log scale for both x & y axes
#'
#' Transform both the x and y axis for the common case of a log-log plot
#'
#' @param ... Arguments passed on to the scales
#'
#' @export
scale_xy_log10 <- function(...) {
  list(ggplot2::scale_x_log10(...), ggplot2::scale_y_log10(...))
}


#' Percentage scales
#'
#' Format the labels on the x axis, y axis, or both
#'
#' @param ... Arguments passed on to the scales
#'
#' @name scale_percent
#'
#' @export
scale_x_percent <- function(...) {
  ggplot2::scale_x_continuous(..., labels = scales::percent_format())
}


#' @rdname scale_percent
#' @export
scale_y_percent <- function(...) {
  ggplot2::scale_y_continuous(..., labels = scales::percent_format())
}


#' @rdname scale_percent
#' @export
scale_xy_percent <- function(...) {
  list(scale_x_percent(...), scale_y_percent(...))
}
