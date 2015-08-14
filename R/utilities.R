#' Detach and then reload a package based on a character string of its name
#'
#' If the package is not already loaded, it still loads it
#'
#' @param package_name A character string (not a bare name, yet)
#'
#' @export
relibrary <- function(package_name) {
  # detach the existing package, avoiding errors if it's not loaded
  p <- paste0("package:", package_name)
  tryCatch(eval(substitute(detach(p, unload = TRUE), list(p = p))),
           error = function(e) NULL)
  require(package_name, character.only = TRUE)
}


#' Reorder a column of a tbl based on another
#'
#' @param col Column to use for reordering
#' @param value Value to reorder by
#' @param ... Extra arguments passed on to reorder, such as \code{function}
#'
#' @export
reorder_col <- function(.data, col, value, ...) {
  # ungroup (will regroup later)
  grps <- dplyr::groups(.data)
  .data <- dplyr::ungroup(.data)

  change_col <- as.character(substitute(col))

  call <- substitute(with(.data, reorder(col, value, ...)),
                     list(col = substitute(col), value = substitute(value)))
  .data[[change_col]] <- eval(call)

  # regroup with the original
  ret <- dplyr::group_by_(.data, .dots = grps)
  ret
}


#' Wrapper around expand.grid more suited to factorial experiments
#'
#' Wrapper around \code{\link{expand.grid}} that returns a tbl_df instead of a
#' data.frame, that allows grouping by all columns as an option, and that
#' treats stringsAsFactors as FALSE by default.
#'
#' @param ... Options passed on to \code{\link{expand.grid}}
#' @param stringsAsFactors Whether to treat strings as factors, by default
#' off (thankfully)
#' @param group_by_all whether to group
#'
#' @export
expand_grid <- function(..., stringsAsFactors = FALSE, group_by_all = FALSE) {
  ret <- dplyr::tbl_df(expand.grid(..., stringsAsFactors = stringsAsFactors))
  if (group_by_all) {
    ret <-  dplyr::group_by_(ret, .dots = colnames(ret))
  }
  ret
}
