#' Extract strata from a tidied survival object
#'
#' @param tbl A table
#' @param column Column to extract. Should be of the form \code{col1=val1, col2=val2, ...}
#' @param ... Extra arguments passed on to \link[tidyr]{extract}, such as \code{remove} or
#' \code{convert}.df
#'
#' @importFrom rlang !!
#'
#' @examples
#'
#' library(survival)
#' library(broom)
#'
#' surv_fit <- survfit(Surv(stop, status) ~ sex + pcdx,
#'                     data = mgus1, subset = (start == 0))
#'
#' surv_fit %>%
#'   tidy() %>%
#'   extract_strata(strata)
#'
#' @export
extract_strata <- function(tbl, column, ...) {
  col <- rlang::enquo(column)
  # get the first value, and find the column names from that
  extracted <- stringr::str_extract_all(tbl[[rlang::as_label(col)]][1], "[^,]*?=")
  col_names <- stringr::str_trim(stringr::str_remove(extracted[[1]], "=$"))

  reg <- paste0(col_names, "=", "(.*)", collapse = ", ")

  tbl %>%
    tidyr::extract(!!col, col_names, reg, ...)
}
