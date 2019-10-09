#' Extract strata from a tidied survival object
#'
#' The tidied results of a survival object are in
#'
#' @param tbl A table
#' @param column Column to extract. Should be of the form \code{col1=val1, col2=val2, ...}
#' @param ... Extra arguments passed on to \link[tidyr]{extract}, such as \code{remove} or
#' \code{convert}.
#'
#' @importFrom rlang !!
#'
#' @examples
#'
#' library(survival)
#' library(broom)
#'
#' surv_td <- survfit(Surv(stop - start, status) ~ sex + pcdx,
#'                     data = mgus1) %>%
#'   tidy()
#'
#' surv_td
#'
#' # Extract into "sex" and "pcdx" columns
#' surv_fit %>%
#'   extract_strata(strata)
#'
#' # Now visualize, separating by strata
#' library(ggplot2)
#' surv_fit %>%
#'   extract_strata(strata) %>%
#'   ggplot(aes(time, estimate, color = sex)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
#'   facet_wrap(~ pcdx)
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
