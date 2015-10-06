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


#' Multiplies matrices after matching the rows of one to the columns of the other
#'
#' This is a custom infix operator: %<*>% multiplies matrices, but first matches the
#' column names of the first with the rows of the second. This works only if the first
#' matrix has column names and the second has row names, so they can be matched.
#'
#' @param m1 First matrix
#' @param m2 Second matrix
#'
#' @return A matrix with \code{nrow(m1)} rows and \code{ncol(m2)} columns.
#'
#' @export
'%<*>%' <- function(m1, m2) {
  if (is.null(colnames(m1))) {
    stop("First matrix does not have column names; cannot use %<*>%")
  } else if (is.null(rownames(m2))) {
    stop("Second matrix does not have column names; cannot use %<*>%")
  }

  n <- intersect(colnames(m1), rownames(m2))

  m1[, n] %*% m2[n, ]
}


#' Standard-evaluation version of sparse_cast
#'
#' @param data A tbl
#' @param row Name of column to use as row names
#' @param column Name of column to use as column names
#' @param value Name of column to use as sparse matrix values,
#' or a numeric value to use
#'
#' @import Matrix
#' @export
sparse_cast_ <- function(data, row_col, column_col, value_col = NULL) {
  row_names <- data[[row_col]]
  col_names <- data[[column_col]]
  if (is.numeric(value_col)) {
    values <- value_col
  } else {
    values <- data[[value_col]]
  }

  # if it's a factor, preserve ordering
  row_u <- if (is.factor(row_names)) levels(row_names) else unique(row_names)
  col_u <- if (is.factor(col_names)) levels(col_names) else unique(col_names)

  ret <- Matrix(0, nrow = length(row_u), ncol = length(col_u),
                dimnames = list(as.character(row_u), as.character(col_u)),
                sparse = TRUE)

  ret[cbind(match(row_names, row_u), match(col_names, col_u))] <- values

  ret
}


#' Create a sparse matrix from row names, column names, and values
#' in a table.
#'
#' @param data A tbl
#' @param row A bare column name to use as row names in sparse matrix
#' @param column A bare column name to use as column names in sparse matrix
#' @param value A bare column name to use as sparse matrix values, default 1
#'
#' @return A sparse Matrix object, with one row for each unique value in
#' the \code{row} column, one column for each unique value in the \code{column}
#' column, and with as many non-zero values as there are rows in \code{data}.
#'
#' @examples
#'
#' dat <- data.frame(a = c("row1", "row1", "row2", "row2", "row2"),
#'   b = c("col1", "col2", "col1", "col3", "col4"),
#'   val = 2)
#'
#' spread_cast(dat, a, b)
#'
#'
#' spread_cast(dat, a, b, val)
#'
#' @name sparse_cast
#'
#' @export
sparse_cast <- function(data, row, column, value) {
  if (missing(value)) {
    value_col <- 1
  } else {
    value_col <- as.character(substitute(value))
    if (is.null(data[[value_col]])) {
      value_col <- value
    }
  }

  sparse_cast_(data, as.character(substitute(row)),
               as.character(substitute(column)), value_col)
}


#' Add counts in parentheses to a factor or character vector
#'
#' @param x A factor or character vector
#'
#' @name add_counts
#'
#' @export
add_counts <- function(x) UseMethod("add_counts")


#' @rdname add_counts
#' @export
add_counts.character <- function(x) {
  tab <- table(x)
  paste0(x, " (", tab[x], ")")
}


#' @rdname add_counts
#' @export
add_counts.factor <- function(x) {
  tab <- table(x)
  levels(x) <- paste0(levels(x), " (", tab[levels(x)], ")")
  x
}
