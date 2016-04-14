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
#' @param data A tbl
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
#' @param row_col String version of column to use as row names
#' @param column_col String version of column to use as column names
#' @param value_col String version of column to use as sparse matrix values,
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
#' sparse_cast(dat, a, b)
#'
#' sparse_cast(dat, a, b, val)
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


#' Decode a string in base 62 into a number
#'
#' Note that this returns a float vector, not an integer, due to
#' the (unfortunate) 32-bit limit of integers in R.
#'
#' @param s a character vector in base 62 encoding
#'
#' @examples
#'
#' strings <- c("wojWiVquxXy", "x1VMNGRqI3m")
#'
#' decode_base_62(strings)
#'
#' @export
decode_base_62 <- function(s) {
  base_62_chars <- stringr::str_split("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", "")[[1]]
  nums <- 62 ^ seq_len(max(nchar(s)))

  decoded <- sapply(stringr::str_split(s, ""), function(ch) {
    indices <- rev(match(ch, base_62_chars) - 1)
    sum(indices * head(nums, length(ch)))
  })

  decoded
}

# originally from tidyr
col_name <- function (x, default = stop("Please supply column name", call. = FALSE))
{
  if (is.character(x))
    return(x)
  if (identical(x, quote(expr = )))
    return(default)
  if (is.name(x))
    return(as.character(x))
  if (is.null(x))
    return(x)
  stop("Invalid column specification", call. = FALSE)
}

#' Split a string column and then unnest on that split column
#'
#' @export
split_unnest <- function(data, col, sep = "[^[:alnum:]]+") {
  col <- col_name(substitute(col))
  data[[col]] <- stringr::str_split(data[[col]], sep)

  data <- tidyr::unnest_(data, col)
  data <- data[data[[col]] != ""]
  data
}


#' Melt multiple matrices of the same dimension into multiple columns
#'
#' @param ... Matrices, given as named arguments
#' @param varnames Variable names to use in molten data.frame
#' @param tbldf Whether to return a tbl_df instead of a data.frame
#'
#' @export
melt_multiple <- function(..., varnames = NULL, tbldf = TRUE) {
  datas <- list(...)
  if (length(datas) == 0) {
    stop("Must be given at least one data frame to melt")
  }
  if (is.null(varnames)) {
    varnames <- dimnames(datas[[1]])
  }

  # if any are missing, replace them
  n <- names(datas)
  n[n == ""] <- paste0("value", seq_len(sum(n == "")))

  ret <- reshape2::melt(datas[[1]], varnames = varnames, value.name = n[1])
  for (n in names(datas)[-1]) {
    ret[[n]] <- c(datas[[n]])
  }

  if (tbldf) {
    ret <- dplyr::tbl_df(ret)
  }

  ret
}


#' Perform a correlation and melt the result into a table
#'
#' @param m A matrix or an object that can be coerced to one (like a
#' sparseMatrix)
#' @param use Passed on to \code{\link{cor}}
#' @param method Passed on to \code{\link{cor}}
#' @param varnames Names for variable columns, passed on to \code{reshape2::melt}
#' @param varnames Name for value column, passed on to \code{reshape2::melt}
#' @param threshold Optionally, a minimum threshold to include
#' @param sort Whether to sort the results in descending correlation
#'
#' @export
tidy_cor <- function(m, use = "everything", method = "pearson",
                     varnames = c("Var1", "Var2"),
                     value.name = "Correlation", threshold = NULL,
                     sort = FALSE) {
  co <- stats::cor(as.matrix(m), use = use, method = method)

  ret <- reshape2::melt(co, varnames = varnames, value.name = value.name)
  ret <- dplyr::tbl_df(ret)

  # filter out self-matches
  ret <- ret[ret[[varnames[1]]] != ret[[varnames[2]]], ]
  if (!is.null(threshold)) {
    ret <- ret[ret[[value.name]] >= threshold, ]
  }

  if (sort) {
    ret <- ret[order(-ret[[value.name]]), ]
  }

  ret
}


#' Wrap the countrycode package to convert back and forth between country
#' code and country name
#'
#' If given a vector of country codes, turn into names. If given a vector
#' of names, turn back into country codes. This is a very simple utility
#' since I find myself using this code too often
#'
#' @param x Vector of either 2-letter country code or country names
#'
#' @export
convert_country <- function(x) {
  if (all(is.na(x) | nchar(x) == 2)) {
    countrycode::countrycode(x, "iso2c", "country.name")
  } else {
    countrycode::countrycode(x, "country.name", "iso2c")
  }
}
