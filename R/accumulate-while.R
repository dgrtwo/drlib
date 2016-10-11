#' Accumulate a list by iteratively performing a function
#'
#' Perform a function to transform an object multiple times, until a predicate
#' is not satisfied.
#'
#' @param .x Starting value
#' @param .f Function to apply to transform each step to the next
#' @param .p A predicate applied to each intermediate step: if false, it stops the iteration
#' @param ... Extra arguments passed to .f
#' @param .compare Whether the predicate will be given the last two steps rather than just
#' the most recent.
#' @param .max Maximum number of iterations to perform. Give \code{Inf} to continue growing the
#' vector as long as necessary.
#'
#' @details
#'
#' I've suggested a function like this in an issue on purrr:
#' \url{https://github.com/hadley/purrr/issues/253}.
#' I've put it here so I can use it personally in the meantime.
#' If it gets added to purrr, I will remove it from drlib.
#'
#' @examples
#'
#' accumulate_while(1, ~ . + 1, ~ . < 5)
#'
#' # accumulate until difference is within some tolerance
#' accumulate_while(1, ~ . / 2, ~ abs(.x - .y) > 1e-4, .compare = TRUE)
#'
#' @export
accumulate_while <- function(.x, .f, .p, ..., .compare = FALSE, .max = 100000) {
  .f <- purrr::as_function(.f, ...)
  .p <- purrr::as_function(.p)

  infinite <- is.infinite(.max)

  if (infinite) {
    .max <- 100000
  }
  ret <- vector("list", .max)
  ret[[1]] <- .x

  so_far <- .x
  i <- 2
  while (TRUE) {
    while (i <= .max) {
      result <- .f(so_far)
      ret[[i]] <- result

      if (.compare && !.p(so_far, result)) {
        break
      }
      if (!.compare && !.p(result)) {
        break
      }

      so_far <- result
      i <- i + 1
    }

    if (i <= .max) {
      break
    }

    # we reached the maximum
    if (infinite) {
      ret <- c(ret, vector("list", length(ret)))
      .max <- length(ret)
    } else {
      warning("Reached .max of ", .max)
      break
    }
  }

  utils::head(ret, i)
}
