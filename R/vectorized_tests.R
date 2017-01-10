#' Note that this comes from functions I previously wrote here:
#' https://github.com/dgrtwo/GSEAMA/blob/master/R/TestAssociation.R

#' Perform a Student's T-test comparing a metric to each column of a matrix
#'
#' This function is similar to \code{apply(m, 2, function(col) t.test(y ~ col)$p.value)},
#' but is vectorized to make it much faster
#'
#' @param m binary matrix
#' @param y numeric vector to test each column of m against
#' @param var.equal assume that the variances are equal
#' @param alternative whether to use a one-sided test, and if so which way
#'
#' @export
vectorized_t_test = function(m, y, var.equal=FALSE, alternative="two.sided") {
  stopifnot(NROW(m) == length(y))

  in.m = y * m
  out.m = y * (1 - m)
  n.in = colSums(m)
  n.out = NROW(m) - n.in

  in.mu = colSums(in.m) / n.in
  out.mu = colSums(out.m) / n.out

  v.in = colSums((in.m - t(t(m) * in.mu))^2) / (n.in - 1)
  v.out = colSums((out.m - t(t(m) * out.mu))^2) / (n.out - 1)

  if (var.equal) {
    df = NROW(m) - 2
    v = (n.in - 1) * v.in + (n.out - 1) * v.out
    v = v / df
    stderr <- sqrt(v*(1/n.in+1/n.out))
  }
  else {
    stderr.in <- sqrt(v.in/n.in)
    stderr.out <- sqrt(v.out/n.out)
    stderr <- sqrt(stderr.in^2 + stderr.out^2)
    df <- stderr^4/(stderr.in^4/(n.in-1) + stderr.out^4/(n.out-1))
  }

  tstat <- (in.mu - out.mu) / stderr

  PVAL <- switch(alternative, less = stats::pt(tstat, df),
                 greater = stats::pt(tstat, df, lower.tail = FALSE),
                 two.sided = 2 * stats::pt(-abs(tstat), df))

  # name them, if the matrix has names
  names(PVAL) <- colnames(m)
  PVAL
}


#' Perform a Wilcoxon rank-sum test comparing a metric to each column of a matrix
#'
#' This function is similar to \code{apply(m, 2, function(col) wilcox.test(y ~ col)$p.value)},
#' but is vectorized to make it much faster
#'
#' @param m binary matrix
#' @param y numeric vector to test each column of m against
#' @param alternative whether to use a one-sided test, and if so which way
#' @param tbl whether to return a data.frame rather than a vector
#'
#' @export
vectorized_wilcoxon_test = function(m, y, alternative="two.sided", tbl = FALSE) {
  # given a boolean matrix and a vector y, apply the wilcoxon test to see
  # if y depends on each column of the matrix, returning a vector of
  # p-values
  stopifnot(NROW(m) == length(y))

  rk = rank(y)
  n.x = colSums(m)
  n.y = NROW(m) - n.x

  STATISTIC = colSums(rk * m) - n.x * (n.x + 1) / 2
  NTIES = table(rk)

  z <- STATISTIC - n.x * n.y / 2

  CORRECTION <- switch(alternative,
                       "two.sided" = sign(z) * 0.5,
                       "greater" = 0.5,
                       "less" = -0.5)

  SIGMA <- sqrt((n.x * n.y / 12) *
                  ((n.x + n.y + 1)
                   - sum(NTIES^3 - NTIES)
                   / ((n.x + n.y) * (n.x + n.y - 1))))
  z <- (z - CORRECTION) / SIGMA

  PVAL <- switch(alternative,
                 "less" = stats::pnorm(z),
                 "greater" = stats::pnorm(z, lower.tail=FALSE),
                 "two.sided" = 2 * pmin(stats::pnorm(z),
                                        stats::pnorm(z, lower.tail = FALSE)))

  if (tbl) {
    # return as a tidy table
    auc <- STATISTIC / (n.x * n.y)
    data.frame(column = colnames(m), p.value = PVAL, auc = auc)
  } else {
    # name them, if the matrix has names
    names(PVAL) <- colnames(m)
    PVAL
  }
}
