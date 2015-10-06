context("sparse_cast")

require(Matrix)

dat <- data.frame(a = c("row1", "row1", "row2", "row2", "row2"),
                  b = c("col1", "col2", "col1", "col3", "col4"),
                  val = 2)

test_that("Sparse cast can turn a data frame into a sparse matrix", {
  m <- sparse_cast(dat, a, b, val)
  expect_equal(dim(m), c(2, 4))
  expect_equal(sum(m), 2 * nrow(dat))
})

test_that("The value argument to sparse_cast can be numeric or a column", {
  m2 <- sparse_cast(dat, a, b, value = 3)
  expect_equal(dim(m2), c(2, 4))
  expect_equal(sum(m2), 3 * nrow(dat))
})
