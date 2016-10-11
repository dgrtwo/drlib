context("accumulate_while")

library(purrr)

test_that("accumulate_while works on simple examples", {
  x1 <- accumulate_while(1, ~ . + 1, ~ . < 10)
  expect_is(x1, "list")
  expect_equal(length(x1), 10)
  expect_equal(as_vector(x1), 1:10)

  x2 <- accumulate_while(10, ~ . - 1, ~ . > 1)
  expect_is(x2, "list")
  expect_equal(length(x2), 10)
  expect_equal(as_vector(x2), 10:1)
})


test_that("accumulate_while works with .compare", {
  x1 <- accumulate_while(1, ~ . / 2, ~ abs(.x - .y) > 1e-4, .compare = TRUE)
  expect_is(x1, "list")
  expect_equal(x1[[1]], 1)
  expect_equal(x1[[2]], 1 / 2)
  diffs <- abs(diff(as_vector(x1)))
  expect_equal(which(diffs <= 1e-4), length(diffs))
})


test_that("accumulate_while warns when it hits .max", {
  expect_warning(x1 <- accumulate_while(1, ~ . + 1, ~ . < 10, .max = 5), "Reached .max")
  expect_equal(length(x1), 5)
})


test_that("accumulate_while can be given an infinite .max", {
  x1 <- accumulate_while(1, ~ . + 1, ~ . < 110000, .max = Inf)
  expect_is(x1, "list")
  expect_equal(length(x1), 110000)
  expect_equal(x1[[110000]], 110000)
})
