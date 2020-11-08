### Tests for defensive-lag-ex ###

library(testthat)

context("modified lag function")

test_that("function throws error if x not an atomic vector with length >= 0", {
  expect_error(lag_modified(list(1:10, c("a", "b", "c"))))
  expect_error(lag_modified(matrix(1:10, nrow = 2)))
  expect_error(lag_modified(iris))
  expect_error(lag_modified(numeric()))
})

test_that("function throws error or warning if n not a non-negative integer", {
  expect_error(lag_modified(1:10, lag = 1:2))
  expect_error(lag_modified(1:10, lag = matrix(1:10, nrow = 2)))
  expect_error(lag_modified(1:10, lag = -1))
  expect_error(lag_modified(1:10, lag = Inf))
  expect_warning(lag_modified(1:10, lag = 11))
  expect_warning(lag_modified(1:10, lag = 2.5))
})

test_that("rounding and setting n = length(x) works", {
  expect_warning(lag_modified(c(rep(1, 5)), lag = 1.1))
  expect_equal(lag_modified(c(rep(1, 5)), lag = 1.1), c(NA, rep(1, 4)))
  expect_warning(lag_modified(c(rep(1, 5)), lag = 6))
  expect_equal(lag_modified(c(rep(1, 5)), lag = 6), c(rep(NA_integer_, 5)))
})
