### Erste Versuche/Draft für Exercise defensive-lag-ex ###

library(checkmate)
library(testthat)

# Originalfunktion
lag <- function(x, n = 1L) {
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

# mögliche ungeeignete inputs:
# x kein (atomic) Vektor
# x Vektor der Länge 0
# n nicht numeric
# n nicht skalar
# n kein Integer
# n negativ
# n größer als length(x)

# Angepasste Funktion
better_lag <- function(x, n = 1L) {
  assert_atomic_vector(x, min.len = 1)  # x should be a non-zero-length vector
  assert_number(n, lower = 0)  # n should be a single positive numeric value
  if (!test_count(n)) {
    warning("n not an integer, rounding n = ", n, " to the nearest integer (", 
            round(n), ")")
    n <- round(n)
  }
  if (n > length(x)) {
    warning("lag n greater than length of vector, setting n = length(x)")
    n <- length(x)
  }
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

# Tests
context("improved lag function")
test_that("function throws error when x not an atomic vector with length >= 0", {
          expect_error(better_lag(list(1:10, c("a", "b", "c"))))
          expect_error(better_lag(matrix(1:10, nrow = 2)))
          expect_error(better_lag(iris))
          expect_error(better_lag(numeric()))
})
test_that("function throws error or warning when n not a non-negative integer", {
  expect_error(better_lag(1:10, n = 1:2))
  expect_error(better_lag(1:10, n = matrix(1:10, nrow = 2)))
  expect_error(better_lag(1:10, n = -1))
  expect_warning(better_lag(1:10, n = 11))
  expect_warning(better_lag(1:10, n = Inf))
  expect_warning(better_lag(1:10, n = 2.5))
})
test_that("rounding and setting n = length(x) works", {
  expect_equal(better_lag(c(rep(1, 5)), n = 1.1), c(NA, rep(1, 4)))
  expect_equal(better_lag(c(rep(1, 5)), n = 6), c(rep(NA, 5), integer(0)))
})

better_lag(list(1:10), n = 1L)
x1 <- list(1:10, 1:10)
better_lag(mydata, n = 1L)
x2 <- c(1, "nc", NA)
lag(1:10, n = 1)
lag(list(1:10), n = 1)

lag(c("dd", "df", "ac"), n = 1:2)



