### Solution defensive-lag-ex ###

library(checkmate)

# Original function
lag <- function(x, n = 1L) {
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

# Modified function
lag_modified <- function(x, lag = 1L) {
  assert_atomic_vector(x, min.len = 1) # x should be a non-zero-length vector
  # lag should be a single positive finite  numeric value
  assert_number(lag, lower = 0, finite = TRUE)

  if (!test_count(lag)) {
    warning(
      "lag not an integer, rounding lag = ", lag, " to the nearest integer (",
      round(lag), ")."
    )
    lag <- round(lag)
  }

  if (lag > length(x)) {
    warning("lag greater than length of vector, setting lag = length(x).")
    lag <- length(x)
  }

  x_length <- length(x)
  c(rep(NA, lag), x[seq_len(x_length - lag)])
}

testthat::test_file("tests-defensive-lag-ex.R")
