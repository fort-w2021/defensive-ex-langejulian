### Solution defensive-colmeans-ex ###

# Modified function
# compute means of all numeric columns in df
# inputs:
#   df: data.frame or something that can be converted to a data.frame
#   na.rm: TRUE/FALSE, whether NAs should be removed or not
# output: a data.frame with the means of all numeric columns
col_means <- function(df, na.rm = FALSE) {
  checkmate::assert_flag(na.rm)

  df <- as.data.frame(df)

  if (any(dim(df) == 0)) {
    warning("Input has 0 rows, 0 columns or both, returning empty dataframe.")
    return(data.frame())
  }

  numeric <- vapply(df, is.numeric, logical(1))
  df_numeric <- df[, numeric, drop = FALSE]

  if (any(dim(df_numeric) == 0)) {
    warning("Input has no numeric columns, returning empty dataframe.")
    return(data.frame())
  }

  as.data.frame(lapply(df_numeric, mean, na.rm = na.rm))
}

testthat::test_file("test-defensive-colmeans.R")
