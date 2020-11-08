### Solution defensive-colmeans-ex ###

# compute means of all numeric columns in df
# output: a data.frame
col_means <- function(df) {
  numeric <- vapply(df, is.numeric, logical(1))
  numeric_cols <- df[, numeric]
  
  
  as.data.frame(lapply(numeric_cols, mean))
}

# compute means of all numeric columns in df
# inputs:
#   df: data.frame or something that can be converted to a data.frame
#   na.rm: TRUE/FALSE, whether NAs should be removed
# output: a data.frame with the means of all numeric columns
col_means_modified <- function(df, na.rm = FALSE) {
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

