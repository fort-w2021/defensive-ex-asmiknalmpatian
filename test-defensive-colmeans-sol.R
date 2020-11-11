col_means <- function(df, na_rm = FALSE) {
  checkmate::assert_logical(na_rm)
  
  if (checkmate::test_atomic_vector(df)) {
    warning("Mean of input vector has been computed.")
    return(mean(df))
  }
  
  
  assert(checkmate::check_data_frame(df),
         checkmate::check_matrix(df),
         checkmate::check_list(df), combine = "or")
  df <- as.data.frame(df)
  
  if (nrow(df) == 0) {
    warning("Input has 0 rows.")
    return(data.frame())
  }
  if (ncol(df) == 0) {
    warning("Input has 0 columns.")
    return(data.frame())
  }
  
  numeric <- vapply(df, is.numeric, logical(1))
  
  numeric_cols <- df[, numeric, drop = FALSE]
  
  if (sum(numeric) == 0) {
    warning("Input does not contain any numeric columns.")
    return(data.frame())
  }
  
  data.frame(lapply(numeric_cols, mean, na.rm = na_rm))
}


