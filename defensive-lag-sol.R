lag <- function(x, n = 1L) {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_count(n)
  
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

# Tests
lag(x = list("10", "2", "3"), 
    n = 2L)

lag(x = data.frame("10", "2", "3"), 
    n = 2L)

lag(x = as.matrix("10", "2", "3"), 
    n = 2L)

lag(x = c("10", "2", "3"), 
    n = "2L")

lag(x = c("10", "2", "3"), 
    n = 2L)

lag(x = c(), 
    n = 2L)

