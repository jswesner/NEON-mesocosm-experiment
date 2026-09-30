pparetocounts_vec <- function(x, xmin, xmax, lambda) {
  
  if (any(xmin <= 0 | xmin >= xmax, na.rm = TRUE)) {
    stop("Parameters out of bounds. Is xmin negative?")
  }
  
  out <- numeric(length(x))
  
  in_bounds <- x >= xmin & x <= xmax
  
  out[!in_bounds] <- 0
  
  idx <- in_bounds & lambda == -1
  out[idx] <-
    1 - (log(x[idx]) - log(xmin[idx])) /
    (log(xmax[idx]) - log(xmin[idx]))
  
  idx <- in_bounds & lambda != -1
  out[idx] <-
    1 - (
      (x[idx]^(lambda[idx] + 1) - xmin[idx]^(lambda[idx] + 1)) /
        (xmax[idx]^(lambda[idx] + 1) - xmin[idx]^(lambda[idx] + 1))
    )
  
  out
}

# vectorized pparetocounts. Need to add this to isdbayes