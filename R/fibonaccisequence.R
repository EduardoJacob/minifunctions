#' Return a vector of fibonnaci numbers
#'
#' @param v a vector with the first 2 or more elements of a fibonacci sequence
#' @param n the length of the desired sequence
#' @return vector of fibonnaci numbers
#' @export
#'
#' @examples
#' fibonaccisequence(c(1,1),10)
#' fibonaccisequence(c(2,3),10)
#' fibonaccisequence(c(1,2,4),10)
fibonaccisequence = function(v,n) {
  # u1 = 1
  # u2 = 1
  L = length(v)
  n = n - L
  for ( i in 1:n ) {
    i2 = i + L - 1
    v = c(v,sum(v[i:i2]))
  }
  return(v)
}

