#' Return a Vector representing the Continued Fraction for the Input Number
#'
#' @param x Input Number
#' @param n Desired Precision (number of fractions)
#' @return Continued Fraction Vector
#' @export
#'
#' @examples
#' continuedfraction(1.24) # Do not use decimals when calling the function
#' continuedfraction(gmp::as.bigq(124,100)) # Use Fractions instead
#' continuedfraction(sqrt(2))
continuedfraction = function(x,n=10) {
  #x = as.bigq(612,93)
  CF = vector()
  for ( i in 1:n ) {
    t = trunc(x)
    CF = c(CF,as.numeric(t))
    frac = x - t
    if ( frac == 0 ) break
    x = 1/frac
  }
  return(CF)
}


