#' Find Root using Newton Method
#'
#' @description
#' Find Root using Newton Method starting with x0 aproximation and executing n iterations
#' Newton's method doesn't always work.
#' Sometimes, things depend on making a good choice for the initial guess
#'
#' @param f A Function
#' @param x0 Initial guess
#' @param n Number of iterations
#'
#' @returns Root aproximation for given function
#'
#' @export
#'
#' @examples
#' newtonmethod(function(x) x^2 -3,3,5) # Square Root of 3
newtonmethod = function(f,x0,n) {
  x = x0
  message("Aproximation 0 : ",x)
  for ( i in 1:n ) {
    x = x - f(x)/numDeriv::grad(f,x)
    message("Aproximation ",i," : ",x)
  }
  return(x)
}





