#' Generate Euclid Numbers
#'
#' @description
#' An Euclid Number is the Product of n first prime numbers, plus 1
#'
#' @param n nth Euclid Number
#'
#' @returns Euclid Number
#'
#' @export
#'
#' @examples
#' for ( i in 1:6 ) print( euclidnumbers(i) )
euclidnumbers = function(n) {
  return( prod(primes::generate_n_primes(n)) + 1 )
}



