#' Compute the Multiplicative Order of A modulo N
#'
#' @description
#' In number theory, given an integer A and a positive integer N with GCD(A,N) = 1,
#' the multiplicative order of A modulo N is the smallest positive integer k with A^k( mod N ) = 1. ( 0 < K < N )
#'
#' @param a Integer
#' @param n Integer Modulo
#'
#' @returns Multiplicative Order
#'
#' @export
#'
#' @examples
#' multiplicativeorder(13,100) # Returns 20
multiplicativeorder = function(a,n) {
  if ( numbers::GCD(a,n) != 1 ) {
    message("Error: GCD(",a,",",n,") must be 1")
    return()
  }
  smallest_positive_integer = 1

  multiplier = 1
  repeat {
    multiplier = (a*multiplier)%%n
    if ( multiplier == 1 ) return(smallest_positive_integer)
    smallest_positive_integer = smallest_positive_integer + 1
  }
}







