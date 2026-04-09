#' Inverse Continued Fraction for Simple Continued Fractions
#'
#' @param v Vector representing the Continued Fraction
#' @return A Number
#' @export
#'
#' @examples
#' continuedfractioninverse(c(1,2,2,2,2,2,2,2,2))
continuedfractioninverse = function(v) {
  L = length(v)
  num = gmp::as.bigz(v[L])
  for ( i in seq(L-1,1) ) num = gmp::as.bigz(v[i]) + gmp::as.bigq(1,num)
  message(num," = ",as.numeric(num))
  return(num)
}

