#' Convert Integer from Base 10 to Fractional Base
#'
#' @param n Integer to convert
#' @param numerator The numerator of the base
#' @param denominator The denominator of the base
#'
#' @return A vector with the numeric representation on the new base
#' @export
#'
#' @examples
#' for ( i in 1:20 ) cat(i," = ",numberconversionfrac(i,4,3),"\n")
numberconversionfrac = function(n,numerator,denominator) {
  # n = 19
  # numerator = 4
  # denominator = 3
  result = vector()
  repeat {
    n1 = n%%numerator
    result = c(n1,result)
    n = denominator * trunc(n/numerator)
    if ( n == 0 ) return(result)
  }
}




