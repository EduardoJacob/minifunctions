#' Operates 2 arguments in High Precision
#'
#' @param a Argument 1
#' @param operator Some operator like "+", "*", etc
#' @param b Argument 2
#'
#' @return The Result of the Operation
#' @export
#'
#' @examples
#' highprecision(1,"/",31)
highprecision = function(a,operator,b) {
  return( Rmpfr::mpfr( do.call(operator,list(gmp::as.bigz(a),gmp::as.bigz(b)))  , precBits = 256) )
}


