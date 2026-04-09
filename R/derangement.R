#' Return the number of Derangements
#'
#' @param n Input Integer
#' @return Number of Derangements
#' @export
#'
#' @examples
#' for ( i in 1:10 ) print(derangement(i))
derangement = function(n) {
  if ( n == 1 ) return(0)
  if ( n == 2 ) return(1)
  return( (n-1)*(derangement(n-1) + derangement(n-2)) )
}
