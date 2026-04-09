#' Return the In Shuffle of a vector
#'
#' @param v Input Vector
#' @param n Number of Shuffles
#' @return Shuffled Vector
#' @export
#'
#' @examples
#' shufflein(0:9,1)
shufflein = function(v,n=1) {
  L = length(v)/2

  for (i in 1:n) {
    newv = vector()
    for (j in 1:L) newv = c(newv,v[j+L],v[j])
    v = newv
  }

  return(v)
}


