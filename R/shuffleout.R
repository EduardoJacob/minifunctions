#' Return the Out Shuffle of a vector
#'
#' @param v Input Vector
#' @param n Number of Shuffles
#' @return Shuffled Vector
#' @export
#'
#' @examples
#' shuffleout(0:9,1)
shuffleout = function(v,n=1) {
  L = length(v)/2

  for (i in 1:n) {
    newv = vector()
    for (j in 1:L) newv = c(newv,v[j],v[j+L])
    v = newv
  }

  return(v)
}


