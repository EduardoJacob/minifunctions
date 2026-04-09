#' Computes the permutations of vector, allowing for repeated values
#'
#' @param v Input Vector
#' @param L Length of the intended permutations ( defaults to the length of v )
#'
#' @return Permutations Dataframe
#' @export
#'
#' @examples
#' permutations(c(1,3,4,4))
#' permutations(c(1,3,4,4),3)
permutations = function(v,L) {
  # v = c(1,3,4,4)
  # L = 3
  if (missing(L)) L=length(v)

  LV = length(v)
  if ( typeof(v) == "character" ) {
    v1 = 1:LV
  } else {
    v1 = letters[1:LV]
  }

  orderings = data.frame( gtools::permutations(LV,L,v1,repeats.allowed=F) )
  for ( i in 1:LV ) orderings = replace(orderings,orderings==v1[i],v[i])
  orderings = unique(orderings)

  if ( typeof(v) != "character" ) {
    orderings = as.data.frame(sapply(orderings, as.numeric))
  }
  return(orderings)
}



