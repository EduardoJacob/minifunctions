#' Generate the Powerset of a Vector
#'
#' @param v Vector
#'
#' @returns A List of Vectors
#'
#' @export
#'
#' @examples
#' powerset(c("A","B","C"))
powerset = function(v) {
  result = list()
  result[[1]] = as.character()
  if ( missing(v) ) return(result)

  n = length(v)
  k = 1

  for ( r in 1:n ) {
    df = gtools::combinations(n,r,v,repeats.allowed=F)
    for ( line in 1:nrow(df) ) {
      set = as.character(as.vector(df[line,]))
      k = k + 1
      result[[k]] = set
    }
  }

  return(result)
}



