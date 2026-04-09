#' Return the Data Frame with the Derangements built from the input vector
#'
#' @param v Input Vector
#' @return derangements dataframe
#' @export
#'
#' @examples
#' derangements(c("A","B","C"))
derangements = function(v) {
  N = length(v)
  df = data.frame( gtools::permutations(N,N,v,repeats.allowed=F) )
  dfout = data.frame()
  for ( line in 1:nrow(df) ) {
    vec = as.character(as.vector(df[line,]))
    if ( sum(v == vec) == 0 ) dfout = rbind(dfout,df[line,])
  }
  if (nrow(dfout) > 0) rownames(dfout) = 1:nrow(dfout)
  return(dfout)
}


