#' Return The Truth Table of the Input Function as a Data Frame
#'
#' @param f A Logical Function - A Function that returns Truth or False
#' @return The Truth Table of the Input Function as a Data Frame
#' @export
#'
#' @examples
#' truthtable( function(A,B,C,D) A && B && (!C) && (!D) )
truthtable = function(f) {
  function_name = deparse(substitute(f))[1]
  function_body = deparse(f)[2]
  #print(function_name)
  #print(function_body)
  if ( nchar(function_name) > 1 ) function_name = function_body

  names = methods::formalArgs(f)
  N = length(names)
  truthtable = data.frame( gtools::permutations(2,N,c(T,F),repeats.allowed=T) )

  result = vector()
  for ( line in 1:nrow(truthtable) ) {
    input = as.list( as.logical(as.vector(truthtable[line,])) )
    result = c(result,do.call(f,input)  )
  }
  truthtable$result = result

  colnames(truthtable) = c(names,function_name)
  truthtable[] = lapply(truthtable, as.numeric)
  return(truthtable)
}


