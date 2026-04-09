#' Computes the Digital Root of integer/string n
#'
#' @param n Input
#'
#' @return Digital Root
#' @export
#'
#' @examples
#' digitalroot(123456)
digitalroot = function(n) {
  x = sum(as.numeric(unlist(strsplit(as.character(n),""))))
  if ( x < 10 ) {
    k = x
  } else {
    k = digitalroot(x)
  }
  return(k)
}

