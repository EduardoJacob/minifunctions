#' Generate the repunit 1111... of Length n
#'
#' @param n Length of the Repunit
#'
#' @return repunit
#' @export
#'
#' @examples
#' repunit(5)
repunit = function(n) {
  return(paste(rep(1,n),collapse = ""))
}

