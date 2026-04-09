#' Operates 2 arguments in the indicated numeric Base
#'
#' @param a Argument 1
#' @param operator Some operator like "+", "*", etc
#' @param b Argument 2
#' @param base Numeric Base System like 2, 3, 10, 16, etc
#'
#' @return The Result of the Operation expressed in the indicated Base
#' @export
#'
#' @examples
#' numberoperation("222","+","111",3)
numberoperation = function(a,operator,b,base) {
  decimal = do.call(operator,list(strtoi(a,base=base),strtoi(b,base=base)))
  based = oro.dicom::dec2base(decimal,base)
  message(a," ",operator," ",b," = ",based," ( decimal ",decimal," )")
  return(based)
}


