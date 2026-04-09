#' Convert Integer from Base1 to Base2
#'
#' @param n Number to convert
#' @param base1 Input Numeric Base System like 2, 3, 10, 16, etc
#' @param base2 Output Numeric Base System like 2, 3, 10, 16, etc
#'
#' @return The converted Integer in the new Base
#' @export
#'
#' @examples
#' n = 100
#' for ( base in 2:16 ) message(n," ( base 10 ) = ",numberconvertion(n,10,base)," ( base ",base," )")
numberconvertion = function(n,base1,base2) {
  # Limited to base2 max 36
  decimal_string = TeachingDemos::digits(n)
  decimal = 0
  L = length(decimal_string)
  for ( i in 1:L ) decimal = decimal + decimal_string[i]*base1^(L-i)

  based = oro.dicom::dec2base(decimal,base2)

  if ( base2 == 10 ) {
    return(as.integer(based))
  } else {
    return(based)
  }

}


