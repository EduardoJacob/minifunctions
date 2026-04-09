#' Computes the fraction that originates the given fixed decimal
#'
#' @param fixed A Numeric with or without decimal point
#'
#' @export
#'
#' @examples
#' decimalfixed(0.352)
decimalfixed = function(fixed) {
  # Count the number of digits to the right of decimal point
  options(scipen=100,digits=20) # Prevent Scientific Notation

  # fixed = 0.7124803
  sfixed = as.character(fixed)
  digits = nchar(sfixed) - stringr::str_locate(sfixed,"\\.")[1]
  if ( is.na(digits) ) digits = 0
  multiplier = 10^digits

  denominator = multiplier
  numerator = fixed * multiplier
  simplify = numbers::GCD(numerator,denominator)
  numerator = numerator/simplify
  denominator = denominator/simplify
  message(sfixed," = ",numerator,"/",denominator," = ",numerator/denominator)
}







