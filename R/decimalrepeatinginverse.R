#' Computes the decimal that originates the given fraction
#'
#' @description
#' Computes the decimal that originates the given fraction
#' #############################
#' ### Author: Roman Chokler ###
#' #############################
#'
#' @param q gmp::as.bigq(numerator,denominator)
#'
#' @return dec string
#' @export
#'
#' @examples
#' decimalrepeatinginverse(gmp::as.bigq(4,7))
decimalrepeatinginverse = function(q) {
  sgn <- sign(q)
  num <- abs(gmp::numerator(q))
  den <- gmp::denominator(q)
  d <- den
  c2 <- 0
  c5 <- 0
  while(d %% 2==0)
  {
    d <- gmp::as.bigz(d/2)
    c2 <- c2 + 1
  }
  while(d %% 5==0)
  {
    d <- gmp::as.bigz(d/5)
    c5 <- c5 + 1
  }
  transient <- max(c2,c5)
  dec <- as.character(gmp::as.bigz(num/den))
  rem <- num %% den
  if (rem != 0)
  {
    dec <- paste0(dec,".")
    if (transient>0)
    {
      for(i in 1:transient)
      {
        rem <- rem * 10
        dec <- paste0(dec,gmp::as.bigz(rem/den))
        rem <- rem %% den
      }
    }
    if (rem != 0)
    {
      dec <- paste0(dec,"[")
      r <- rem
      rem <- rem * 10
      dec <- paste0(dec,gmp::as.bigz(rem/den))
      rem <- rem %% den
      while (rem != r)
      {
        rem <- rem * 10
        dec <- paste0(dec,gmp::as.bigz(rem/den))
        rem <- rem %% den
      }
      dec <- paste0(dec,"]")
    }
  }
  if (sgn == -1)
  {
    dec <- paste0("-",dec)
  }
  return(dec)
}















