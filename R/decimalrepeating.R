#' Computes the fraction that originates the given repeating decimal
#'
#' @description
#' Computes the fraction that originates the given repeating decimal
#' #############################
#' ### Author: Roman Chokler ###
#' #############################
#'
#' @param dec A string, with the repeating decimal between square brackets
#'
#' @return fraction
#' @export
#'
#' @examples
#' decimalrepeating("1.8[3]")
decimalrepeating <- function(dec) {
  bq <- 0
  sgn <- 1
  n <- nchar(dec)
  if (regexpr("-",dec)[1]==1)
  {
    sgn <- -1
    dec <- substr(dec,2,n)
    n <- n - 1
  }
  pnt <- regexpr("\\.",dec)[1]
  rstart <- regexpr("\\[",dec)[1]
  rend <- regexpr("\\]",dec)[1]
  if (pnt == -1)
  {
    return(gmp::as.bigq(dec) * sgn)
  }
  if (n==pnt)
  {
    return((gmp::as.bigq(substr(dec,1,pnt-1))) * sgn)
  }
  bq <- gmp::as.bigq(substr(dec,1,pnt-1))
  if (rstart == -1)
  {
    transient <- substr(dec,pnt+1,n)
    den <- gmp::pow.bigz(10,nchar(transient))
    transient <- sub("^0+","",transient)
    if (transient == "")
    {
      transient <- "0"
    }
    return((bq + gmp::div.bigq(transient,den)) * sgn)
  }
  den <- gmp::as.bigz(1)
  if (rstart > pnt + 1)
  {
    transient <- substr(dec,pnt+1,rstart-1)
    den <- gmp::pow.bigz(10,nchar(transient))
    transient <- sub("^0+","",transient)
    if (transient == "")
    {
      transient <- "0"
    }
    bq <- bq + gmp::div.bigq(transient,den)
  }
  if ((rend == -1) || (rstart >= rend - 1) || n > rend)
  {
    return(gmp::as.bigq(NA))
  }
  rep <- substr(dec,rstart+1,rend-1)
  den <- den * (gmp::pow.bigz(10,nchar(rep)) - 1)
  rep <- sub("^0+","",rep)
  if (rep == "")
  {
    rep <- "0"
  }
  return((bq + gmp::div.bigq(rep,den)) * sgn)
}









