#' Return the abbreviations of the 12 months of the year in the current language
#'
#' @return months
#' @export
#'
#' @examples
#' getmonths()
getmonths = function() {
  months = vector()
  for (i in 1:12) {
    m = paste0("0", i)
    m = stringr::str_sub(m, -2)
    m = paste0("2022-", m, "-01")
    m = substr(months(as.Date(m)) , 1 , 3)
    months = c(months, m)
  }
  return(months)
}
