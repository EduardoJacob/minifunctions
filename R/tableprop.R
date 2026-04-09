#' Display Count and Proportions of discrete Variable
#'
#' @description
#' Display the table count and proportions of variable discrete values
#'
#' @param v Vector of discrete variable
#'
#' @export
#'
tableprop = function(v) {
  tab = table(v)
  print( cbind(Count = tab, Proportion = prop.table(tab)) )
}




