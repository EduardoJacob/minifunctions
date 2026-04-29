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
  df = data.frame(Count = tab, Proportion = prop.table(tab))
  df$Proportion.v = NULL
  names(df) = c(deparse(substitute(v)),"Count", "Proportion")
  # colnames becomes disaligned when there are emojis
  # print( as_tibble(df) )
  # knitr::kable(df)
  print(df)
}




