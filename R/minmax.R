#' Return the Min–Max normalization (0–1) for all numeric columns of Data Frame
#'
#' @param df Input DataFrame
#' @return Normalized DataFrame
#' @export
#'
minmax = function(df) {
  df = as.data.frame(lapply(df, function(x) {
    if (is.numeric(x)) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    else x
  }))

  return(df)
}


