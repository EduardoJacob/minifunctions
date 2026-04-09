#' Print Data Frame as HTML Table
#'
#' @param df Data Frame to be printed as HTML Table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' printdataframe(iris)
#' }
printdataframe = function(df) {
  # df = iris
  # dataframe = "iris"
  dataframe = deparse(substitute(df))
  # RColorBrewer::display.brewer.all()
  colors = RColorBrewer::brewer.pal(3,"Set3")[1:2]
  Colors = rep(colors,length.out=nrow(df))

  # caption = paste0("<span style='color:black;font-weight:bold;text-align:center'>",dataframe," data frame</span>")
  caption = paste0("<p style='color:black;font-weight:bold;text-align:center'>",dataframe,"</p>")

  HTML = df |>
    knitr::kable(format="html",row.names=T,caption=caption,escape = FALSE) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                  full_width = F,
                  position = "center",
                  font_size = 12,
                  fixed_thead = F) |>
    kableExtra::row_spec(0, bold = T, color = "black", background = "#DDDDDD")
  # column_spec(4, bold = T, color = "white", background = "red")

  for ( i in 1:nrow(df)) {
    HTML = kableExtra::row_spec(HTML,i, bold = T, color = "black", background = Colors[i])
  }

  print(HTML)
  return(HTML)
}

# data(iris, package = "datasets")
# printdataframe(iris)



