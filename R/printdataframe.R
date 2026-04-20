#' Print Data Frame as HTML Table
#'
#' @param df Data Frame to be printed as HTML Table
#' @param expand_images Boolean: if TRUE, image URLs will be expanded to show the images in the table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' printdataframe(iris)
#' }
printdataframe = function(df, expand_images = FALSE) {

  dataframe = deparse(substitute(df))
  colors = RColorBrewer::brewer.pal(3,"Set3")[1:2]
  Colors = rep(colors, length.out = nrow(df))

  caption = paste0("<p style='color:black;font-weight:bold;text-align:center'>", dataframe, "</p>")

  # Remove excess blanks
  df = df |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))

  # Convert to clicable Links and expand images
  df_processed = df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ {
      if (is.character(.)) {
        # Usamos ifelse (vetorizado) em vez de if simples para evitar o erro de 'length > 1'
        is_url = grepl("http", .)
        is_img = grepl("\\.(jpg|jpeg|png|gif|webp)", ., ignore.case = TRUE)

        # Lógica para links e imagens
        res = .
        if (any(is_url)) {
          res = ifelse(is_url,
                       ifelse(expand_images & is_img,
                              paste0("<a href='", ., "' target='_blank'><img src='", ., "'></a>"),
                              paste0("<a href='", ., "' target='_blank'>", ., "</a>")),
                       .)
        }
        return(res)
      } else {
        return(.) # Retorna a coluna numérica intacta
      }
    })) |>

  # Convert internal "\n" to "<br>"
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\n", "<br>", .)))

  # Generate HTML table
  HTML = df_processed |>
    knitr::kable(format="html", row.names = T, caption = caption, escape = FALSE) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                              full_width = F,
                              position = "center",
                              font_size = 12,
                              fixed_thead = F) |>
    kableExtra::row_spec(0, bold = T, color = "black", background = "#DDDDDD")

  for (i in 1:nrow(df)) {
    HTML = kableExtra::row_spec(HTML, i, bold = T, color = "black", background = Colors[i])
  }

  print(HTML)
  return(HTML)
}

# data(iris, package = "datasets")
# printdataframe(iris)



