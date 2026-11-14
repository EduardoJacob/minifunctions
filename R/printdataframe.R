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

  # Iterate over dataframe columns
  for (col in names(df)) {

    # Only process character columns
    if (is.character(df[[col]])) {

      # Iterate over column elements
      for (i in seq_along(df[[col]])) {

        url = df[[col]][i]

        # Check if value is an URL
        is_url = grepl("^https?://", url)

        if (is_url) {

          # Check if URL points to an image
          is_img = FALSE

          if (expand_images) is_img = isimageurl(url)

          # Generate HTML
          if (is_img) {

            df[[col]][i] = paste0(
              "<a href='", url,
              "' target='_blank'><img src='",
              url, "'></a>"
            )

          } else {

            df[[col]][i] = paste0(
              "<a href='", url,
              "' target='_blank'>",
              url, "</a>"
            )
          }
        }
      }
    }
  }



  # Convert internal "\n" to "<br>"
  df = df |> dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\n", "<br>", .)))

  # Generate HTML table
  HTML = df |>
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



