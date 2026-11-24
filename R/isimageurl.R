#' Check if URL points to image
#'
#' @param url A character string representing the URL to check.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' isimageurl("https://yt3.ggpht.com/tcKNto_1eN27XUiDvx00ffffff-no-rj")
#' }
isimageurl = function(url) {
  # url = "https://yt3.ggpht.com/tcKNto_1eN27XUiDvcXV90ffffff-no-rj"
  req <- httr2::request(url) |>
    httr2::req_method("HEAD") |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) NULL
  )

  if (is.null(resp)) return(FALSE)

  content_type <- httr2::resp_header(resp, "content-type")

  if (is.null(content_type)) return(FALSE)

  grepl("^image/", content_type, ignore.case = TRUE)

}



