#' Submit a prompt to SerpApi
#'
#' @param prompt Text prompt
#'
#' @export
#'
#' @examples
#' \dontrun{
#' serpapi("Qual a capital de Portugal?")
#' }
serpapi = function(prompt) {
  message("Calling SerpApi: ", prompt )

  SERPAPI_KEY = Sys.getenv("SERPAPI_KEY")

  start_time = Sys.time()

  answer = httr2::request("https://serpapi.com/search.json") |>
    httr2::req_url_query(
      engine = "google",
      q = prompt,
      api_key = SERPAPI_KEY
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  answer = answer[["organic_results"]]

  # 2. Extract the specific fields from 'organic_results'
  answer = answer |>
    purrr::map_dfr(~list(
      source  = .x$source,
      title   = .x$title,
      snippet = .x$snippet
    ))


  time_diff = difftime(Sys.time(), start_time, units = "secs")
  message("Elapsed ", round(time_diff, 2), " seconds.")

  # save html on current directory
  # writeLines(html,"sepapi.html")

  return(answer)

}

