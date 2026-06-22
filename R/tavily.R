#' Submit a search to Tavily Service
#'
#' @param prompt Text prompt
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tavily("What is Langgraph?")
#' }
tavily = function(prompt,max_results=1) {
  message("Calling Tavily: ", prompt )

  start_time = Sys.time()

  answer = httr2::request("https://api.tavily.com/search") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      api_key = Sys.getenv("TAVILY_API_KEY"),
      query = prompt,
      search_depth = "advanced",
      max_results = max_results
    )) |>
    httr2::req_perform()

  answer = answer |>
    httr2::resp_body_json()

  answer = purrr::map_dfr(
    answer$results,
    ~data.frame(
      query   = answer$query,
      url     = .x$url,
      score   = .x$score,
      title   = .x$title,
      content = .x$content,
      stringsAsFactors = FALSE
    )
  )

  time_diff = difftime(Sys.time(), start_time, units = "secs")
  message("Elapsed ", round(time_diff, 2), " seconds.")

  return(answer)

}

