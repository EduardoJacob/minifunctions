#' Allows for editing a string variable
#'
#' @param x String variable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' editvariable(x)
#' }
editvariable = function(x) {
  varname = deparse(substitute(x))
  print(varname)
  words_per_line = 10

  words = strsplit(x, "\\s+")[[1]]
  lines = split(words, ceiling(seq_along(words) / words_per_line))

  tf = tempfile(fileext = ".txt")

  writeLines(paste(sapply(lines, paste, collapse = " "), collapse = "\n"), tf)

  utils::file.edit(tf,title=varname)

  # readline("Press [Enter] after finishing editing in RStudio...")

  paste(readLines(tf, warn = FALSE), collapse = " ")
}




