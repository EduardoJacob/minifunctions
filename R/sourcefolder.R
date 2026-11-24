#' Source all the R Scripts in a given folder
#'
#' @param folder Input folder
#' @export
#'
sourcefolder = function(folder) {

  functions = list.files(folder, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(functions,source))
  message(length(functions)," functions Loaded")

}


