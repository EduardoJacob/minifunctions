#' Send command to new terminal
#'
#' @param command Send command to terminal
#'
#' @export
#'
#' @examples
#' \dontrun{
#' terminal("echo 'Hello World'")
#' }
terminal = function(command) {
  command = paste0(command,"\r")
  term_id = rstudioapi::terminalCreate()
  rstudioapi::terminalSend(term_id,command )
  rstudioapi::terminalActivate()
}

