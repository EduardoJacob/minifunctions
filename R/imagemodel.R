#' Identify the IA model name that generated the image
#'
#' @param image Image Path
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imagemodel("P:/DISKD/Wallpapers/img250.png")
#' }
imagemodel = function(image) {
  # image = "P:/DISKD/Wallpapers/img184.png"

  img = png::readPNG(image, info = TRUE)
  workflow = attr(img, "info")
  workflow = workflow$text
  if ( is.null(workflow) ) return("NA")

  pattern = "[^\"[:space:]]+\\.(safetensors|ckpt|pt|pth|gguf|bin)"

  model = regmatches(
    workflow,
    gregexpr(pattern, workflow, ignore.case = TRUE)
  )[[1]]

  return(model)
}


