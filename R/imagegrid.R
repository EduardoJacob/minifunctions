#' Display the images in a folder as a grid using the magick package
#'
#' @param folder Images folder
#' @param ncol Optional grid number of columns
#' @param nrow Optional grid number of rows
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imagegrid("P:/DISKD/Youtube R Programming/AppLogos")
#' }
imagegrid = function(folder, ncol=0, nrow=0) {
  # Check if folder exists
  if ( !dir.exists(folder) ) {
    message("folder not found ",folder)
    return(NULL)
  }

  files = list.files(folder, full.names = TRUE)
  info = file.info(files)

  files = data.frame(
    file_name = basename(files),
    created = as.Date(info$ctime)
  )

  # sort by created date descending
  files = files[order(files$created, decreasing = TRUE), ]
  rownames(files) = NULL

  file_paths = vector()

  # loop through the files in the folder
  N = 0
  for ( file in files$file_name ) {
    extension = tolower( tools::file_ext(file) )

    # if extension not in the list of image extensions, skip the file
    if ( extension  %notin% c("jpg", "jpeg", "png", "bmp", "gif") ) next

    file_path = file.path(folder, file)

    if ( file.info(file_path)$isdir ) next

    N = N + 1
    file_paths = c(file_paths, file_path)

  }

  if ( N == 0 ) {
    message("No image files found in ",folder)
    return(NULL)
  }

  # Read the images using magick
  images = lapply(file_paths, magick::image_read)

  # if ncol or nrow is not specified, calculate them based on the number of images
  if ( ncol == 0 || nrow == 0 ) {
    ncol = ceiling(sqrt(N))
    nrow = ceiling(N / ncol)
  }

  # Combine the images into a grid
  grid_image = magick::image_montage(
    do.call(c, images),
    tile = paste(ncol, nrow, sep = "x"),
    geometry = "320x180+10+10",
    bg = "#505050"
  )

  # Display the grid image
  print(grid_image)
}




