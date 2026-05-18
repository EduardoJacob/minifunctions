#' Mass Rename and extension fix for images in a folder
#'
#' @param folder Optional Images Folder. Defaults to Current Working Directory
#' @param image_prefix Optional Image Prefix to be used on new names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imagerenamer("P:/DISKD/Wallpapers","IMG")
#' }
imagerenamer = function(folder=getwd(),image_prefix) {
  # folder = "C:/MEGA/R/Config.AI"
  # image_prefix = "IMG"

  files = list.files(folder, pattern = "\\.(jpg|jpeg|png|gif|bmp)$", full.names = TRUE, ignore.case = TRUE)

  N = length(files)
  D = nchar(as.character(N))
  sprintf_format = paste0("%0",D,"d")

  for (i in 1:N ) {
    # i = 1
    f = files[i]
    cat("Processing image",i,"/",N,":",f)

    img = magick::image_read(f)
    # Convert extension to lowercase
    extension = tolower(magick::image_info(img)$format)
    # format newname based on prefix image plus counter and extension
    if ( missing(image_prefix) ) {
      newname = tools::file_path_sans_ext(basename(f))
      newname = paste0(newname,".",extension)
    } else {
      newname = paste0(image_prefix,sprintf(sprintf_format,i),".",extension)
    }

    # rename image
    cat(" - Renaming to",newname,"\n")
    file.rename(f, file.path(folder, newname))
  }

}

