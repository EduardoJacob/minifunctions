#' Mass Rename and extension fix for images in a folder
#'
#' @param folder Images Folder
#' @param image_suffix Image Suffix to be used on new names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imagerenamer("P:/DISKD/Wallpapers","IMG")
#' }
imagerenamer = function(folder,image_suffix) {
  # folder = "P:/DISKD/Wallpapers"
  # image_suffix = "IMG"
  files = list.files(folder,full.names=T)

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
    # format newname based on prefix image plus counter with 4 digits and extension
    newname = paste0(image_suffix,sprintf(sprintf_format,i),".",extension)
    # rename image

    cat(" - Renaming to",newname,"\n")
    file.rename(f, file.path(folder, newname))
  }

}

