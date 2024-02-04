
### Convert HEIC images from iPhone to PNG ###

library(magick)
library(stringr)


###########################
### Load in HEIC images ###
###########################

files <- dir("raw_img", full.names = TRUE)

img <- image_read(path = files)


#####################################################
### Select images that haven't been converted yet ###
#####################################################

processed <- gsub(x = dir("www"), pattern = "\\.jpeg$", "")
all <- gsub(x = dir("raw_img"), pattern = "\\.HEIC$", "")

ind <- which(!all %in% processed)

############################
### Convert HEIC to JPEG ###
############################

if (length(ind) > 0) {  #only write files for unprocessed images
  
  # Change file types and directory path
  new_names <- gsub(x = files, pattern = "\\.HEIC$", ".jpeg")
  new_names <- gsub(x = new_names, pattern = "raw_img", "www")
  
  # Export new images
  for (i in 1:length(files)) {
    image_write(img[i], path = new_names[i], format = "jpeg", quality = 75)
  }
  
}


