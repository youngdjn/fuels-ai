library(tidyverse)
library(here)
library(magick)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))

imgs = list.files(datadir("grupenhoff_photos_renamed"),full.names = TRUE)

size = 512
dir = "photos_center_512/"
if(!dir.exists(datadir(dir))) dir.create(datadir(dir))

for(i in 1:length(imgs)) {

  filename = basename(imgs[i])
  if(file.exists(paste0(datadir(dir),filename))) {
    cat("already exists\n")
    next()
  }
  
  #cat("running for",filename,"\n")
  
  img = image_read(imgs[i])

  
  ### get image dims
  info = image_info(img)
  width = info$width
  height = info$height
  
  
  if(width > height) {
  
  ## get a square in the middle
  left_offset = (width-height)/2
  crop_string = paste0(height,"x",height,"+",left_offset)   
  
  # # to make a ground photo
  # top_offset = height/2
  # height = height/2
  # left_offset = (width-height)/2
  # crop_string = paste0(height,"x",height,"+",left_offset,"+",top_offset)

  } else {
    
    ## portrait
    
    ## get a square in the middle
    top_offset = (height-width)/2
    crop_string = paste0(width,"x",width,"+0+",top_offset)   

    # # to make a ground photo
    # top_offset = height/2
    # height = height/2
    # left_offset = (width-height)/2
    # crop_string = paste0(height,"x",height,"+",left_offset,"+",top_offset)
    
  }
    
  
  img = image_crop(img,crop_string)

  scale_string = paste0("x",size)
  img = image_scale(img,scale_string)
  
  img = image_orient(img)
  
  ### get image dims
  info = image_info(img)
  width = info$width
  height = info$height
  
  if(width != size | height != size) {
    cat("Img",filename,"not square but",width,"x",height,"\n")
  }

  image_write(img,paste0(datadir(dir),filename))
  
}
