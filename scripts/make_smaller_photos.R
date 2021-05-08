library(tidyverse)
library(here)
library(magick)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))

imgs = list.files(datadir("grupenhoff_photos_renamed"),full.names = TRUE)

dir = "photos_lowcenter_512/"
if(!dir.exists(datadir(dir))) dir.create(datadir(dir))

for(i in 1:length(imgs)) {

  filename = basename(imgs[i])
  if(file.exists(paste0(datadir(dir),filename))) {
    cat("already exists\n")
    next()
  }
  
  cat("running for",filename,"\n")
  
  img = image_read(imgs[i])

  
  ### get image dims
  info = image_info(img)
  width = info$width
  height = info$height
  
  ## get a square in the middle
  left_offset = (width-height)/2
  
  # to make a ground photo
  top_offset = height/2
  height = height/2
  left_offset = (width-height)/2
  
  
  crop_string = paste0(height,"x",height,"+",left_offset,"+",top_offset)
  
  img = image_crop(img,crop_string)

  scale_string = paste0("x",512)
  img = image_scale(img,scale_string)
  

  image_write(img,paste0(datadir(dir),filename))
  
}
