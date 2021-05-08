library(tidyverse)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))


#### Make a data frame of plot data, with plot IDs to match the photo names ####

pd = read_csv(datadir("grupenhoff_plot_data_orig/HolyGrail_Description.csv"))

#standardize plot ID
plots = pd %>%
  mutate(plot_id_std = str_to_upper(plot_id)) %>%
  mutate(plot_id_std = str_replace(plot_id_std,fixed("FM10-"),replacement = "FMA")) %>%
  mutate(plot_id_std = str_replace(plot_id_std,fixed("FM19-"),replacement = "FMB")) %>%
  filter(!(site == "springsfire" & pre_post_fire == "postfire" & year == "2019")) %>% # springs plotdata: drop 2019 postfire
  filter(!(site == "BearMountain" & pre_post_fire == "postfire" & year == "2019")) %>% # bm plotdta: drop (2019) postfire
  # remove spaces
  mutate(plot_id_std = str_replace(plot_id_std,fixed(" "),"")) %>%
  ## get the plot number part
  mutate(plot_number = str_extract(plot_id_std,"[0-9]+") %>% as.numeric) %>%
  mutate(plot_name = str_extract(plot_id_std,"[A-Z]+")) %>%
  select(plot_id,plot_id_std,plot_name,plot_number,everything()) %>%
  # make plot number a string with padding and make a unique identifier with year_name_number_direction
  mutate(plot_number = str_pad(plot_number,width=4,side="left",pad="0")) %>%
  mutate(plot_id_std = paste(year,plot_name,plot_number,sep="_")) %>%
  mutate(plot_id = toupper(plot_id)) %>%
  mutate(plot_number_numeric = plot_number %>% as.numeric)


#### Make a data frame of photo names and file locations, to match plot data ####

photos = list.files(datadir("grupenhoff_photos_orig_sorted"), recursive=TRUE, full.names = TRUE)
  
photos = data.frame(file_loc = photos)

photos = photos %>%
  mutate(file_loc = as.character(file_loc)) %>%
  mutate(year = str_extract(file_loc,"/[0-9]{4}/") %>% str_sub(2,-2),
         filename = basename(file_loc)) %>%
  mutate(photoname = str_split(filename,fixed(".")) %>% map(1)) %>%
  mutate(photoname = toupper(photoname)) %>%
  mutate(photoname = str_replace(photoname,fixed("FM10-"),replacement = "FMA")) %>%
  mutate(photoname = str_replace(photoname,fixed("FM19-"),replacement = "FMB")) %>%
  mutate(photoname = str_remove_all(photoname,fixed(" ")) %>% str_remove_all(fixed("-"))) %>%
  mutate(photoname = str_replace(photoname,fixed("FRENCHMEADOWS"),"FM")) %>%
  mutate(photoname = str_replace(photoname,fixed("CALAVERAS"),"BIGTREES")) %>%  
  # get the plot number
  mutate(plotnumber = str_extract(photoname,"[0-9]+")) %>%
  mutate(cardinaldir = str_sub(photoname,-1,-1)) %>%
  mutate(cardinaldir = recode(cardinaldir, "E" = "W",
                              "W"= "E",
                              "N"= "S",
                              "S" = "N")) %>%
  mutate(plotname = str_extract(photoname,"^[A-Z]+")) %>%
  # make plot number a string with padding and make a unique identifier with year_name_number_direction
  mutate(plotnumber = str_pad(plotnumber,width=4,side="left",pad="0")) %>%
  mutate(plot_id_std = paste(year,plotname,plotnumber,sep="_"))


#### Rename photos to a standardized ID and copy to a folder where they're all root level  ####

## first keep only photos that match a plot
photos_matching = left_join(photos,plots,by="plot_id_std") %>%
  filter(!is.na(plot_name)) %>%
  mutate(new_filename = paste0(plot_id_std,"_",cardinaldir,".jpg")) %>%
  mutate(new_filepath = datadir(paste0("grupenhoff_photos_renamed/",new_filename)))

for(i in 1:nrow(photos_matching)) {
  photo = photos_matching[i,]
  file.copy(from = photo$file_loc, to = photo$new_filepath)
}


### only keep plot data that matches photos

plots_w_photos = left_join(plots,photos_matching %>% select(plot_id_std),by = "plot_id_std") %>% unique



#### fine fuels: For each transect, get: mean litter depth, total 1 hr, 10 hr, 100 hr

f = read_csv(datadir("grupenhoff_plot_data_orig/HolyGrail_FineFuels.csv"))

f = f %>%
  mutate(plot_id = toupper(plot_id)) %>%
  mutate(plot_number = str_extract(plot_id,"[0-9]+$")) %>%
  mutate(plot_name = str_remove(plot_id,fixed(plot_number))) %>%
  mutate(plot_number = as.numeric(plot_number)) %>%
  mutate(across(c(starts_with("count_x1"),ends_with("h_length_m")),as.numeric),
         across(starts_with("litter"),as.numeric)) %>%
  mutate(ct1hr = count_x1h * (11.3/x1h_length_m)) %>%
  mutate(ct10hr = count_x10h * (11.3/x10h_length_m)) %>%
  mutate(ct100hr = count_x100h * (11.3/x100h_length_m)) %>%
  mutate(litter = (litter1_cm + litter2_cm)/2) %>%
  mutate(cardinaldir = recode(azimuth,"0" = "N","90" = "E", "180" = "S", "270" = "W")) %>%
  select(plot_name,plot_number,year,pre_post_fire,cardinaldir,starts_with("ct1"),litter)


#### for each plot get: shrub cover, CWD cover, litter cover, bare soil

plots_dat = plots_w_photos %>%
  select(plot_id,plot_id_std, year, pre_post_fire, TOS_percent, TOS_HT_m, BARESOIL, LITTER, CWD) %>%
  mutate(plot_number = str_extract(plot_id,"[0-9]+$")) %>%
  mutate(plot_name = str_remove(plot_id,fixed(plot_number))) %>%
  mutate(plot_number = as.numeric(plot_number)) %>%
  select(-plot_id)

#### for each plot, from tree data, get number of trees

trees = read_csv(datadir("grupenhoff_plot_data_orig/HolyGrail_Trees.csv"))

trees_plot = trees %>%
  mutate(plotid = toupper(plotid)) %>%
  filter(status != "stump") %>%
  group_by(year,plotid,pre_post_fire) %>%
  summarize(n_trees = n()) %>%
  ungroup() %>%
  mutate(plot_number = str_extract(plotid,"[0-9]+$")) %>%
  mutate(plot_name = str_remove(plotid,fixed(plot_number))) %>%
  mutate(plot_number = as.numeric(plot_number)) %>%
  dplyr::select(-plotid)

## pull tree count into plot data

plots_dat = left_join(plots_dat,trees_plot,by=c("year","plot_name","plot_number","pre_post_fire"))


#### Make a df that has a row for each photo and pulls in the plot data

fuel_photo_dat = photos_matching %>%
  select(new_filename,plot_id_std,cardinaldir)

## bring in the plot data

fuel_photo_dat = left_join(fuel_photo_dat,plots_dat,by="plot_id_std")

## bring in transect data

fuel_photo_dat = left_join(fuel_photo_dat,f, by=c("plot_name","plot_number","year","pre_post_fire","cardinaldir"))

## standardize the data cols

my_rescale = function(x) {
  scales::rescale(x,to=c(-0.5,0.5))
}

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x,na.rm=TRUE)) /(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))-.5)
}


fuel_photo_dat = fuel_photo_dat %>%
  select(plot_number,plot_name,everything()) %>%
  mutate(across(TOS_percent:litter, as.numeric)) %>%
  mutate(across(TOS_percent:litter, normalize))
  
fueltransect_photo_dat = fuel_photo_dat %>%
  mutate(photo_name = paste(plot_id_std,cardinaldir,sep="_")) %>%
  select(plot_id_std,cardinaldir,photo_name,new_filename,ct1hr:litter) %>%
  mutate(natester = rowSums(across(ct1hr:litter))) %>%
  filter(!is.na(natester)) %>%
  filter(grepl("N.jpg|S.jpg|E.jpg|W.jpg",new_filename))

plotdata_photo_dat = fuel_photo_dat %>%
  mutate(photo_name = paste(plot_id_std,cardinaldir,sep="_")) %>%
  select(plot_id_std,cardinaldir,photo_name,new_filename,TOS_percent:n_trees) %>%
  mutate(natester = rowSums(across(TOS_percent:n_trees))) %>%
  filter(!is.na(natester)) %>%
  filter(grepl("N.jpg|S.jpg|E.jpg|W.jpg",new_filename))

## make a photo-plot crosswalk
xwalk = photos_matching %>%
  select(new_filename,plot_id_std,cardinaldir,year = year.x, pre_post_fire, plot_id)

write_csv(xwalk,datadir("data_prepped/photo_plot_xwalk.csv"))
write_csv(fueltransect_photo_dat,datadir("data_prepped/fueltransect_photo_dat.csv"))
write_csv(plotdata_photo_dat,datadir("data_prepped/plotdata_photo_dat.csv"))

