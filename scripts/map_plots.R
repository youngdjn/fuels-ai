library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))


locs = read_csv(datadir("grupenhoff_plot_data_orig/HolyGrail_trt_utm.csv"))

locs = locs %>%
  mutate(utmzone = str_sub(`UTM Zone`,1,2))

locs_utm11 = locs %>%
  filter(X > 435449.1) %>%
  filter(!(is.na(X) | is.na(Y)))

locs_utm10 = locs %>%
  filter(X < 435449.1)

locs11 = st_as_sf(locs_utm11,coords=c("X","Y"), crs="32611")
st_crs(locs11) = "32611"
locs10 = st_as_sf(locs_utm10,coords=c("X","Y"), crs="32610")
st_crs(locs10) = "32610"

locs = bind_rows(locs11,locs10)
