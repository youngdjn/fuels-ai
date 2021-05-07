library(tidyverse)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))


















#### old code
library(tidyverse)

d = read_csv("data/springs_plot.csv")

photo_dirs = c("N","S","E","W")

d_foc = d %>%
  select(plotid,tos) %>%
  slice(-1) %>%
  filter(plotid != "springs21") %>%
  mutate(tos = ifelse(tos=="tr",0.5,tos) %>% as.numeric) %>%
  inner_join(photo_dirs, copy=TRUE, by=character()) %>%
  rename("photo_dir" = "y") %>%
  mutate(photo_file = paste0(plotid,"-",photo_dir,".jpg")) %>%
  select(-photo_dir,-plotid) %>%
  filter(!is.na(tos)) %>%
  select(photo_file,everything())

write_csv(d_foc,"springs_shrub.csv")
