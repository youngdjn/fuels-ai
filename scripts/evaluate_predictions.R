library(tidyverse)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))


## open all the results

f = list.files(datadir("cnn_preds"),full.names = TRUE)
f_fuel = f[grepl("preds_fuel",f)]
f_plot = f[!grepl("preds_fuel",f)]

a_fuel = map(f_fuel,read_csv)
a_fuel = bind_cols(a_fuel)

alldata = data.frame()

for(i in 1:length(f)) {
  
  file = f[i]
  d = read_csv(file)
  d = d[,2:ncol(d)]
  d$varname = colnames(d)[1]
  d = d %>%
    rename("pred" = 1)
  
  alldata = bind_rows(alldata,d)
}
  
a_fuel = alldata %>%
  filter(str_starts(varname,"pred_fuel")) %>%
  pivot_wider(names_from=varname,values_from=pred, values_fn=list(pred=mean))

a_plot = alldata %>%
  filter(!str_starts(varname,"pred_fuel")) %>%
  select(!cardinaldir) %>%
  group_by(plot_id_std,varname) %>%
  mutate(id=row_number()) %>%
  ungroup() %>%
  pivot_wider(id_cols=c(id,plot_id_std) ,names_from=varname,values_from=pred)

d_plot = read_csv(datadir("data_prepped/plotdata_photo_dat.csv"))
plotdat = left_join(a_plot,d_plot)

d_fuel = read_csv(datadir("data_prepped/fueltransect_photo_dat.csv"))
fueldat = left_join(a_fuel,d_fuel)


write_csv(plotdat,datadir("data_w_preds/plotdata.csv"))
write_csv(fueldat,datadir("data_w_preds/fueldata.csv"))


# > names(d)
# [1] "id"                   "plot_id_std"          "pred_cwd_center"      "pred_cwd_lowcenter"   "pred_litter_center"  
# [6] "pred_ntrees_center"   "pred_rock_center"     "pred_shrub_center"    "pred_shrub_lowcenter" "pred_shrubht_center" 
# [11] "cardinaldir"          "photo_name"           "new_filename"         "TOS_percent"          "TOS_HT_m"            
# [16] "BARESOIL"             "LITTER"               "ROCK"                 "CWD"                  "n_trees"             
# [21] "natester"             "is_valid"  

# > names(d)
# [1] "plot_id_std"               "cardinaldir"               "pred_fuel100h_center"      "pred_fuel1h_center"       
# [5] "pred_fuel1h_lowcenter"     "pred_fuellitter_center"    "pred_fuellitter_lowcenter" "photo_name"               
# [9] "new_filename"              "ct1hr"                     "ct10hr"                    "ct100hr"                  
# [13] "litter"                    "natester"                  "is_valid"  



# shrub cover

plot_fit(title = "Number of trees (plot mean)",
         xvar = "n_trees",
         yvar = "pred_ntrees_center",
         plot_mean = TRUE)




plot_fit = function(title,xvar,yvar,plot_mean = FALSE) {
  
  d = plotdat
  
  if(plot_mean) {
  d = d %>%
    group_by(plot_id_std) %>%
    summarize_all(mean)
  }
  
  r = cor(d[,xvar],d[,yvar])
  r_text = paste0("r = ",r %>% round(2))
  
  p = ggplot(d,aes( x = !!as.name(xvar),
                y = !!as.name(yvar))) +
    geom_point(color="aquamarine4") +
    annotate("text",label=r_text,x=-Inf,y=Inf,size=5, hjust=-0.2,vjust=1.5) +
    #geom_abline(a=0,b=1) +
    geom_smooth(method="lm", color="grey40") +
    labs(x="Observed",y="Predicted", title = title) +
    theme_bw(15) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  print(p)
  
  png(datadir(paste0("figures/",title,".png")), res=250, width=1000, height=1000)
  print(p)
  dev.off()

}




