library(tidybayes)
library(tidyverse)
library(lubridate)
library(ggthemes)

treatments = readRDS(file = "data/treatments.rds")
brm_gpp = readRDS("models/brm_gpp.rds") 


gpp_wrangled = readRDS(file = "data/gpp_wrangled.rds") %>% 
  mutate(mean_gpp = mean(gpp, na.rm = T),
         gpp_s = gpp/mean_gpp) %>% 
  mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) %>% 
  mutate(day_of_experiment = paste0("Day ", day_of_experiment)) 


gpp_table = gpp_wrangled |> 
  group_by(temp_treat) |> 
  mutate(min = min(gpp),
         max = max(gpp)) |> 
  group_by(temp_treat, min, max) |> 
  median_qi(gpp)


saveRDS(gpp_table, file = "tables/gpp_table.rds")
write_csv(gpp_table, file = 'tables/gpp_table.csv')
