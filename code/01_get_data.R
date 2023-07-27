library(tidyverse)
library(rio)
library(lubridate)
library(ggthemes)
library(janitor)


# Load data ---------------------------------------------------------------
#1) load treatment data
treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(temp_treat, "_",nutrient_treat))

#2) load temperature data
temperature <- read_csv("data/temperature.csv") %>% 
  mutate(date = mdy(date), # get proper date format
         temp_deg_c = (temp_deg_f - 32)*(5/9), # convert celsius to Fahrenheit
         date_time = parse_date_time(paste(date, time), orders = "Ymd HMS")) %>% # merge dates/times
  filter(!is.na(temp_deg_c)) %>% 
  select(-heater) # remove heater column (don't need it)

#3) combine temp and treatment data, then get mean temps
treatments_temperature =  left_join(temperature, treatments) %>%  # add treatment info to temperature measures
  group_by(tank, temp_treat, nutrient_treat, treatment) %>% 
  reframe(mean_temp = mean(temp_deg_c, na.rm = T),
          sd_temp = sd(temp_deg_c, na.rm = T),
          cv_temp = sd(temp_deg_c, na.rm = T) / mean(temp_deg_c, na.rm = T) * 100)

#4) load body size data

lengths_raw <- import_list("data/body size data.xlsx", setclass = "tbl") %>%  # load all body size tabs
  bind_rows()

#5) load data to correct body size taxa
taxon_fixes = read_csv("data/taxon_fixes.csv") %>% 
  distinct(taxon, correct_taxon, correct_stage)

#6) fix taxa, add treatment info
lengths_fixed = lengths_raw %>% 
  left_join(taxon_fixes) %>% 
  left_join(treatments_temperature) %>% 
  filter(!is.na(length_mm))

#7) Save
write_csv(lengths_fixed, file = "data/lengths_fixed.csv")
  

