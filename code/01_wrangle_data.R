library(tidyverse)
library(rio)
library(lubridate)
library(ggthemes)
library(janitor)


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
treatments_temperature = left_join(temperature, treatments) %>%  # add treatment info to temperature measures
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
  
library(tidyverse)

#8) get length-mass coefficients
macro_lw_coeffs = read_csv("data/macro_lw_coeffs.csv") # published length-weight coefficients

#9) convert mm length to mg mass
dw_raw = lengths_fixed %>% 
  filter(correct_stage != "adult") %>% 
  mutate(correct_taxon = str_to_sentence(correct_taxon)) %>%
  left_join(macro_lw_coeffs) %>% 
  mutate(dw_mg = case_when(correct_taxon != "Daphnia" ~ a*length_mm^b,
                           TRUE ~ 10^(a + b*log10(length_mm)))) 

write_csv(dw_raw, file = "data/dw_raw.csv")


# check and correct data --------------------------------------------------

# plot the data to check for errors (repeat with different x and y variables)

dw_raw %>% 
  ggplot(aes(x = tank, y = dw_mg)) +
  geom_jitter() +
  scale_y_log10()

dw_raw %>% 
  # filter(dw_mg <= 1000) %>% 
  ggplot(aes(x = length_mm)) + 
  geom_histogram() +
  scale_x_log10()

# filter obvious outliers (anything else to add?)
dw_fixed = dw_raw %>% 
  filter(length_mm < 50) %>%
  filter(dw_mg < 200) %>%
  group_by(tank) %>%
  mutate(xmin = min(dw_mg, na.rm = T),
         xmax = max(dw_mg, na.rm = T),
         counts = 1) %>% 
  mutate(correct_taxon = case_when(correct_taxon == "Dytiscidae" ~ "Coleoptera",
                                   correct_taxon == "Ceratopogonidae" ~ "Diptera",
                                   TRUE ~ correct_taxon)) %>% 
  filter(correct_taxon != "Trichoptera") |>  # there was only one identified. I checked pics on 4/6/2025. No trichoptera were in the samples. Don't know which one this was, so just removed it
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  filter(correct_taxon != "Daphnia") %>%
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) 


write_csv(dw_fixed, file = "data/dw_fixed.csv")
saveRDS(dw_fixed, file = "data/dw_fixed.rds")


# cull xmin ---------------------------------------------------------------

library(poweRlaw)

# load data
dat = read_csv("data/dw_fixed.csv") 

# cull with fixed length via Morin 2004 -----------------------------------
# filter all lengths that are less than 10x the sieve diameter (250um)
# i.e., remove all bugs < 2.5 mm
treatments = readRDS("data/treatments.rds")

# wrangle GPP
gpp = lapply(readRDS("data/gppPosteriors.rds"), function(x) x$summary) %>% bind_rows() %>% 
  separate(tank, into = c("tank", "time")) %>% 
  mutate(tank = parse_number(tank),
         date = case_when(time == 1 ~ "2022-06-22", T ~ "2022-06-28")) %>% 
  clean_names() %>% 
  left_join(readRDS(file = "data/dat_clauset_xmins.rds") %>% distinct(tank, nutrient_treat, temp_treat)) %>% 
  as_tibble()

saveRDS(gpp, file = "data/gpp_wrangled.rds")

gpp_long = readRDS(file = "data/gpp_wrangled.rds") |> 
  left_join(treatments) %>% 
  mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) %>% 
  mutate(day_of_experiment = paste0("Day ", day_of_experiment)) 

gpp_wrangled = gpp_long %>% 
  distinct(tank, gpp, time) |>  
  mutate(date_code = case_when(time == "1" ~ "june_22", T ~ "june_28")) %>% 
  select(tank, date_code, gpp) %>%
  group_by(date_code) |> 
  mutate(gpp_s = scale(gpp)) |> 
  select(-gpp) |> 
  pivot_longer(cols = c(-tank, -date_code)) %>% 
  unite(date_name, c(date_code, name)) %>% 
  pivot_wider(names_from = date_name, values_from = value)

dat_morin_xmins = dat %>% 
  filter(length_mm >= 2.5) %>%
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg),
         source = "morin_xmins") %>% 
  left_join(gpp_wrangled)

saveRDS(dat_morin_xmins, file = "data/dat_morin_xmins.rds")
