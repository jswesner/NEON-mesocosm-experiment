library(tidyverse)
library(janitor)
library(lubridate)
library(ggthemes)

# assign treatments
tank_row = tibble(tank = 1:24,
               row = rep(1:4, each = 6)) 

tank_randomized = tibble(tank = sample(1:24)) %>% 
  left_join(tank_row)
  
treatments <- tibble(temp_treat = rep(c("ambient", "heated"), 12),
                     nutrient_treat = rep(c("ambient","ambient", "nutrient", "nutrient"), 6)) %>% 
  mutate(tank = 1:24,
         row = rep(1:4, each = 6)) %>% 
  select(row, tank, everything()) %>% 
  mutate(target_temp_f = case_when(temp_treat == "ambient" ~ 80.6, TRUE ~ 87.8)) # 4 celsius target warming is same as 7 degree F above ambient

# temperature target
# To keep F temp ~ 7 degrees warmer


# nutrients to add
# 35% of ammonium nitrate is N, all available to algae: https://modcalculator.com/molarmasscalculator?x1=NH4NO3, 
# 18.9% of sodium phosphate is P: https://modcalculator.com/molar-mass-of/Na3PO4
# 23% of Nitrate is N and 33% of phosphate is P
# mesocosm water is ~ 1mg/L Nitrate and ~ 0.7 mg/L Phosphate, which yields ~ 1*0.23 = 0.23 mg/L N and 0.7*0.33 = 0.23 mg/L P
# mesocosm water is 1:1 ratio of N:P
# target a 1:1 ratio with doubling of amount
nutrient_additions <- tibble(day = c(1, 8, 11, 15, 18, 22, 25),
       fraction = c(1, rep(0.1, 6)),
       onetoone_ammoniumnitrate_mgL = (0.23*2)/0.23, # doubles background concentration at varying ratios
       onetoone_sodiumphosphate_mgL = (0.7*2)/0.33,
       fivetoone_ammoniumnitrate_mgL = (0.23*10)/0.23,
       fivetoone_sodiumphosphate_mgL = (0.7*2)/0.33,
       tentoone_ammoniumnitrate_mgL = (0.23*20)/0.23,
       tentoone_sodiumphosphate_mgL = (0.7*2)/0.33) %>% 
  pivot_longer(cols = c(-fraction,-day), values_to = "mgL_molecule_target") %>% 
  mutate(tank_volume = 0.33*1136,
         g_molecule_toaddpertank = tank_volume*mgL_molecule_target*fraction/1000,
         background_mgL_NorP = case_when(grepl("nitrate", name) ~ 1, TRUE ~ 0.7),
         target_mgL_NorP = 2*background_mgL_NorP) %>% 
  separate(name, c("ratio", "molecule", NA)) %>% 
  mutate(element = case_when(grepl("nitrate", molecule) ~ "N", TRUE ~ "P"))


onetoone_additions <- nutrient_additions %>% 
  filter(ratio == "onetoone") %>% 
  select(day, g_molecule_toaddpertank, element, ratio) %>% 
  pivot_wider(names_from = "element", values_from = "g_molecule_toaddpertank") %>% 
  rename(grams_amon_nitrate_toadd_pertank = N,
         grams_sod_phosp_toadd_pertank = P)

treatments %>% 
  expand_grid(onetoone_additions) %>% 
  arrange(day, tank) %>% 
  mutate(correct = case_when(nutrient_treat == "ambient" ~ 0, TRUE ~ 1),
         grams_amon_nitrate_toadd_pertank = round(grams_amon_nitrate_toadd_pertank*correct,2),
         grams_sod_phosp_toadd_pertank = round(grams_sod_phosp_toadd_pertank*correct, 2)) %>% 
  write_csv("data/treatments.csv") %>% 
  filter(day == 1) %>% 
  filter(temp_treat != "heated" & nutrient_treat != "ambient") %>% 
  print(n = Inf)

