library(poweRlaw)
library(tidyverse)

# load data
dat = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  filter(correct_taxon != "Daphnia") %>%
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) 


# cull with fixed length via Morin 2004 -----------------------------------
# filter all lengths that are less than 10x the sieve diameter (250um)
# i.e., remove all bugs < 2.5 mm
treatments = readRDS("data/treatments.rds")

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


# same but with emergers --------------------------------------------------

dw_raw_adults = read_csv("data/dw_raw_adults.csv") %>% select(tank, temp_treat, nutrient_treat, dw_mg, m2) %>% 
  mutate(stage = "adult")

dat_emerge_ind = bind_rows(dw_raw_adults, dat_morin_xmins|> mutate(stage = "larval")) |> 
  group_by(tank, temp_treat, dw_mg, m2, stage) %>% 
  tally() %>% 
  mutate(no_m2 = n/m2) |> 
  ungroup() |> 
  left_join(gpp_wrangled)

saveRDS(dat_emerge_ind, file = "data/dat_emerge_ind.rds")

dat_emerge_ind |> 
  arrange(tank, -dw_mg) |>
  # filter(dw_mg >= min(dat_morin_xmins$xmin)) |> 
  group_by(tank) |> 
  mutate(order = 1:max(row_number())) |> 
  ggplot(aes(color = stage, x = dw_mg, y = order)) + 
  geom_point(size = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~tank)



