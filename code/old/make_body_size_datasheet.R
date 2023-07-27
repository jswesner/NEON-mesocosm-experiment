library(tidyverse)

example_length_data = tibble(taxon = c("Chironomidae", "Ephemeroptera", "Odonata", "Coleoptera")) %>% 
  expand_grid(tank = 1:3) %>% 
  expand_grid(individual = 1:20) %>% 
  mutate(experiment = "NEON_mesocosm_2022",
         length_mm = round(rexp(nrow(.), 4), 2),
         lengt_mm = length_mm + 0.4) %>% 
  arrange(tank, length_mm) %>% 
  mutate(individual = 1:nrow(.),
         date = as.Date("2022-06-30")) %>% 
  select(experiment, tank, date, taxon, individual, length_mm) %>% 
  write_csv("data/example_length_data.csv")

