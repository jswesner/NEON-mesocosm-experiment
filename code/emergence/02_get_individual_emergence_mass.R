library(tidyverse)
library(brms)
library(tidybayes)

dw_raw_adults <- read_csv("data/dw_raw_adults.csv")

dw_raw_adults %>% 
  ggplot(aes(x = tank, y = dw_mg)) + 
  geom_jitter(width = 0.2) +
  facet_wrap(temp_treat ~ taxa, scales = "free_x")


dw_raw_adults %>% 
  filter(grepl("Chiro", taxa)) %>% 
  group_by(tank) %>% 
  arrange(dw_mg) %>% 
  mutate(order = row_number()) %>% 
  ggplot(aes(x = order, y = dw_mg)) +
  geom_point(aes(color = treatment)) +
  scale_y_log10() +
  facet_wrap(~treatment)



dw_raw_adults %>% 
  filter(grepl("Chiro", taxa)) %>% 
  ggplot(aes(x = dw_mg, fill = treatment, group = tank)) +
  geom_density() +
  facet_wrap(~treatment, scales = "free_y", ncol = 1) +
  scale_x_log10()
