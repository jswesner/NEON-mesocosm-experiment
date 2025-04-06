library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())

# load model
fit_isd = readRDS("models/fit_isd_clauset.rds")
# load raw (unculled) data
dat = readRDS(file = "data/dw_fixed.rds") %>% ungroup


treatments = fit_isd$data %>% distinct(temp_treat, nutrient_treat) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

# simulate body sizes

sim_individuals = fit_isd$data %>% 
  # group_by(tank) %>%
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  select(-dw_mg, -counts) %>% 
  distinct() %>% 
  mutate(counts = 1) %>% 
  add_predicted_draws(fit_isd, ndraws = 1000)

gm_size = sim_individuals %>% 
  group_by(tank, nutrient_treat, temp_treat) %>% 
  reframe(gm = exp(mean(log(.prediction))))

sim_individuals %>% 
  left_join(gm_size) %>% 
  mutate(max = max(.prediction)) %>% 
  ggplot(aes(x = reorder(tank, gm), y = .prediction, color = nutrient_treat)) + 
  geom_jitter(width = 0.2, shape = ".") +
  scale_y_log10() +
  geom_point(aes(y = gm), color = "black")


dat %>% 
  group_by(tank, nutrient_treat, temp_treat) %>% 
  mutate(gm = exp(mean(log(dw_mg)))) %>% 
  ggplot(aes(x = reorder(tank, gm), y = dw_mg, color = temp_treat)) + 
  geom_jitter(width = 0.2, shape = ".") +
  scale_y_log10() +
  geom_point(aes(y = gm), color = "black")
