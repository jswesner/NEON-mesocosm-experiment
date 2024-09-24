library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)

# load model
fit_isd = readRDS("models/fit_isd.rds")

# posts individual tanks ---------------------------------
post_dots = fit_isd$data %>% 
  select(-dw_mg) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NULL)

# posts treatments ---------------------------------
post_tanks = fit_isd$data %>% 
  select(-dw_mg, -tank) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  group_by(temp_treat, nutrient_treat, .draw) %>% 
  reframe(.epred = mean(.epred))  # average over different xmins and xmaxs


# plots ---------------------------------

post_tanks %>% 
  ggplot(aes(x = nutrient_treat, y = .epred, color = temp_treat)) + 
  stat_pointinterval(position = position_dodge(width = 0.2))

post_dots %>% 
  group_by(tank) %>% 
  mutate(median = median(.epred)) %>% 
  ggplot(aes(x = reorder(tank, -median), y = .epred)) + 
  stat_pointinterval()


post_dots %>% 
  group_by(tank) %>% 
  mutate(median = median(.epred)) %>% 
  ggplot(aes(x = temp_treat, y = .epred, color = nutrient_treat, group = tank)) + 
  stat_pointinterval()

post_dots %>% 
  group_by(tank) %>% 
  mutate(median = median(.epred)) %>% 
  ggplot(aes(x = reorder(tank, -median), y = .epred)) + 
  stat_pointinterval()


post_dots %>% 
  group_by(tank) %>% 
  mutate(median = median(.epred)) %>% 
  ggplot(aes(x = temp_treat, y = .epred, color = nutrient_treat, group = tank)) + 
  stat_pointinterval()
