library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())

make_isd_plots = function(model){
  isd_preds_bytank = model$data %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg), xmax = max(dw_mg)) %>%
  distinct(temp_treat, nutrient_treat, xmin, xmax, tank) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(model, re_formula = NULL)

isd_lines_bytank = isd_preds_bytank %>% 
  group_by(temp_treat, nutrient_treat, xmin, xmax, tank) %>% 
  median_qi(.epred) %>%
  select(-.width, -.point, -.interval) %>% 
  pivot_longer(cols = starts_with("."),
               values_to = "lambda") %>% 
  expand_grid(interval = 1:30) %>% 
  group_by(name, temp_treat, nutrient_treat, tank) %>% 
  # mutate(x = seq(min(xmin), max(xmax), length.out = max(interval))) %>%
  mutate(x = xmin + (xmax - xmin) * (seq(0, 1, length.out = max(interval)))^10) %>%
  mutate(prob_y = pparetocounts(x = x, xmin = xmin, xmax = xmax, lambda = lambda)) %>%
  ungroup %>% 
  left_join(dw_fixed %>% group_by(tank) %>% reframe(max_y = max(row_number()))) %>% 
  left_join(renumber)

raw_data = model$data %>% 
  arrange(tank, -dw_mg) %>% 
  group_by(tank) %>% 
  mutate(y = row_number()/max(row_number())) %>% 
  left_join(renumber)

isd_lines_bytank %>% 
  select(-interval, -lambda) %>% 
  pivot_wider(names_from = name, values_from = prob_y) %>% 
  mutate(treatment = paste(temp_treat, nutrient_treat, sep = '_')) %>% 
  ggplot(aes(x = x, y = .epred)) +
  geom_point(data = raw_data, aes(x = dw_mg, y = y, color = temp_nut_plus),
             shape = 1, alpha = 0.3, 
             size = 0.6) +
  geom_lineribbon(aes(group = interaction(temp_treat, nutrient_treat, tank),
                      ymin = .lower, ymax = .upper), 
                  linewidth = 0.2, alpha = 0.9) +
  ggh4x::facet_grid2(temp_nut_plus~new_tank) +
  scale_x_log10() +
  # scale_y_log10() +
  # coord_cartesian(ylim = c(0.5e-02, NA)) +
  scale_color_colorblind() +
  guides(color = "none") +
  NULL
}


