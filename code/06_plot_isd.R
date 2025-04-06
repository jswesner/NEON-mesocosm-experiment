library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())

# load model
fit_isd = readRDS("models/fit_isd_clauset.rds")

treatments = fit_isd$data %>% distinct(temp_treat, nutrient_treat) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

saveRDS(treatments, file = "data/treatments.rds")

# posts individual tanks ---------------------------------
post_dots = fit_isd$data %>% 
  select(-dw_mg) %>% 
  distinct()  %>% 
  left_join(treatments) %>% 
  add_epred_draws(fit_isd, re_formula = NULL)

# posts treatments ---------------------------------
post_lines = fit_isd$data %>% 
  select(-dw_mg, -tank) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  group_by(temp_treat, nutrient_treat, .draw) %>% 
  reframe(.epred = mean(.epred))  %>% # average over different xmins and xmaxs
  left_join(treatments)

# plots ---------------------------------
# plot_epreds = post_dots %>% 
#   group_by(tank, temp_nut_plus, temp_treat, nutrient_treat, nut_plus, temp_plus) %>% 
#   median_qi(.epred) %>% 
#   ggplot(aes(x = temp_plus, y = .epred)) +
#   geom_point(shape = 1,
#     # aes(color = nut_plus)
#              ) +
#   stat_pointinterval(data = post_lines, aes(color = nut_plus)) +
#   scale_color_colorblind() +
#   scale_fill_colorblind() +
#   facet_wrap(~nut_plus) +
#   geom_line(data = post_lines %>% filter(.draw <= 1000),
#             aes(group = .draw, color = nut_plus),
#             alpha =0.03) +
#   guides(color = "none") +
#   labs(x = "Temperature",
#        y = "\u03bb") +
#   NULL

plot_epreds = post_dots %>%
  group_by(tank, temp_nut_plus, temp_treat, nutrient_treat, nut_plus, temp_plus) %>%
  median_qi(.epred) %>%
  ggplot(aes(x = nut_plus, y = .epred)) +
  geom_point(shape = 1,
             size = 0.5
             # aes(color = nut_plus)
  ) +
  stat_pointinterval(data = post_lines, aes(color = nut_plus), alpha = 0.5) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  facet_wrap(~temp_plus) +
  geom_line(data = post_lines %>% filter(.draw <= 1000),
            aes(group = .draw, color = nut_plus),
            alpha =0.01) +
  guides(color = "none") +
  labs(x = "Nutrients",
       y = "\u03bb") +
  NULL

ggsave(plot_epreds, file = "plots/plot_epreds.jpg", width = 6, height = 4, dpi = 400)


# summaries ---------------------------------------------------------------

post_dots %>% 
  group_by(tank, temp_treat, nutrient_treat) %>% 
  median_qi(.epred) %>% 
  filter(.epred == max(.epred)|.epred == min(.epred))

post_lines %>% 
  median_qi(.epred)

post_lines %>% 
  group_by(temp_treat, nutrient_treat) %>% 
  median_qi(.epred)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(temp_treat) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(temp_treat) %>% 
  reframe(prob_greater = sum(diff >0)/max(.draw))

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  select(-ambient, -nutrient) %>% 
  pivot_wider(names_from = temp_treat, values_from = diff) %>% 
  mutate(diff_interaction = heated - ambient) %>% 
  median_qi(diff_interaction)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  select(-ambient, -nutrient) %>% 
  pivot_wider(names_from = temp_treat, values_from = diff) %>% 
  mutate(diff_interaction = heated - ambient) %>% 
  reframe(prob_greater = sum(diff_interaction > 0)/max(.draw))

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  group_by(nutrient_treat) %>% 
  median_qi(diff)
