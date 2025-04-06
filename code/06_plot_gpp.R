library(tidybayes)
library(tidyverse)
library(lubridate)

treatments = readRDS(file = "data/treatments.rds")
brm_gpp = readRDS("models/brm_gpp.rds") 
gpp_wrangled = readRDS(file = "data/gpp_wrangled.rds") %>% 
  mutate(mean_gpp = mean(gpp, na.rm = T),
         gpp_s = gpp/mean_gpp) %>% 
  mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) %>% 
  mutate(day_of_experiment = paste0("Day ", day_of_experiment)) 

posts_gpp = brm_gpp$data %>% distinct(temp_treat, nutrient_treat, time) %>% 
  left_join(treatments) %>% 
  mutate(mean_gpp = brm_gpp$data2$mean_gpp) %>% 
  add_epred_draws(brm_gpp) %>% 
  mutate(.epred = .epred*mean_gpp) %>% 
  left_join(gpp_wrangled %>% distinct(time, date, day_of_experiment))

raw_gpp = brm_gpp$data %>% mutate(gpp = gpp_s*brm_gpp$data2$mean_gpp) %>% 
  left_join(treatments) %>% 
  left_join(gpp_wrangled %>% distinct(time, date, day_of_experiment))

plot_epreds_gpp = posts_gpp %>% 
  ggplot(aes(x = nut_plus, y = .epred)) +
  geom_point(data = raw_gpp,
             aes(y = gpp),
             shape = 1,
             size = 0.5
             # aes(color = nut_plus)
  ) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  facet_grid(day_of_experiment~temp_plus) +
  geom_line(data = posts_gpp %>% filter(.draw <= 1000),
            aes(group = .draw, color = nut_plus),
            alpha =0.01) +
  stat_pointinterval(aes(color = nut_plus), alpha = 0.5) +
  guides(color = "none") +
  labs(x = "Nutrients",
       y = "Gross Primary Production") +
  NULL

ggsave(plot_epreds_gpp, 
       file = "plots/plot_epreds_gpp.jpg", width = 6, height = 4, dpi = 400)

posts_gpp %>% 
  ungroup %>% 
  select(-.iteration, -.row, -.chain) %>% 
  select(temp_nut_plus, .draw, .epred, time) %>% 
  pivot_wider(names_from = temp_nut_plus, values_from = .epred) %>% 
  group_by(time) %>% 
  mutate(diff = `+heat | +nutrients` - `+heat | -nutrients`) %>% 
  # median_qi(diff)
  reframe(prob = sum(diff > 0)/max(.draw))
  
gpp_table = posts_gpp %>% 
  group_by(day_of_experiment, temp_nut_plus) %>% 
  median_qi(.epred) %>% 
  ungroup %>% 
  select(-.width, -.point, -.interval)

saveRDS(gpp_table, file = "tables/gpp_table.rds")
