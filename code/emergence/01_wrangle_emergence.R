library(tidyverse)
library(lubridate)
library(ggthemes)
library(janitor)
library(brms)

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(temp_treat, "_",nutrient_treat))

emergence <- read_csv("data/emergence.csv") %>% 
  left_join(treatments) %>% 
  mutate(date = mdy(date_collected),
         g_day = dry_bugs_only_g/trap_days) %>% 
  mutate(g_day_s = g_day/mean(g_day, na.rm = T) + 0.01,
         day = julian(date),
         day_s = scale(day)) %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  mutate(mean_individual_mgdw = dry_bugs_only_g/no_ind,
         mean_individual_mgdw_s1 = (mean_individual_mgdw/mean(mean_individual_mgdw, na.rm = T))) %>% 
  mutate(day = day - day_1) %>% 
  mutate(dragons = case_when(grepl("dragon", notes) ~ 1, TRUE ~ 0)) %>% 
  mutate(dragons = case_when(grepl("2 dragons", notes) ~ 2, TRUE ~ dragons),
         dragon_mass = dragons*0.076) %>% # min mass of the two dragons measured on 2/28/2025 (0.076 and 0.079 gDM)
  mutate(dragon_free_g_day = (dry_bugs_only_g - dragon_mass)/trap_days,
         dragon_free_g_day_s = dragon_free_g_day/mean(dragon_free_g_day, na.rm = T) + 0.01,
         no_ind_s = no_ind/mean(no_ind, na.rm = T) + 0.01,
         no_ind_perday = no_ind/trap_days) %>% 
  ungroup %>% 
  mutate(no_ind_perday_s = no_ind_perday/mean(no_ind_perday, na.rm = T) + 1)

saveRDS(emergence, file = "data/emergence_wrangled.rds")

emergence %>% 
  ggplot(aes(x = day, y = no_ind_perday)) + 
  geom_point() +
  facet_wrap(~treatment) +
  geom_smooth()

emergence %>% 
  ggplot(aes(x = dry_bugs_only_g, y = no_ind)) + 
  geom_point() + 
  # scale_x_log10() + 
  scale_y_log10() +
  geom_smooth(method = "lm")

emergence %>%
  ggplot(aes(x = julian_s, y = g_day, color = treatment)) + 
  geom_point() + 
  # scale_x_log10() +
  scale_y_log10() +
  geom_line(aes(group = tank)) +
  facet_wrap(~treatment)


# model
emerge_brm = brm(g_day01 ~ s(julian_scaled, by = treatment, k = 6) + (1|treatment),
                 data = emergence, family = Gamma(link = "log"),
                 iter = 1000, chains = 1)
  

conds = plot(conditional_effects(emerge_brm), points = T)        

conds$`julian_scaled:treatment` +
  scale_y_log10() +
  facet_wrap(~treatment)


# daily emergence
library(tidybayes)

posts = tibble(julian_scaled = 0:max(emerge_brm$data$julian_scaled) -  
                           min(emerge_brm$data$julian_scaled)) %>% 
  expand_grid(treatment = unique(emerge_brm$data$treatment)) %>% 
  add_epred_draws(emerge_brm)

# total emergence
posts %>% 
  group_by(treatment, .draw) %>% 
  reframe(total_emerge = sum(.epred)) %>% 
  ggplot(aes(x = total_emerge, y = treatment)) + 
  stat_halfeye()

# difference between heated and ambient
posts %>% 
  separate(treatment, into = c("heat", "nutrients")) %>% 
  group_by(heat, .draw, julian_scaled) %>% 
  reframe(.epred = mean(.epred)) %>% 
  group_by(heat, .draw) %>% 
  reframe(total_emerge = sum(.epred)) %>%
  ggplot(aes(x = total_emerge, y = heat)) + 
  stat_halfeye() 
  
# difference between heated and ambient
posts %>% 
  separate(treatment, into = c("heat", "nutrients")) %>% 
  group_by(nutrients, .draw, julian_scaled) %>% 
  reframe(.epred = mean(.epred)) %>% 
  group_by(nutrients, .draw) %>% 
  reframe(total_emerge = sum(.epred)) %>%
  ggplot(aes(x = total_emerge, y = nutrients)) + 
  stat_halfeye() 

# total emergence in the last 5 days.

posts %>% 
  filter(julian_scaled >= 20) %>% 
  group_by(treatment, .draw) %>% 
  reframe(total_emerge = sum(.epred)) %>% 
  group_by(treatment) %>% 
  ggplot(aes(x = total_emerge, y = treatment)) + 
  stat_halfeye()
