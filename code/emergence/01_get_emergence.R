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
         g_day = dry_bugs_only_g/trap_days,
         julian = julian(date),
         min_julian = min(julian(date)),
         julian_scaled = julian - min_julian,
         julian_s = scale(julian),
         g_day01 = g_day + 0.01)

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
