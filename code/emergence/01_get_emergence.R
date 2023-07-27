library(tidyverse)
library(lubridate)
library(ggthemes)
library(janitor)

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(temp_treat, "_",nutrient_treat))

emergence <- read_csv("data/emergence.csv") %>% 
  left_join(treatments) %>% 
  mutate(date = mdy(date_collected),
         g_day = dry_bugs_only_g/trap_days)

emergence %>% 
  ggplot(aes(x = dry_bugs_only_g, y = no_ind)) + 
  geom_point() + 
  # scale_x_log10() + 
  scale_y_log10() +
  geom_smooth(method = "lm")

emergence %>%
  ggplot(aes(x = date, y = dry_bugs_only_g, color = treatment)) + 
  geom_point() + 
  # scale_x_log10() + 
  # scale_y_log10() +
  geom_line(aes(group = tank))



          