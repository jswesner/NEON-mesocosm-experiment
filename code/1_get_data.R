library(tidyverse)
library(googledrive)
library(lubridate)
library(ggthemes)
library(janitor)

#download latest data
drive_download("NEON mesocosm study 2022/temperature",
               path = "data/temperature.csv",
               overwrite = T)

temperature <- read_csv("data/temperature.csv") %>% 
  mutate(temp_deg_c = (temp_deg_f - 32)*(5/9),
         date_time = parse_date_time(paste(date, time), orders = "Ymd HMS"))

heaters = temperature %>% filter(date == "2022-06-15") %>% 
  distinct(tank, heater)


# Save o2 measures --------------------------------------------------------

temperature %>%
  select(-heater) %>% 
  left_join(heaters) %>% 
  clean_names() %>% 
  filter(!is.na(o2_do_mg_l)) %>% 
  mutate(temp_deg_ysi = case_when(is.na(temp_from_ysi_deg_c) ~ temp_deg_c, TRUE ~ temp_from_ysi_deg_c)) %>% 
  select(tank, date_time, o2_do_mg_l, temp_deg_c) %>% 
  write_csv(file = "data/o2_temp.csv")


# Plot temperature --------------------------------------------------------
temperature %>% 
  select(-heater) %>% 
  left_join(heaters) %>%
  filter(date >= "2022-05-27") %>%
  drop_na(temp_deg_f) %>% 
  drop_na(temp_deg_c) %>% 
  ggplot(aes(x = date_time, y = temp_deg_c, color = heater)) + 
  geom_point() + 
  geom_line(aes(group = tank), alpha = 0.5) +
  scale_color_colorblind() + 
  theme_classic() + 
  # scale_y_log10() +
  NULL

temperature %>%
  filter(date >= "2022-05-27") %>%
  select(-heater) %>% 
  left_join(heaters) %>% 
  group_by(date_time, heater) %>% 
  summarize(mean = mean(temp_deg_c, na.rm = T)) %>% 
  pivot_wider(names_from = heater, values_from = mean) %>% 
  mutate(diff = y - n) %>%
  drop_na(diff) %>% 
  ggplot(aes(x = date_time, y = diff)) +
  geom_line()+
  geom_point() + 
  ylim(-10, 10) + 
  geom_hline(yintercept = 0, color = "dodgerblue") + 
  theme_classic()


temperature %>%
  glimpse() %>% 
  filter(date >= "2022-05-27") %>%
  select(-heater) %>% 
  left_join(heaters) %>% 
  ggplot(aes(x = temp_from_ysi_deg_c, y = temp_deg_c, color = heater)) + 
  geom_point() +
  ylim(0, 40) +
  xlim(0, 40) + 
  geom_abline()



# Plot O2_temp ------------------------------------------------------------

temperature %>%
  select(-heater) %>% 
  left_join(heaters) %>% 
  clean_names() %>% 
  filter(!is.na(o2_do_mg_l)) %>% 
  ggplot(aes(y = o2_do_mg_l, x = date_time)) +
  geom_point() + 
  geom_line(aes(group = tank))

