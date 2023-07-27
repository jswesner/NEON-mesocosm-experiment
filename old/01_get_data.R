library(tidyverse)
library(rio)
library(lubridate)
library(ggthemes)
library(janitor)


# Load data ---------------------------------------------------------------

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(temp_treat, "_",nutrient_treat))

temperature <- read_csv("data/temperature.csv") %>% 
  mutate(date = mdy(date), # get proper date format
         temp_deg_c = (temp_deg_f - 32)*(5/9), # convert celsius to Fahrenheit
         date_time = parse_date_time(paste(date, time), orders = "Ymd HMS")) %>% # merge dates/times
  select(-heater) # remove heater column (don't need it)

treatments_temperature =  left_join(temperature, treatments) # add treatment info to temperature measures

body_sizes <- import_list("data/body size data.xlsx", setclass = "tbl") %>%  # load all body size tabs
  bind_rows() %>% # combine all the loaded body size tabs
  left_join(treatments_temperature) # add treatment data corresponding to each tank (1-24)

# Save o2 measures --------------------------------------------------------

temperature %>%
  clean_names() %>% 
  filter(!is.na(o2_do_mg_l)) %>% 
  select(tank, date_time, o2_do_mg_l, temp_deg_c) %>% 
  write_csv(file = "data/o2_temp.csv")


# Plot temperature --------------------------------------------------------
temp_plot = temperature %>% 
  filter(date >= "2022-05-27") %>%
  drop_na(temp_deg_f) %>% 
  drop_na(temp_deg_c) %>% 
  ggplot(aes(x = date_time, y = temp_deg_c, color = temp_treat)) + 
  geom_point(size = 0.3) + 
  geom_line(aes(group = tank), alpha = 0.5, linewidth = 0.2) +
  scale_color_colorblind() + 
  theme_classic() + 
  # scale_y_log10() +
  NULL

ggsave(temp_plot, file = "plots/temp_plot.jpg", width = 5, height = 3)

temperature %>%
  filter(date >= "2022-05-27") %>% 
  group_by(date_time, temp_treat) %>% 
  summarize(mean = mean(temp_deg_c, na.rm = T)) %>% 
  pivot_wider(names_from = temp_treat, values_from = mean) %>% 
  mutate(diff = heated - ambient) %>%
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
  ggplot(aes(x = temp_from_ysi_deg_c, y = temp_deg_c, color = temp_treat)) + 
  geom_point() +
  ylim(0, 40) +
  xlim(0, 40) + 
  geom_abline()



# Plot O2_temp ------------------------------------------------------------
library(ggthemes)
library(viridis)
temperature %>%
  clean_names() %>% 
  filter(!is.na(o2_do_mg_l)) %>% 
  ggplot(aes(y = o2_do_mg_l, x = date_time)) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(group = tank, color = treatment)) +
  facet_wrap(~treatment)
  

