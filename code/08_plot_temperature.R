library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())


# Load data ---------------------------------------------------------------
#1) load treatment data
treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(temp_treat, "_",nutrient_treat))

#2) load temperature data
temperature <- read_csv("data/temperature.csv") %>% 
  mutate(date = mdy(date), # get proper date format
         temp_deg_c = (temp_deg_f - 32)*(5/9), # convert celsius to Fahrenheit
         date_time = parse_date_time(paste(date, time), orders = "Ymd HMS")) %>% # merge dates/times
  filter(!is.na(temp_deg_c)) %>% 
  select(-heater) # remove heater column (don't need it)

#3) combine temp and treatment data, then get mean temps
treatments_temperature =  left_join(temperature, treatments) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  mutate(day_of_experiment =  julian(date) - julian(as.Date("2022-05-27")))


temp_mean_diff = treatments_temperature %>%
  group_by(date, temp_treat) %>%
  reframe(mean_temp = mean(temp_deg_c)) %>% 
  pivot_wider(names_from = temp_treat, values_from = mean_temp) %>% 
  mutate(diff = heated - ambient) %>% 
  filter(date >= "2022-05-27")

temp_mean_diff %>% 
  reframe(mean = mean(diff, na.rm = T),
          sd = sd(diff, na.rm = T))

temp_mean_diff %>% 
  group_by(date) %>% 
  reframe(mean = mean(diff, na.rm = T),
          sd = sd(diff, na.rm = T)) %>% View()


# plot --------------------------------------------------------------------

temperature_plot = treatments_temperature %>% 
  filter(day_of_experiment >= 0) %>% 
  group_by(tank, date, temp_plus, day_of_experiment) %>% 
  reframe(temp_deg_c = mean(temp_deg_c)) %>% 
  mutate(temp_plus = as.factor(temp_plus),
         temp_plus = fct_relevel(temp_plus, "+heat")) %>% 
  ggplot(aes(x = day_of_experiment, y = temp_deg_c, color = temp_plus)) +
  geom_point(size = 0.5) +
  geom_line(aes(group = tank), alpha = 0.2) +
  scale_color_manual(values = c("#e69f00", "#000000")) +
  labs(y = "Water Temperature (\u00b0C)",
       x = "Day of Experiment",
       color = "") +
  NULL

ggsave(temperature_plot, file = "plots/temperature_plot.jpg", dpi = 400, width = 6, height = 4)
