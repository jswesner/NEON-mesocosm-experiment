library(tidyverse)


# load data
dw_raw = read_csv(file = "data/dw_raw.csv")

# plot the data to check for errors (repeat with different x and y variables)

dw_raw %>% 
  ggplot(aes(x = tank, y = dw_mg)) +
  geom_jitter() +
  scale_y_log10()

dw_raw %>% 
  # filter(dw_mg <= 1000) %>% 
  ggplot(aes(x = length_mm)) + 
  geom_histogram() +
  scale_x_log10()

# filter obvious outliers (anything else to add?)
dw_fixed = dw_raw %>% 
  filter(dw_mg < 200) %>%
  group_by(tank) %>%
  mutate(xmin = min(dw_mg, na.rm = T),
         xmax = max(dw_mg, na.rm = T),
         counts = 1) 

write_csv(dw_fixed, file = "data/dw_fixed.csv")
saveRDS(dw_fixed, file = "data/dw_fixed.rds")

