library(tidyverse)
dw_fixed = readRDS(file = "data/dw_fixed.rds")

dw_culled = dw_fixed %>% 
  # filter(dw_mg>0) %>% 
  mutate(log2_bins = cut(log2(dw_mg), breaks = 7)) %>% 
  group_by(log2_bins, tank)  %>%
  tally() %>% 
  separate(log2_bins, into = c("min", "max"), sep = ",", remove = F) %>% 
  mutate(min = 2^parse_number(min),
         max = 2^parse_number(max))

dw_culled %>% 
  ggplot(aes(x = min, y = n)) + 
  geom_point() +
  facet_wrap(~tank, scales = "free") +
  scale_x_log10()

dw_newxmins = dw_culled %>% group_by(tank) %>% 
  filter(n == max(n)) %>% 
  filter(min == max(min)) %>% 
  distinct(tank, min) %>% 
  rename(newxmin_cutoff = min)

dw_new = dw_fixed %>% left_join(dw_newxmins) %>% filter(dw_mg >= newxmin_cutoff) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg))

saveRDS(dw_new, file = "data/dw_new.rds")
