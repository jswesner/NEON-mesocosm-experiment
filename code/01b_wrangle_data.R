library(poweRlaw)
library(tidyverse)
# this method follows Clauset et al. 2009 by estimating the minimum
# size for which the data follow a power law using by minimizing the K-S statistic.
# i.e., xmin is model-based

# load data
dat = read_csv("data/dw_fixed.csv") %>% filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg))

dat_list = dat %>% group_by(tank) %>% group_split()

xmin_list = list()

for(i in 1:length(dat_list)){
  powerlaw = conpl$new(dat_list[[i]]$dw_mg)
  xmin_list[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin,
                          tank = unique(dat_list[[i]]$tank))
}

xmins_clauset = bind_rows(xmin_list)

dat_clauset_xmins = dat %>% left_join(xmins_clauset) %>% 
  group_by(tank) %>% 
  filter(dw_mg >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset,
         xmax = max(dw_mg))

saveRDS(dat_clauset_xmins, file = "data/dat_clauset_xmins.rds")


# data with emergers --------------------
# emergence data collected 27-28 June at the end of the experiment. Same time as benthics
dw_raw_adults = read_csv("data/dw_raw_adults.csv") %>% select(tank, temp_treat, nutrient_treat, dw_mg, m2) %>% 
  mutate(stage = "adult")
dat = read_csv("data/dw_fixed.csv") %>% filter(dw_mg > 0) %>% mutate(m2 = 0.09) %>% select(tank, temp_treat, nutrient_treat, dw_mg, m2) %>% 
  mutate(stage = "larvae")

dat_emerge_ind = bind_rows(dw_raw_adults, dat) %>% 
  group_by(tank, temp_treat, nutrient_treat, dw_mg, m2, stage) %>% 
  tally() %>% 
  mutate(no_m2 = n/m2)

dat_list_emerge = dat_emerge_ind %>% group_by(tank) %>% group_split()

xmin_list_emerge = list()

for(i in 1:length(dat_list_emerge)){
  powerlaw = conpl$new(dat_list_emerge[[i]]$dw_mg)
  xmin_list_emerge[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin,
                          tank = unique(dat_list_emerge[[i]]$tank))
}

xmins_clauset_emerge = bind_rows(xmin_list_emerge)

dat_clauset_xmins_emerge = dat_emerge_ind %>% left_join(xmins_clauset_emerge) %>% 
  group_by(tank) %>% 
  filter(dw_mg >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset,
         xmax = max(dw_mg))

saveRDS(dat_clauset_xmins_emerge, file = "data/dat_clauset_xmins_emerge.rds")


