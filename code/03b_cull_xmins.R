library(poweRlaw)
library(tidyverse)

dat = readRDS(file = "data/dw_fixed.rds")

# this method follows Clauset et al. 2009 by estimating the minimum
# size for which the data follow a power law using by minimising the K-S statistic.
# i.e., xmin is model-based

# load data
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



# No zooplankton ----------------------------------------------------------

# load data
dat_list = dat %>% 
  filter(correct_taxon != "Daphnia") %>% 
  group_by(tank) %>% group_split()

xmin_list = list()

for(i in 1:length(dat_list)){
  powerlaw = conpl$new(dat_list[[i]]$dw_mg)
  xmin_list[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin,
                          tank = unique(dat_list[[i]]$tank))
}

xmins_clauset = bind_rows(xmin_list)

dat_clauset_xmins_nozoo = dat %>% 
  filter(correct_taxon != "Daphnia") %>% 
  left_join(xmins_clauset) %>% 
  group_by(tank) %>% 
  filter(dw_mg >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset,
         xmax = max(dw_mg))

saveRDS(dat_clauset_xmins_nozoo, file = "data/dat_clauset_xmins_nozoo.rds")

