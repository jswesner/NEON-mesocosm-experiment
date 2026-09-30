library(tidyverse)
library(brms) 
library(isdbayes)
library(poweRlaw)
library(ggthemes)
theme_set(brms::theme_default(base_family = "bayesplotbase_family"))

# fit mean size -----------------------------------------------------------
# load data

dat_morin_xmins = readRDS(file = "data/dat_morin_xmins.rds") 

dat = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  filter(correct_taxon != "Daphnia") %>%
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) |> 
  left_join(dat_morin_xmins |> ungroup() |> select(tank, june_22_gpp_s, june_28_gpp_s) |>  distinct())

tibble(sim_mg = rgamma(500, shape = 0.03, scale = 10)) %>% 
  ggplot(aes(x = 1, y = sim_mg)) +
  geom_jitter()

median(rgamma(500, shape = 0.03, scale = 10))
mean(dat$dw_mg)
sd(dat$dw_mg)

scale = 10
shape = mean(dat$dw_mg)/scale
shape

# brm_mean_larvae = brm(dw_mg ~ temp_treat*nutrient_treat + 
#                         (1 + temp_treat*nutrient_treat|correct_taxon),
#                       data = dat,
#                       iter = 2000, chains = 4,
#                       family = Gamma(link = "log"),
#                       prior = c(prior(normal(-5, 3), class = "Intercept"),
#                                 prior(normal(0, 1), class = "b")),
#                       cores = 4, threads = threading(2))

# saveRDS(test_larvae, file = "models/test_larvae.rds")

brm_mean_larvae = readRDS(file = "models/brm_mean_larvae.rds")

brm_mean_larvae = update(brm_mean_larvae, . ~ temp_treat*june_22_gpp_s*june_28_gpp_s +
                           (1 + temp_treat*june_22_gpp_s*june_28_gpp_s|correct_taxon),
                         chains = 4, iter =2000, newdata = dat,
                         cores = 4, threads = threading(2))

saveRDS(brm_mean_larvae, file = "models/brm_mean_larvae.rds")

# brm_mean_larvae_morin = update(brm_mean_larvae, . ~ temp_treat*june_22_gpp_s*june_28_gpp_s +
#                            (1 + temp_treat*june_22_gpp_s*june_28_gpp_s|correct_taxon),
#                          chains = 4, iter =2000, newdata = dat_morin_xmins,
#                          cores = 4, threads = threading(2))
# 
# saveRDS(brm_mean_larvae_morin, file = "models/brm_mean_larvae_morin.rds")
