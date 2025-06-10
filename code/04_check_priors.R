library(tidyverse)
library(brms) 
library(isdbayes)
library(poweRlaw)

# model isd ---------------------------------------------------------------

dw_new = readRDS(file = "data/dat_clauset_xmins.rds") # xmins are corrected using clauset method
treatments = readRDS("data/treatments.rds") %>% 
  mutate(treatments = paste0(temp_treat, "_", nutrient_treat)) %>% 
  select(temp_nut_plus, treatments)

# fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat + 
#                 (1|tank), 
#           data = dw_new,
#           stanvars = stanvars,
#           family = paretocounts(),
#           chains = 1, iter = 1000,
#           # file = "models/fit_isd.rds",
#           prior = c(prior(normal(-1.6, 0.5), class = "Intercept"),
#                     prior(normal(0, 0.2), class = "b")))

n = 500

isd_priors = tibble(a = rnorm(n, -1.6, 0.5),
       b1 = rnorm(n, 0, 0.2),
       b2 = rnorm(n, 0, 0.2),
       b3 = rnorm(n, 0, 0.2),
       .draw = 1:n) %>% 
  mutate(ambient_ambient = a,
          = a + b1*1 + b2*0 + b3*0,
         nut = a + b1*0 + b2*1 + b3*0,
         temp_nut = a + b1*1 + b2*1 + b3*1)

isd_priors %>% 
  ggplot(aes(x = ))


# fit mean size -----------------------------------------------------------
dat = readRDS(file = "data/dw_fixed.rds") 

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

brm_mean_larvae = update(brm_mean_larvae, chains = 1, iter = 500, newdata = dat)

# fit gpp -----------------------------------------------------------------

gpp_wrangled = readRDS(file = "data/gpp_wrangled.rds") %>% 
  mutate(mean_gpp = mean(gpp, na.rm = T),
         gpp_s = gpp/mean_gpp)

brm_gpp = brm(gpp_s ~ nutrient_treat*temp_treat + (1 + nutrient_treat*temp_treat|time),
              family = Gamma(link = "log"),
              data = gpp_wrangled,
              prior = c(prior(normal(0, 1), class = "Intercept"),
                        prior(normal(0, 1), class = "b"),
                        prior(exponential(2), class = "sd")),
              data2 = list(mean_gpp = unique(gpp_wrangled$mean_gpp)))

saveRDS(brm_gpp, file = "models/brm_gpp.rds")

pp_check(brm_gpp)














