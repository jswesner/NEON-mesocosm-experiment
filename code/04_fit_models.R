library(tidyverse)
library(brms) 
library(isdbayes)
library(poweRlaw)

# model isd ---------------------------------------------------------------

# dw = readRDS(dw_fixed, file = "data/dw_fixed.rds") # old data without corrected xmins
dw_new = readRDS(file = "data/dat_clauset_xmins.rds") # xmins are corrected using clauset method

fit_isd = readRDS("models/fit_isd_clauset.rds")

# fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat + 
#                 (1|tank), 
#           data = dw_new,
#           stanvars = stanvars,
#           family = paretocounts(),
#           chains = 1, iter = 1000,
#           # file = "models/fit_isd.rds",
#           prior = c(prior(normal(-2, 0.5), class = "Intercept"),
#                     prior(normal(0, 0.2), class = "b")))

saveRDS(fit_isd, file = "models/fit_isd_clauset.rds")

# model without zoops -----------------------------------------------------

dat_clauset_xmins_nozoo = readRDS(file = "data/dat_clauset_xmins_nozoo.rds")

fit_isd_nozoo = update(fit_isd, newdata = dat_clauset_xmins_nozoo)
saveRDS(fit_isd_nozoo, file = "models/fit_isd_nozoo.rds")

# model isd with emergence ---------------------------------------------------------------

dat_clauset_xmins_emerge = readRDS(file = "data/dat_clauset_xmins_emerge.rds") %>% mutate(counts = no_m2)

fit_isd = readRDS("models/fit_isd_clauset.rds") 

# fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat + 
#                 (1|tank), 
#           data = dw_new,
#           stanvars = stanvars,
#           family = paretocounts(),
#           chains = 1, iter = 1000,
#           # file = "models/fit_isd.rds",
#           prior = c(prior(normal(-1.6, 0.5), class = "Intercept"),
#                     prior(normal(0, 0.2), class = "b")))

# fit_isd_emerge_clauset = update(fit_isd, newdata = dat_clauset_xmins_emerge)

# saveRDS(fit_isd_emerge_clauset, file = "models/fit_isd_emerge_clauset.rds")

# fit_isd_emerge_clauset = readRDS(file = "models/fit_isd_emerge_clauset.rds")

# test power law ----------------------------------------------------------
dat = readRDS(file = "data/dw_fixed.rds")
dat_list = dat %>% group_by(tank) %>% group_split()


# Define the function to calculate KS distance statistic and return a tibble
calculate_ks_stat_tibble <- function(data) {
  # Create an object for the continuous power law
  m <- conpl$new(data$dw_mg)
  
  # Fit the model to the data
  fit <- estimate_pars(m)
  m$setPars(fit)
  
  # Set the empirical xmin parameter in the model
  xmin_empirical <- min(data$dw_mg)
  m$setXmin(xmin_empirical)
  
  # Get the xmax value
  xmax_value <- max(data$dw_mg)
  
  # Get the KS distance statistic
  ks_stat <- poweRlaw::get_distance_statistic(m, 
                                              xmax = xmax_value, 
                                              distance = "ks")
  
  # Create a tibble with the KS distance statistic and tank identifier
  result <- tibble(tank = unique(data$tank), ks_stat = ks_stat)
  
  return(result)
}

# Apply the function to each element of the dat_list and combine the results
ks_stats_tibble <- lapply(dat_list, calculate_ks_stat_tibble) %>%
  bind_rows()

ks_stats = ks_stats_tibble %>% 
  left_join(dat %>% distinct(tank, temp_treat, nutrient_treat)) 

saveRDS(ks_stats, file = "models/ks_stats.rds")


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














