library(tidyverse)
library(brms) 
library(isdbayes)
library(poweRlaw)

# fit with xmin CIs -------------------------------------------------------

dat_clauset_xmins_boot_lower = readRDS(file = "data/dat_clauset_xmins_lower.rds")
dat_clauset_xmins_boot_upper = readRDS(file = "data/dat_clauset_xmins_upper.rds")
fit_isd = readRDS("models/fit_isd_clauset.rds")

fit_isd_lower = update(fit_isd, newdata = dat_clauset_xmins_boot_lower, 
                       chains = 1, iter = 500)
fit_isd_upper = update(fit_isd, newdata = dat_clauset_xmins_boot_upper, 
                       chains = 1, iter = 500)

saveRDS(fit_isd_lower, file = "models/fit_isd_lower.rds")
saveRDS(fit_isd_upper, file = "models/fit_isd_upper.rds")