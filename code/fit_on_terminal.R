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
#           prior = c(prior(normal(-1.6, 0.5), class = "Intercept"),
#                     prior(normal(0, 0.2), class = "b")))

# fit_isd = update(fit_isd, newdata = dw_new)

# saveRDS(fit_isd, file = "models/fit_isd_clauset.rds")

fit_isd_newprior = update(fit_isd, prior = c(prior(normal(-2, 0.5), class = "Intercept"),
                                             prior(normal(0, 0.2), class = "b")),
                          chains = 4, iter = 2000, 
                          cores = 4)

saveRDS(fit_isd_newprior, file = "models/fit_isd_newprior.rds")
