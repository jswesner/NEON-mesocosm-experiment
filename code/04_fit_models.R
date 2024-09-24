library(tidyverse)
library(brms) 
library(isdbayes)

# model isd ---------------------------------------------------------------

# dw = readRDS(dw_fixed, file = "data/dw_fixed.rds") # old data without corrected xmins
dw_new = readRDS(file = "data/dw_new.rds") # xmins are corrected using mlebin method

fit_isd = readRDS("models/fit_isd.rds")

fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat + 
                (1|tank), 
          data = dw_new,
          stanvars = stanvars,
          family = paretocounts(),
          chains = 4, iter = 2000,
          file = "models/fit_isd.rds",
          prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                    prior(normal(0, 0.2), class = "b")))

