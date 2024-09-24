library(tidyverse)
library(brms) 
library(isdbayes)

# load model
fit_isd = readRDS("models/fit_isd.rds")

pp_check(fit_isd) + scale_x_log10()

pp_check(fit_isd, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()
