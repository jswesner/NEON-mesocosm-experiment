library(flowCore)
library(tidyverse)
library(rstan)
library(sizeSpectra)

stan_model = stan_model("models/b_pareto_singlesample.stan")

test_fcs <- read.FCS("data/phytoplankton/2023-01-12T15-29-26_exars_2022_01_2/exars_2022_01_2_BF_Dead.fcs", 
                     truncate_max_range = F)

test_2 = read.FCS("data/phytoplankton/2023-01-12T15-29-26_exars_2022_01_2/exars_2022_01_2_BF_Live.fcs", 
                  truncate_max_range = F)

sizes = tibble(micron = test_fcs@exprs[,1]) %>% 
  bind_rows(tibble(micron = test_2@exprs[,1]))


hist(sizes$micron)

xmin = min(sizes$micron)
xmax = max(sizes$micron)
x = sizes$micron

dat = tibble(xmin = xmin, xmax = xmax, x = x)

stan_data = list(xmin = dat$xmin,
                N = nrow(dat),
                xmax = dat$xmax,
                x = dat$x)



fit = sampling(object = stan_model,
               data = stan_data,
               iter = 1000, chains = 1)


MLE.plot(x = x, b = -1.44)
