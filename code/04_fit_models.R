library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 
source("models/paretocounts.R") # loads the models necessary for fitting the isd

# model isd ---------------------------------------------------------------

dw = readRDS(dw_fixed, file = "data/dw_fixed.rds")

fit = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat, 
          data = dw,
          stanvars = stanvars,  # new thing added by the package
          family = paretocounts(), # new thing added by the package
          chains = 1, iter = 1000,
          file = "models/brm_fit_samples.rds",
          prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                    prior(normal(0, 0.2), class = "b")
          ))

fit = update(fit, iter = 2000, chains = 4)

fit_nodaphnia = update(fit, newdata = dw %>% filter(taxon != "daphnia"))



# check model -------------------------------------------------------------

summary(fit)

conditional_effects(fit)
