library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 
source("models/paretocounts.R") # loads the models necessary for fitting the isd

# model isd ---------------------------------------------------------------

dw = readRDS(dw_fixed, file = "data/dw_fixed.rds")

fit_nodaphnia = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat, 
          data = dw %>% filter(correct_taxon != "Daphnia"),
          stanvars = stanvars,  # new thing added by the package
          family = paretocounts(), # new thing added by the package
          chains = 4, iter = 2000,
          file = "models/brm_fit_nodaphnia.rds",
          prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                    prior(normal(0, 0.2), class = "b")))

fit_daphnia = update(fit, newdata = dw)



# check model -------------------------------------------------------------

summary(fit)

conditional_effects(fit)
