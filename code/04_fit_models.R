library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 
library(isdbayes)

# model isd ---------------------------------------------------------------

dw = readRDS(file = "data/dw_fixed.rds")

fit_nodaphnia = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat, 
          data = dw %>% filter(correct_taxon != "Daphnia"),
          stanvars = stanvars,  # new thing added by the package
          family = paretocounts(), # new thing added by the package
          chains = 4, iter = 2000,
          file = "models/brm_fit_nodaphnia.rds",
          prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                    prior(normal(0, 0.2), class = "b")))

fit_daphnia = update(fit, newdata = dw)
saveRDS(fit_daphnia, file = "models/fit_daphnia.rds")


# check model -------------------------------------------------------------

summary(fit_nodaphnia)

plot_fit = plot(conditional_effects(fit_nodaphnia, effects = "temp_treat:nutrient_treat"))

plot_fit$`temp_treat:nutrient_treat`$data %>% 
  ggplot(aes(x = temp_treat, y = estimate__, color = nutrient_treat)) + 
  geom_point(position = position_dodge(width = .2)) +
  geom_linerange(aes(ymin = lower__, ymax = upper__),
                 position = position_dodge(width = .2)) +
  labs(y = "asdfasdf") +
  theme_default()
