library(tidyverse)
library(brms) 
library(isdbayes)
library(poweRlaw)
library(tidybayes)

# sample from prior only and check how reasonable they are

fit_isd_gpp_predictor = readRDS(file = "models/fit_isd_morin_gpp_predictor.rds")

fit_isd_priors = update(fit_isd_gpp_predictor, 
                                      sample_prior = "only",
                                      chains = 1, iter = 500, 
                                      prior = c(prior(normal(0, 0.2), class = "b"),
                                                prior(normal(-2, 0.5), class = "Intercept"),
                                                prior(exponential(2), class = "sd")))

saveRDS(fit_isd_priors, file = "models/fit_isd_priors.rds")

d = fit_isd_gpp_predictor$data

gpp1 = c(-1, 0, 1)
gpp2 = 0

prior_preds = d |> 
  distinct(temp_treat) |> 
  expand_grid(june_22_gpp_s = gpp1,
              june_28_gpp_s = gpp2) |> 
  mutate(xmin = min(d$xmin), xmax = max(d$xmax), counts = 1) |> 
  add_epred_draws(fit_isd_priors, re_formula = NA)
  

prior_preds |> 
  ggplot(aes(x = temp_treat, y = .epred)) + 
  stat_pointinterval() +
  facet_wrap(~june_22_gpp_s)


prior_preds_tank = d |> 
  distinct(temp_treat, tank) |> 
  expand_grid(june_22_gpp_s = gpp1,
              june_28_gpp_s = gpp2) |> 
  mutate(xmin = min(d$xmin), xmax = max(d$xmax), counts = 1) |> 
  add_epred_draws(fit_isd_priors, re_formula = NULL)

prior_preds_tank |> 
  ggplot(aes(x = temp_treat, y = .epred)) + 
  stat_pointinterval(aes(group = tank), position = position_jitter(width = 0.1)) +
  facet_wrap(~june_22_gpp_s)
