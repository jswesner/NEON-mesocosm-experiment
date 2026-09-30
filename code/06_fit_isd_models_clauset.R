library(tidyverse)
library(brms) 
library(isdbayes)
library(poweRlaw)
library(ggthemes)
theme_set(brms::theme_default(base_family = "bayesplotbase_family"))

# refit models but use the clauset xmins instead of morin
# fitting parts are silenced. They may take minutes to hours to refit.
# model isd ---------------------------------------------------------------
treatments = readRDS("data/treatments.rds")

gpp_long = readRDS(file = "data/gpp_wrangled.rds") |> 
  left_join(treatments) %>% 
  mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) %>% 
  mutate(day_of_experiment = paste0("Day ", day_of_experiment)) 

gpp_wrangled = gpp_long %>% 
  ungroup %>% 
  mutate(nep_s = scale(nep),
         gpp_s = scale(gpp)) %>% 
  mutate(date_code = case_when(time == "1" ~ "june_22", T ~ "june_28")) %>% 
  select(tank, date_code, nep_s, nep, gpp_s, gpp) %>% 
  pivot_longer(cols = c(-tank, -date_code)) %>% 
  unite(date_name, c(date_code, name)) %>% 
  pivot_wider(names_from = date_name, values_from = value)

dat_clauset_xmins = readRDS(file = "data/dat_clauset_xmins.rds") %>% 
  left_join(gpp_wrangled)

# check if GPP increased with nutrient treatment
# gpp_by_treatment = gpp_long |> 
#   ggplot(aes(x = temp_plus, y = gpp, group = interaction(nut_plus, temp_plus))) +
#   geom_boxplot(width = 0.5, aes(fill = nut_plus), outlier.shape = NA) +
#   geom_point(position = position_dodge(width = 0.5), shape = 21) +
#   facet_wrap(~day_of_experiment) +
#   scale_fill_colorblind(black = F) +
#   labs(x = "Temperature Treatment",
#        y = expression(paste("Gross Primary Production (", mg~O[2]~L^{-1}~d^{-1}, ")")),
#        fill = "") +
#   theme(legend.position = "inside",
#         legend.position.inside = c(0.9, 0.9))
# 
# 
# ggsave(gpp_by_treatment, file = "plots/gpp_by_treatment.jpg", width = 6, height = 6)


# model with factorial nutrients ------------------------------------------

# fit_isd_morin = readRDS("models/fit_isd_morin.rds")
# 
# fit_isd_morin = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat +
#                 (1|tank),
#           data = dat_morin_xmins,
#           stanvars = stanvars,
#           family = paretocounts(),
#           chains = 4, iter = 2000,
#           # file = "models/fit_isd.rds",
#           prior = c(prior(normal(-2, 0.5), class = "Intercept"),
#                     prior(normal(0, 0.2), class = "b")))

# fit_isd = update(fit_isd, newdata = dat_clauset_xmins)
# saveRDS(fit_isd, file = "models/fit_isd_clauset.rds")

# fit_isd_morin = update(fit_isd_morin, newdata = dat_morin_xmins, chains = 1, iter = 1000)
# saveRDS(fit_isd_morin, file = "models/fit_isd_morin.rds")


# model with continuous gpp -----------------------------------------------

fit_isd_gpp_predictor = readRDS(file = "models/fit_isd_morin_gpp_predictor.rds")

fit_isd_gpp_predictor_clauset = update(fit_isd_gpp_predictor, newdata = dat_clauset_xmins,
                               formula = . ~ temp_treat*june_22_gpp_s*june_28_gpp_s + (1|tank),
                               iter = 2000, chains = 4,
                               prior = c(prior(0))
                               data2 = dat_clauset_xmins)
# 
# saveRDS(fit_isd_gpp_predictor_clauset, file = "models/fit_isd_clauset_gpp_predictor.rds")
# 
# fit_isd_temponly_clauset = update(fit_isd_gpp_predictor, newdata = dat_clauset_xmins,
#                           formula = . ~ temp_treat + (1|tank),
#                           iter = 1000, chains = 1,
#                           data2 = dat_clauset_xmins)
# 
# saveRDS(fit_isd_temponly_clauset, file = "models/fit_isd_temponly_clauset.rds")

# fit_isd_interceptonly_clauset = update(fit_isd_gpp_predictor, newdata = dat_clauset_xmins,
#                                formula = . ~ 1 + (1|tank),
#                                iter = 1000, chains = 1,
#                                data2 = dat_clauset_xmins)
# 
# saveRDS(fit_isd_interceptonly_clauset, file = "models/fit_isd_interceptonly_clauset.rds")


# fit_isd_gpp_predictor_additive28th_clauset = update(fit_isd_gpp_predictor, newdata = dat_clauset_xmins,
#                                             formula = . ~ temp_treat*june_22_gpp_s + june_28_gpp_s + (1|tank),
#                                             iter = 1000, chains = 1,
#                                             data2 = dat_clauset_xmins)
# 
# saveRDS(fit_isd_gpp_predictor_additive28th_clauset, file = "models/fit_isd_clauset_gpp_predictor_additive28th.rds")
# #
# 
# fit_isd_gpp_predictor_additive22th_clauset = update(fit_isd_gpp_predictor, newdata = dat_clauset_xmins,
#                                             formula = . ~ temp_treat*june_28_gpp_s + june_22_gpp_s + (1|tank),
#                                             iter = 1000, chains = 1,
#                                             data2 = dat_clauset_xmins)
# 
# saveRDS(fit_isd_gpp_predictor_additive22th_clauset, file = "models/fit_isd_clauset_gpp_predictor_additive22th.rds")
# #
# 
# 
