library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)
library(poweRlaw)
library(ggthemes)

# fitting parts are silenced. They may take minutes to hours to refit.
# load data ---------------------------------------------------------------

dat_morin_xmins = readRDS(file = "data/dat_morin_xmins.rds") 

# model with continuous gpp -----------------------------------------------

fit_isd_gpp_predictor = readRDS(file = "models/fit_isd_morin_gpp_predictor.rds")

fit_isd_gpp_predictor = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                               formula = . ~ temp_treat*june_22_gpp_s*june_28_gpp_s + (1|tank),
                               prior = c(prior(normal(0, 0.2), class = "b"),
                                         prior(normal(-2, 0.5), class = "Intercept"),
                                         prior(exponential(2), class = "sd")),
                               iter = 2000, chains = 4,
                               data2 = dat_morin_xmins)

saveRDS(fit_isd_gpp_predictor, file = "models/fit_isd_morin_gpp_predictor.rds")

fit_isd_temponly = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                               formula = . ~ temp_treat + (1|tank),
                               iter = 1000, chains = 1,
                               data2 = dat_morin_xmins)

saveRDS(fit_isd_temponly, file = "models/fit_isd_temponly.rds")

fit_isd_interceptonly = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                          formula = . ~ 1 + (1|tank),
                          iter = 1000, chains = 1,
                          data2 = dat_morin_xmins)

saveRDS(fit_isd_interceptonly, file = "models/fit_isd_interceptonly.rds")


fit_isd_gpp_predictor_additive28th = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                                formula = . ~ temp_treat*june_22_gpp_s + june_28_gpp_s + (1|tank),
                                iter = 1000, chains = 1,
                                data2 = dat_morin_xmins)

saveRDS(fit_isd_gpp_predictor_additive28th, file = "models/fit_isd_morin_gpp_predictor_additive28th.rds")


fit_isd_gpp_predictor_additive22th = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                                            formula = . ~ temp_treat*june_28_gpp_s + june_22_gpp_s + (1|tank),
                                            iter = 1000, chains = 1,
                                            data2 = dat_morin_xmins)

saveRDS(fit_isd_gpp_predictor_additive22th, file = "models/fit_isd_morin_gpp_predictor_additive22th.rds")

fit_isd_gpp_predictor_28th = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                                formula = . ~ temp_treat*june_28_gpp_s + (1|tank),
                                iter = 2000, chains = 4,
                                data2 = dat_morin_xmins)

saveRDS(fit_isd_gpp_predictor_28th, file = "models/fit_isd_morin_gpp_predictor_28th.rds")

fit_isd_gpp_predictor_22th = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                                    formula = . ~ temp_treat*june_22_gpp_s + (1|tank),
                                    iter = 2000, chains = 4,
                                    data2 = dat_morin_xmins)

saveRDS(fit_isd_gpp_predictor_22th, file = "models/fit_isd_morin_gpp_predictor_22th.rds")

fit_isd_gpp_predictor_nutrient = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins,
                                    formula = . ~ temp_treat*june_22_gpp_s*june_28_gpp_s + nutrient_treat + (1|tank),
                                    iter = 2000, chains = 4,
                                    data2 = dat_morin_xmins)

saveRDS(fit_isd_gpp_predictor_nutrient, file = "models/fit_isd_morin_gpp_predictor_nutrient.rds")


# refit after excluding largest values ------------------------------------

# does the treatment effect remain if we exclude the largest X% of sizes. This is motivated by the residual deviations of the largest sizes.

# dat_morin_xmins_exclude_largest = dat_morin_xmins |> 
#   group_by(tank) |> 
#   filter(dw_mg <= 0.5*xmax) |> # keep only sizes that are <= half of xmax
#   mutate(xmax = max(dw_mg))
# 
# fit_isd_gpp_exclude_largest = update(fit_isd_gpp_predictor, newdata = dat_morin_xmins_exclude_largest, 
#                                      cores = 4)
# saveRDS(fit_isd_gpp_exclude_largest, file = "models/fit_isd_gpp_exclude_largest.rds")
# 
# # refit with emergencexmax_value# refit with emergence included --------------------------------
# 
# # data with all emergence, even if adults are smaller than the larval cutoff
# dat_emerge_ind = readRDS(file = "data/dat_emerge_ind.rds") |> 
#   group_by(tank) |> 
#   mutate(xmin = min(dw_mg),
#          xmax = max(dw_mg)) %>% 
#   mutate(counts = n)
# 
# # data with emergence culled to >= the larval cutoff
# dat_emerge_ind_cutoff = readRDS(file = "data/dat_emerge_ind.rds") |> 
#   filter(dw_mg >= min(dat_morin_xmins$xmin)) |> 
#   group_by(tank) |> 
#   mutate(xmin = min(dw_mg),
#          xmax = max(dw_mg)) %>% 
#   mutate(counts = n)
# 
# fit_isd_allemergence = update(fit_isd_gpp_predictor, newdata = dat_emerge_ind, cores = 4)
# saveRDS(fit_isd_allemergence, file = "models/fit_isd_allemergence.rds")
# 
# # fit_isd_culledemergence = update(fit_isd_gpp_predictor, newdata = dat_emerge_ind_cutoff, cores = 4)
# # saveRDS(fit_isd_culledemergence, file = "models/fit_isd_culledemergence.rds")
# 
# # check if GPP increased with nutrient treatment ------------------------
# 
# treatments = readRDS("data/treatments.rds")
# 
# gpp_long = readRDS(file = "data/gpp_wrangled.rds") |> 
#   left_join(treatments) %>% 
#   mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) %>% 
#   mutate(day_of_experiment = paste0("Day ", day_of_experiment)) 
# 
# gpp_wrangled = gpp_long %>% 
#   ungroup %>% 
#   mutate(nep_s = scale(nep),
#          gpp_s = scale(gpp)) %>% 
#   mutate(date_code = case_when(time == "1" ~ "june_22", T ~ "june_28")) %>% 
#   select(tank, date_code, nep_s, nep, gpp_s, gpp) %>% 
#   pivot_longer(cols = c(-tank, -date_code)) %>% 
#   unite(date_name, c(date_code, name)) %>% 
#   pivot_wider(names_from = date_name, values_from = value)
# 
# # gpp_by_treatment = gpp_long |> 
# #   ggplot(aes(x = temp_plus, y = gpp, group = interaction(nut_plus, temp_plus))) +
# #   geom_boxplot(width = 0.5, aes(fill = nut_plus), outlier.shape = NA) +
# #   geom_point(position = position_dodge(width = 0.5), shape = 21) +
# #   facet_wrap(~day_of_experiment) +
# #   scale_fill_colorblind(black = F) +
# #   labs(x = "Temperature Treatment",
# #        y = expression(paste("Gross Primary Production (", mg~O[2]~L^{-1}~d^{-1}, ")")),
# #        fill = "") +
# #   theme(legend.position = "inside",
# #         legend.position.inside = c(0.9, 0.9))
# # 
# # 
# # ggsave(gpp_by_treatment, file = "plots/gpp_by_treatment.jpg", width = 6, height = 6)
# 
# min(gpp_wrangled$june_22_gpp)
# 
