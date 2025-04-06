library(tidyverse)
library(brms) 
library(isdbayes)

# load model
fit_isd = readRDS("models/fit_isd_clauset.rds")

pp_check(fit_isd) + scale_x_log10()

pp_check(fit_isd, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()

# check mean size model
brm_mean_larvae = readRDS(file = "models/brm_mean_larvae_rawdata_lognorm.rds")

pp_check(brm_mean_larvae, type = "dens_overlay_grouped", group = "tank") + 
  scale_x_log10()


# check priors
prior_preds = brm_mean_larvae$data %>% distinct(temp_treat, nutrient_treat) %>% 
  expand_grid(.draw = 1:100) %>% 
  mutate(int = rnorm(nrow(.), -5, 3),
         b = rnorm(nrow(.), 0, 1)) %>% 
  mutate(log_y = case_when(temp_treat == "ambient" & nutrient_treat == "ambient" ~ int,
                           TRUE ~ int + b)) %>% 
  mutate(dw_mg = exp(log_y))

cond_plot = plot(conditional_effects(brm_mean_larvae, effects = "temp_treat:nutrient_treat"))
cond_plot$`temp_treat:nutrient_treat`$data %>% 
  ggplot(aes(x = temp_treat, color = nutrient_treat)) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__)) +
  facet_wrap(~nutrient_treat) +
  stat_pointinterval(data = prior_preds, aes(y = dw_mg), color = "grey") +
  scale_y_log10()
