library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 
library(isdbayes)
<<<<<<< HEAD
library(tidybayes)
library(ggthemes)
library(viridis)

# model isd ---------------------------------------------------------------

dw = readRDS(file = "data/dw_fixed.rds") %>% 
  mutate(tank = as.integer(tank)) %>% 
  filter(correct_taxon != "Daphnia")
=======

# model isd ---------------------------------------------------------------

dw = readRDS(file = "data/dw_fixed.rds")
>>>>>>> 505ce3cf1f0066d7de966ba229d2bf5a88180f36

fit_nodaphnia = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat + (1|tank), 
          data = dw ,
          stanvars = stanvars,  # new thing added by the package
          family = paretocounts(), # new thing added by the package
          chains = 4, iter = 2000,
          file = "models/brm_fit_nodaphnia.rds",
          prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                    prior(normal(0, 0.2), class = "b")))

saveRDS(fit_nodaphnia, file = "models/fit_nodaphnia_tank.rds")

# check model -------------------------------------------------------------

summary(fit_nodaphnia)

<<<<<<< HEAD
conditional_effects(fit)


saveRDS(fit_daphnia, file = "models/fit_nodaphnia_tank.rds")




raw_isd_data = fit_nodaphnia_tank$data %>% 
  group_by(tank) %>% 
  arrange(tank, -dw_mg) %>% 
  mutate(n_yx = 1:max(row_number())) %>% 
  mutate(trt_both = paste0(temp_treat, ",", nutrient_treat))

tank_rep = raw_isd_data %>% 
  distinct(trt_both, tank) %>% 
  arrange(trt_both, tank) %>% 
  group_by(trt_both) %>% 
  mutate(tank_rep = 1:max(row_number()))

lambda_posts = fit_nodaphnia_tank$data %>% 
  select(-dw_mg) %>% 
  distinct() %>% 
  add_epred_draws(fit_nodaphnia_tank, re_formula = NULL)

lambda_posts_summary = lambda_posts %>% 
  group_by(tank, xmin, xmax, temp_treat, nutrient_treat) %>% 
  median_qi(.epred) %>% 
  pivot_longer(cols = c(.epred, .lower, .upper)) %>% 
  rename(.epred = value)

dat_grid = fit_nodaphnia_tank$data %>%
  # filter(tank == 1) %>% 
  select(-dw_mg) %>% 
  distinct() %>% 
  group_by(tank) %>% 
  group_split()

dat_grid_expanded = lapply(dat_grid, function(df){
  df %>% 
    expand_grid(x = 10^seq(log10(xmin), log10(xmax), length.out = 50)) %>% 
    left_join(lambda_posts_summary) %>% 
    mutate(prob_yx = (1 - (x^(.epred + 1) - (xmin^(.epred + 
                                                     1)))/((xmax)^(.epred + 1) - (xmin^(.epred + 1))))) %>% 
    rename(lambda = .epred)
})

isd_posts = bind_rows(dat_grid_expanded) %>%
  left_join(raw_isd_data %>% group_by(tank) %>% tally()) %>% 
  select(-lambda) %>% 
  mutate(prob_yx = prob_yx*n) %>% 
  pivot_wider(names_from = name, values_from = prob_yx) %>% 
  left_join(tank_rep)

isd_posts %>% 
  ggplot(aes(x = x, y = .epred)) +
  geom_point(data = raw_isd_data %>% left_join(tank_rep), 
             aes(x = dw_mg, y = n_yx, size = dw_mg, color = trt_both),
             shape = 21,
             alpha = 0.3) +
    geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper)) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~tank) +
  coord_cartesian(ylim = c(1, NA)) +
  facet_grid(trt_both~ tank_rep) +
  guides(color = "none") +
  theme_default()


# conditional effects plot ------------------------------------------------
lambda_posts

treatment_posts = fit_nodaphnia_tank$data %>% 
  select(-dw_mg) %>% 
  distinct() %>% 
  add_epred_draws(fit_nodaphnia_tank, re_formula = NA) 


lambda_posts %>% 
  left_join(lambda_posts) %>% 
  ggplot(aes(x = interaction(temp_treat, nutrient_treat), 
             y = .epred)) +
  geom_boxplot(data = treatment_posts,
              aes(fill = interaction(temp_treat, nutrient_treat)),
              outlier.shape = NA) + 
  stat_pointinterval(aes(group = tank),
                     .width = 0.95,
                     linewidth = 0.2,
                     position = position_jitter(width = 0.08),
                     color = "black") +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  theme_default()





=======
plot_fit = plot(conditional_effects(fit_nodaphnia, effects = "temp_treat:nutrient_treat"))

saveRDS(plot_fit, file = "plots/plot_fit.rds")

plot_fit = readRDS(file = "plots/plot_fit.rds")

plot_fit$`temp_treat:nutrient_treat`$data %>% 
  ggplot(aes(x = temp_treat, y = estimate__, color = nutrient_treat)) + 
  geom_point(position = position_dodge(width = .2)) +
  geom_linerange(aes(ymin = lower__, ymax = upper__),
                 position = position_dodge(width = .2)) +
  labs(y = "asdfasdf") +
  theme_default()
>>>>>>> 505ce3cf1f0066d7de966ba229d2bf5a88180f36
