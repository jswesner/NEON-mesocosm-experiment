library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())
source('code/custom_functions.r')

# load model
fit_isd = readRDS("models/fit_isd_clauset.rds")

treatments = fit_isd$data %>% distinct(temp_treat, nutrient_treat) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

renumber = treatments %>% 
  left_join(fit_isd$data %>% distinct(temp_treat, nutrient_treat, tank)) %>% 
  group_by(temp_nut_plus) %>% 
  mutate(new_tank = row_number())

# posts individual tanks ---------------------------------
post_dots = fit_isd$data %>% 
  select(-dw_mg) %>% 
  distinct()  %>% 
  left_join(treatments) %>% 
  add_epred_draws(fit_isd, re_formula = NULL)

# posts treatments ---------------------------------
post_lines = fit_isd$data %>% 
  select(-dw_mg, -tank) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  group_by(temp_treat, nutrient_treat, .draw) %>% 
  reframe(.epred = mean(.epred))  %>% # average over different xmins and xmaxs
  left_join(treatments)

# plots ---------------------------------
# plot_epreds = post_dots %>% 
#   group_by(tank, temp_nut_plus, temp_treat, nutrient_treat, nut_plus, temp_plus) %>% 
#   median_qi(.epred) %>% 
#   ggplot(aes(x = temp_plus, y = .epred)) +
#   geom_point(shape = 1,
#     # aes(color = nut_plus)
#              ) +
#   stat_pointinterval(data = post_lines, aes(color = nut_plus)) +
#   scale_color_colorblind() +
#   scale_fill_colorblind() +
#   facet_wrap(~nut_plus) +
#   geom_line(data = post_lines %>% filter(.draw <= 1000),
#             aes(group = .draw, color = nut_plus),
#             alpha =0.03) +
#   guides(color = "none") +
#   labs(x = "Temperature",
#        y = "\u03bb") +
#   NULL

plot_epreds = post_dots %>%
  group_by(tank, temp_nut_plus, temp_treat, nutrient_treat, nut_plus, temp_plus) %>%
  median_qi(.epred) %>%
  ggplot(aes(x = nut_plus, y = .epred)) +
  geom_point(shape = 1,
             size = 0.5
             # aes(color = nut_plus)
  ) +
  stat_pointinterval(data = post_lines, aes(color = nut_plus), alpha = 0.5) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  facet_wrap(~temp_plus) +
  geom_line(data = post_lines %>% filter(.draw <= 1000),
            aes(group = .draw, color = nut_plus),
            alpha =0.01) +
  guides(color = "none") +
  labs(x = "Nutrients",
       y = "\u03bb") +
  NULL

ggsave(plot_epreds, file = "plots/plot_epreds.jpg", width = 6, height = 4, dpi = 400)


# plot isd ----------------------------------------------------------------
pparetocounts = function(x, xmin, xmax, lambda){(1 - (x^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1))))}
dw_fixed = readRDS(file = "data/dw_fixed.rds")

isd_preds = dw_fixed %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg), xmax = max(dw_mg)) %>% 
  group_by(temp_treat, nutrient_treat) %>% 
  mutate(xmin = mean(xmin), xmax = mean(xmax)) %>% 
  distinct(temp_treat, nutrient_treat, xmin, xmax) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(fit_isd, re_formula = NA)

isd_lines = isd_preds %>% 
  group_by(temp_treat, nutrient_treat, xmin, xmax) %>% 
  median_qi(.epred) %>%
  select(-.width, -.point, -.interval) %>% 
  pivot_longer(cols = starts_with("."),
               values_to = "lambda") %>% 
  expand_grid(interval = 1:50) %>% 
  group_by(name, temp_treat, nutrient_treat) %>% 
  mutate(x = seq(min(xmin), max(xmax), length.out = max(interval))) %>% 
  mutate(prob_y = pparetocounts(x = x, xmin = xmin, xmax = xmax, lambda = lambda)) %>%
  ungroup 

isd_lines %>% 
  select(-interval, -lambda) %>% 
  pivot_wider(names_from = name, values_from = prob_y) %>% 
  ggplot(aes(x = x, y = .epred)) + 
  geom_lineribbon(aes(group = interaction(temp_treat, nutrient_treat),
                      ymin = .lower, ymax = .upper), 
                  linewidth = 0.2, alpha = 0.5) +
  facet_wrap(temp_treat ~ nutrient_treat, ncol = 1) +
  scale_x_log10() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.5e-05, NA)) +
  NULL


# isd lines by tank
isd_plot = make_isd_plots(fit_isd)
isd_plot + scale_y_log10()


# plot xmin ---------------------------------------------------------------
fit_isd$data %>% 
  left_join(renumber) %>% 
  distinct(tank, new_tank, temp_nut_plus, xmin, xmax) %>% 
  ggplot(aes(x = temp_nut_plus, y = xmin)) +
  geom_point()

# compare different data culling ------------------------------------------

fit_edwards_cull = readRDS("models/old/fit_isd.rds")
fit_clauset_cull = readRDS("models/fit_isd_clauset.rds")

conditional_effects(fit_edwards_cull)
conditional_effects(fit_clauset_cull)



# plots for power law -----------------------------------------------------
ks_stats = readRDS(file = "models/ks_stats.rds") %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

dat = readRDS(file = "data/dw_fixed.rds") %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

ks_stats %>% 
  ggplot(aes(x = temp_nut_plus, y = ks_stat)) +
  geom_point()

dat %>% 
  left_join(fit_isd$data %>% distinct(xmin, xmax, tank) %>% 
              rename(xmin_clauset = xmin,
                     xmax_clauset = xmax)) %>% 
  left_join(renumber) %>% 
  mutate(power_law = case_when(dw_mg <= xmin_clauset ~ "Not Power Law",
                               TRUE ~ "Power Law")) %>% 
  ggplot(aes(x = tank, y = dw_mg, color = power_law,
             shape = power_law)) + 
  geom_jitter(size = 0.5) +
  facet_grid(~ temp_nut_plus) +
  scale_color_colorblind() +
  scale_shape_manual(values = c(1, 16)) +
  # scale_y_log10() +
  NULL
  
dat_fixed_power = dat %>% 
  left_join(fit_isd$data %>% distinct(xmin, xmax, tank) %>% 
              rename(xmin_clauset = xmin,
                     xmax_clauset = xmax)) %>% 
  left_join(renumber) %>% 
  mutate(power_law = case_when(dw_mg <= xmin_clauset ~ "Not Power Law",
                               TRUE ~ "Power Law")) %>% 
  arrange(tank, -dw_mg) %>% 
  group_by(tank) %>% 
  mutate(y = row_number()/max(row_number()))

dat_fixed_power %>% 
  ggplot() +
  geom_point(aes(x = dw_mg, y = y, color = power_law,
                 alpha = power_law),
             shape = 1, 
             size = 0.6) +
  ggh4x::facet_grid2(temp_nut_plus~new_tank) +
  scale_x_log10() +
  scale_y_log10() +
  # coord_cartesian(ylim = c(0.5e-02, NA)) +
  scale_color_colorblind() +
  scale_alpha_manual(values = c(0.1, 1)) +
  guides(color = "none") +
  NULL

dat_fixed_power %>% 
  group_by(tank, temp_nut_plus) %>% 
  add_tally() %>% 
  group_by(tank, temp_nut_plus, power_law, n) %>% 
  tally() %>% 
  mutate(prop = nn/n) %>% 
  filter(power_law == "Not Power Law") %>% 
  ggplot(aes(x = temp_nut_plus, y = prop)) +
  geom_point()



# summaries ---------------------------------------------------------------

post_dots %>% 
  group_by(tank, temp_treat, nutrient_treat) %>% 
  median_qi(.epred) %>% 
  filter(.epred == max(.epred)|.epred == min(.epred))

post_lines %>% 
  median_qi(.epred)

post_lines %>% 
  group_by(temp_treat, nutrient_treat) %>% 
  median_qi(.epred)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(temp_treat) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(temp_treat) %>% 
  reframe(prob_greater = sum(diff >0)/max(.draw))

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  select(-ambient, -nutrient) %>% 
  pivot_wider(names_from = temp_treat, values_from = diff) %>% 
  mutate(diff_interaction = heated - ambient) %>% 
  median_qi(diff_interaction)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  select(-ambient, -nutrient) %>% 
  pivot_wider(names_from = temp_treat, values_from = diff) %>% 
  mutate(diff_interaction = heated - ambient) %>% 
  reframe(prob_greater = sum(diff_interaction > 0)/max(.draw))

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  group_by(nutrient_treat) %>% 
  median_qi(diff)

post_lines %>% 
  select(-ends_with("plus")) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  group_by(nutrient_treat) %>% 
  reframe(prob_greater = sum(diff>0)/max(.draw))




# larval mean size --------------------------------------------------------

brm_mean_larvae = readRDS(file = "models/brm_mean_larvae_rawdata_lognorm.rds")

raw_sizes = brm_mean_larvae$data %>%
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

posts_mean_larvae_tank = brm_mean_larvae$data %>% as_tibble() %>% 
  select(-dw_mg) %>% 
  distinct() %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>%
  add_epred_draws(brm_mean_larvae, re_formula = NULL) 

posts_mean_larvae_tank %>% 
  group_by(tank) %>% 
  mutate(median = median(.epred)) %>% 
  ggplot(aes(x = tank, y = .epred)) + 
  stat_pointinterval(, color = "red") +
  geom_point(data = brm_mean_larvae$data, aes(y = dw_mg)) 

posts_mean_larvae_trt = brm_mean_larvae$data %>% as_tibble() %>% 
  select(-dw_mg, -tank) %>% 
  distinct() %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>%
  add_linpred_draws(brm_mean_larvae, re_formula = NA) %>%  # doing this instead of epred b/c lognormal median is exp(mu), but epred gives lognormal mean which is exp(mu + (sigma^2/2))
  mutate(.epred = exp(.linpred))

plot_individual_mass = posts_mean_larvae_trt %>% 
  ggplot(aes(y = temp_nut_plus, x = exp(.linpred))) +
  # geom_jitter(data = raw_sizes,
  #             aes(x = dw_mg, size = dw_mg),
  #             height = 0.0,
  #             width = 0) +
  stat_halfeye() +
  # scale_x_log10() +
  labs(x = "Individual Mass (mgDM)") +
  geom_hline(aes(yintercept = 0.05)) +
  theme(axis.title.y = element_blank()) +
  NULL

ggsave(plot_individual_mass, file = "plots/plot_individual_mass.jpg", 
       width = 5, height = 5, dpi = 400)

posts_mean_larvae_trt %>% 
  mutate(trt = paste0(temp_treat, "_", nutrient_treat)) %>% 
  ungroup %>% 
  select(trt, .draw, .epred) %>% 
  pivot_wider(names_from = trt, values_from = .epred) %>% 
  pivot_longer(cols = -c(.draw, heated_ambient)) %>% 
  mutate(diff = heated_ambient - value) %>% 
  group_by(name) %>% 
  median_qi(diff)

posts_mean_larvae_trt %>% 
  mutate(trt = paste0(temp_treat, "_", nutrient_treat)) %>% 
  ungroup %>% 
  select(trt, .draw, .epred) %>% 
  pivot_wider(names_from = trt, values_from = .epred) %>% 
  pivot_longer(cols = -c(.draw, heated_ambient)) %>% 
  mutate(diff = value - heated_ambient) %>% 
  group_by(name) %>% 
  reframe(prob = sum(diff<0)/max(.draw)) 
  

