library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(brms::theme_default(base_family = "bayesplotbase_family"))
source('code/custom_functions.r')
source('code/custom_functions/pparetocounts_vec.r')

fit_isd = readRDS("models/fit_isd_morin_gpp_predictor.rds")
treatments = readRDS("data/treatments.rds")

d = fit_isd$data |> 
  left_join(treatments |> distinct(temp_treat, temp_plus))

gpp_range = c(-1, 0, 1)

post_temp_all = d |> 
  distinct(temp_treat, temp_plus) |> 
  expand_grid(june_28_gpp_s = gpp_range) |>
  mutate(june_22_gpp_s = 0) |> 
  mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
  mutate(
    gpp_cat = case_when(
      gpp_sum < 0 ~ "'a)'~GPP[Low]",
      gpp_sum > 0 ~ "'b)'~GPP[High]",
      gpp_sum == 0 ~ "'c)'~GPP[Medium]")) |> 
  mutate(gpp_cat = as.factor(gpp_cat)) |> 
  mutate(xmin = 0.01, xmax = 1000, counts = 1) |> 
  add_epred_draws(fit_isd, re_formula = NA)

post_temp_lowmedhigh = tibble(
  june_22_gpp_s = 0, 
  june_28_gpp_s = gpp_range, 
  gpp_cat = c("'a)'~GPP[Low]", "'b)'~GPP[Medium]", "'c)'~GPP[High]")) |> 
  mutate(gpp_cat = as.factor(gpp_cat)) |> 
  expand_grid(temp_treat = unique(d$temp_treat)) |> 
  left_join(treatments |> distinct(temp_treat, temp_plus)) |> 
  mutate(xmin = 0.01, xmax = 1000, counts = 1) |> 
  add_epred_draws(fit_isd, re_formula = NA)


lambda_temp_plot = post_temp_lowmedhigh |> 
  ggplot(aes(x = temp_plus, y = .epred,
             color = temp_plus)) +
  geom_line(data = . %>% filter(.draw <= 300),
            aes(group = .draw),
            alpha = 0.2,
            linewidth = 0.05) +
  stat_pointinterval() +
  facet_grid(~gpp_cat, labeller = label_parsed) +
  scale_color_colorblind() +
  guides(color = "none") +
  labs(y = "\u03bb",
       x = "Temperature Treatment")

ggsave(lambda_temp_plot, file = "plots/lambda_temp_plot.jpg", width = 6, height = 3)


post_temp_lowmedhigh |> 
  ungroup() |> 
  select(temp_treat, starts_with("gpp_"), .draw, .epred) |> 
  pivot_wider(names_from = temp_treat, values_from = .epred) |> 
  mutate(diff = heated - ambient) |> 
  group_by(gpp_cat) |> 
  median_qi(diff)

post_temp_lowmedhigh |> 
  ungroup() |> 
  select(temp_treat, starts_with("gpp_"), .draw, .epred) |> 
  group_by(gpp_cat, temp_treat) |> 
  median_qi(.epred)

post_temp_lowmedhigh |> 
  ungroup() |> 
  select(temp_treat, starts_with("gpp_"), .draw, .epred) |> 
  pivot_wider(names_from = temp_treat, values_from = .epred) |> 
  mutate(diff = heated - ambient) |> 
  group_by(gpp_cat) |> 
  reframe(prob = sum(diff > 0)/max(.draw))

# isd plots ---------------------------------------------------------------
model = fit_isd

isd_preds_bytank = model$data %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg), xmax = max(dw_mg)) %>%
  distinct(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(model, re_formula = NULL)

isd_lines_bytank = isd_preds_bytank %>% 
  group_by(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank) %>% 
  median_qi(.epred) %>%
  select(-.width, -.point, -.interval) %>% 
  pivot_longer(cols = starts_with("."),
               values_to = "lambda") %>% 
  expand_grid(interval = 1:30) %>% 
  group_by(name, temp_treat, june_28_gpp_s, june_22_gpp_s, tank) %>% 
  # mutate(x = seq(min(xmin), max(xmax), length.out = max(interval))) %>%
  mutate(x = xmin + (xmax - xmin) * (seq(0, 1, length.out = max(interval)))^10) %>%
  mutate(prob_y = pparetocounts_vec(x = x, xmin = xmin, xmax = xmax, lambda = lambda)) %>%
  ungroup %>% 
  left_join(d %>% group_by(tank) %>% reframe(max_y = max(row_number()))) |> 
  left_join(treatments |> distinct(temp_treat, temp_plus)) |> 
  select(-interval, -lambda) %>% 
  pivot_wider(names_from = name, values_from = prob_y) %>% 
  mutate(treatment = paste(temp_treat, june_28_gpp_s, june_22_gpp_s, sep = '_')) 


raw_data = readRDS("data/dat_morin_xmins.rds") %>% 
  arrange(tank, -dw_mg) %>% 
  group_by(tank) %>% 
  mutate(y = row_number()/max(row_number())) %>% 
  left_join(treatments |> distinct(temp_treat, temp_plus)) 

ambient = isd_lines_bytank |>
  filter(temp_treat == "ambient") |> 
  ggplot(aes(x = x, y = .epred)) +
  geom_lineribbon(aes(group = interaction(temp_treat, june_28_gpp_s, june_22_gpp_s, tank),
                      ymin = .lower, ymax = .upper), 
                  linewidth = 0.2, fill = "grey80") +
  geom_point(data = raw_data|>
               filter(temp_treat == "ambient") , aes(x = dw_mg, y = y),
             shape = 1,
             color = "black",
             size = 0.6) +
  ggh4x::facet_grid2(tank ~ temp_plus) +
  scale_x_log10() +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.5e-02, NA)) +
  guides(color = "none") +
  labs(y = "Pr (X \u2265 x)",
       x = "mgDM") +
  theme(strip.text.y = element_blank()) +
  NULL

heated = isd_lines_bytank |>
  filter(temp_treat != "ambient") |> 
  ggplot(aes(x = x, y = .epred)) +
  geom_lineribbon(aes(group = interaction(temp_treat, june_28_gpp_s, june_22_gpp_s, tank),
                      ymin = .lower, ymax = .upper), 
                  linewidth = 0.2, fill = "#E69F00") +
  geom_point(data = raw_data|>
               filter(temp_treat != "ambient") , aes(x = dw_mg, y = y),
             shape = 1,
             color = "#E69F00",
             size = 0.6) +
  ggh4x::facet_grid2(tank ~ temp_plus) +
  scale_x_log10() +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.5e-02, NA)) +
  guides(color = "none") +
  labs(y = "Pr (X \u2265 x)",
       x = "mgDM") +
  theme(strip.text.y = element_blank()) +
  NULL

library(patchwork)

isd_plots = (ambient | heated) + 
  plot_layout(axes = "collect") 

ggsave(isd_plots, file = "plots/isd_plots.jpg", width = 5, height = 9)


# show taxa ---------------------------------------------------------------

isd_lines_bytank |>
  ggplot(aes(x = x, y = .epred)) +
  geom_lineribbon(aes(group = interaction(temp_treat, june_28_gpp_s, june_22_gpp_s, tank),
                      ymin = .lower, ymax = .upper), 
                  linewidth = 0.2, fill = "grey80") +
  geom_point(data = raw_data, 
             aes(x = dw_mg, y = y,
                 color = correct_taxon)) +
  ggh4x::facet_grid2(tank~temp_plus) +
  scale_x_log10() +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.5e-02, NA)) +
  # guides(color = "none") +
  labs(y = "Pr (X \u2265 x)",
       x = "mgDM") +
  theme(strip.text.y = element_blank()) +
  NULL

