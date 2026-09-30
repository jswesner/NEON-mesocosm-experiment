library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(brms::theme_default(base_family = "bayesplotbase_family"))
source('code/custom_functions.r')
source('code/custom_functions/pparetocounts_vec.r')

# get differences between predicted and observed p(X>=x). This measures residuals and shows where the data do not match the model predictions/powerlaw

fit_isd = readRDS("models/fit_isd_morin_gpp_predictor.rds")
treatments = readRDS("data/treatments.rds")

d = fit_isd$data |> 
  left_join(treatments |> distinct(temp_treat, temp_plus))

isd_preds_bytank = d %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg), xmax = max(dw_mg)) %>%
  distinct(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank, temp_plus) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(fit_isd, re_formula = NULL, ndraws = 500)

isd_lines_bytank = isd_preds_bytank %>% 
  group_by(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank, temp_plus, .draw) %>% 
  filter(.draw <= 100) |> 
  expand_grid(interval = 1:30) %>% 
  group_by(temp_treat, june_28_gpp_s, june_22_gpp_s, tank, temp_plus, .draw) %>% 
  mutate(x = xmin + (xmax - xmin) * (seq(0, 1, length.out = max(interval)))^10) %>%
  rowwise() |> 
  mutate(prob_y = pparetocounts(x = x, xmin = xmin, xmax = xmax, lambda = .epred)) %>%
  ungroup %>% 
  left_join(d %>% group_by(tank) %>% reframe(max_y = max(row_number()))) |> 
  left_join(treatments |> distinct(temp_treat, temp_plus))

raw_data = readRDS(file = "data/dat_morin_xmins.rds") %>% 
  arrange(tank, -dw_mg) %>% 
  group_by(tank) %>% 
  mutate(y = row_number()/max(row_number())) %>% 
  left_join(treatments |> distinct(temp_treat, temp_plus)) 

preds_per_size = isd_preds_bytank %>%
  group_by(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank, temp_plus, .draw) |>
  filter(.draw <= 100) |>
  right_join(raw_data |> select(tank, dw_mg, y, correct_taxon), relationship = "many-to-many") |>
  # rowwise() |>
  mutate(prob_y = pparetocounts_vec(x = dw_mg, xmin = xmin, xmax = xmax, lambda = .epred)) %>%
  ungroup %>%
  left_join(d |> count(tank, name = "max_y")) |>
  group_by(tank) |> 
  filter(dw_mg < xmax) # do this b/c the prob at max size is always 0 so it doesn't say anything about the skews

logdiffs = preds_per_size |> 
  filter(.draw <= 100) |> 
  mutate(diff_logprob = log(y) - log(prob_y),
         diff_prob = y - prob_y,
         abs_diff_logprob = abs(diff_logprob),
         abs_diff_prob = abs(diff_prob))

logdiffs |> 
  ggplot(aes(x = diff_logprob, group = tank)) + 
  ggridges::geom_density_ridges(aes(y = tank)) +
  facet_wrap(~temp_plus) +
  geom_vline(xintercept = 0)

logdiffs_summary = logdiffs |> 
  group_by(tank, dw_mg, temp_plus, correct_taxon) |> 
  median_qi(diff_logprob) 
  

# which large taxon is the max deviation

logdiffs_summary |>
  filter(dw_mg > 0.1) |>
  group_by(correct_taxon, tank, temp_plus) |> 
  filter(diff_logprob == max(abs(diff_logprob))) |> 
  group_by(temp_plus, correct_taxon) |> 
  tally()

logdiffs |>
  ungroup() |> 
  # sample_n(20000) |> 
  filter(.draw == sample(unique(logdiffs$.draw, size = 1))) |> 
  ggplot(aes(x = dw_mg, y = diff_logprob, color = temp_plus)) +
  geom_point(size = 0.5) +
  scale_color_colorblind() +
  scale_x_log10() +
  facet_wrap(~tank, ncol = 4) +
  geom_hline(yintercept = 0)

breaks <- c(signif_half_up(2^(-3:2), 1), Inf)

log_difference = preds_per_size |> 
  filter(.draw <= 1000) |> 
  mutate(diff_logprob = log(y) - log(prob_y),
         diff_prob = y - prob_y,
         abs_diff_logprob = abs(diff_logprob),
         abs_diff_prob = abs(diff_prob)) |> 
  mutate(log10_bin = cut(dw_mg,
      breaks = breaks,
      include.lowest = TRUE)) |> 
  group_by(tank, .draw, temp_plus, log10_bin) |> 
  reframe(diff_logprob = mean(diff_logprob)) |> 
  arrange(tank) |> 
  filter(!is.na(log10_bin))

residual_plot = log_difference |> 
  group_by(.draw, temp_plus, log10_bin) |> 
  reframe(diff_logprob = mean(diff_logprob)) |> 
  ggplot(aes(x = log10_bin, y = diff_logprob)) +
  stat_pointinterval(data = log_difference,
                     # color = "black",
                     aes(color = temp_plus, group = interaction(tank, log10_bin)),
                     # position = position_jitter(width = 0.02),
                     alpha = 0.1,
                     # normalize = "groups",
                     scale = 0.2,
                     size = 0.2) +
  geom_line(data = . %>% filter(.draw <= 300),
            aes(group = interaction(temp_plus, .draw), color = temp_plus),
            alpha = 0.1,
            linewidth = 0.05) +
  stat_halfeye(aes(fill = temp_plus,
                   color = temp_plus),
               alpha = 0.6,
               position = position_nudge(0.0)) +
  scale_y_continuous(breaks = -2:3) +
  geom_hline(yintercept = 0) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  labs(y = "ISD residuals (log ratio)",
       x = "mgDM log2 intervals",
       fill = "",
       color = "") +
  annotate(x = 0.6, y = 2, label = "More common than expected from power law",
           geom = "text", hjust = 0,
           family = "sans", 
           size = 3) +
  annotate(x = 0.6, y = -2, label = "Less common than expected from power law",
           geom = "text", hjust = 0,
           family = "sans", size = 3) +
  theme(legend.position = "top")

ggsave(residual_plot, file = "plots/residual_plot.jpg", width = 6, height = 6, dpi = 400)


log_difference |> 
  group_by(.draw, temp_plus, log10_bin) |> 
  reframe(diff_logprob = mean(diff_logprob)) |> 
  group_by(log10_bin, temp_plus) |> 
  median_qi(exp(diff_logprob))

log_difference |> 
  left_join(d |> distinct(tank, june_28_gpp_s, june_22_gpp_s)) |> 
  ggplot(aes(x = june_28_gpp_s, y = diff_logprob)) + 
  geom_point(shape = ".") +
  facet_wrap(log10_bin~temp_plus) +
  geom_smooth(method = lm)
