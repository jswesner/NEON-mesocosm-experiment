library(poweRlaw)
library(tidyverse)
library(poweRlaw)
library(sizeSpectraFit)
library(ggridges)
library(tidybayes)
library(ggthemes)
library(brms)
theme_set(theme_default())

# plot ks values showing power law fit

treatments = readRDS("data/treatments.rds")
dat_raw = readRDS("data/dw_fixed.rds") %>% 
  left_join(treatments)
fit_isd_morin = readRDS("models/fit_isd_morin.rds")
ks_stats_culled_morin = readRDS(file = "models/ks_stats_culled_morin.rds") %>% 
  left_join(dat_raw %>% distinct(tank, temp_nut_plus))


tank_lambdas = fit_isd_morin$data %>% 
  distinct(tank, temp_treat, nutrient_treat) %>% 
  mutate(xmin = 0.01, xmax = 1000, counts = 1) %>% 
  left_join(ks_stats_culled_morin) %>% 
  add_epred_draws(fit_isd_morin)


tank_lambdas %>%
  ungroup %>% 
  group_by(tank, temp_nut_plus, ks_stat) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = ks_stat, y = .epred)) +
  geom_point(aes(color = temp_nut_plus))

tank_lambdas %>% 
  # filter(ks_stat <= 0.2) %>% 
  group_by(temp_nut_plus) %>% 
  median_qi(.epred)

ks_stats_culled_morin %>% 
  filter(ks_stat > 0.2)

# plot histograms of raw data showing xmin cutoffs
# load data

dat_clauset_xmins = readRDS(file = "data/dat_clauset_xmins.rds") # xmin is the geometric mean of 100 bootstrapped xmin estimates from XX_cull_xmin.R
bin_mins = readRDS(file = 'data/bin_mins.rds') # derived using Edwards method from 2026 paper. code is in XX_cull_xmin.R at the bottom.

treatments = readRDS("data/treatments.rds")

tanks_treatments  = dat_clauset_xmins %>% select(tank, contains("treat")) %>% 
  distinct() %>% left_join(treatments) %>% 
  arrange(temp_nut_plus) %>% 
  ungroup %>% 
  mutate(tank_id = rep(1:6, 4))

xmin_boots = bind_rows(readRDS("models/xmin_list.rds")) %>% group_by(tank) %>% 
  rename(xmin_clauset_raw = xmin_clauset)

tanks_treatments  = dat_clauset_xmins %>% select(tank, contains("treat")) %>% 
  distinct() %>% 
  left_join(treatments) %>% 
  left_join(xmin_boots %>% group_by(tank) %>% reframe(median = median(xmin_clauset_raw))) %>% 
  arrange(temp_nut_plus, median) %>% 
  ungroup %>% 
  mutate(tank_id = rep(1:6, 4))

# plot xmin sims -----------------------------------------------------------
trt_labels = tanks_treatments %>% filter(tank_id == 5)

xmin_cutoffs = xmin_boots %>%
  left_join(tanks_treatments) %>% 
  mutate(temp_nut_plus = as.factor(temp_nut_plus)) %>% 
  mutate(temp_nut_plus = fct_relevel(temp_nut_plus, "-heat | -nutrients", "-heat | +nutrients", "+heat | -nutrients")) %>% 
  ggplot(aes(x = xmin_clauset_raw, 
             y = interaction(tank_id, desc(temp_nut_plus)), 
             fill = nut_plus,
             color = nut_plus)) +
  geom_density_ridges(aes(group = tank, 
                          alpha = temp_plus,
                          scale = 0.9), 
                      color = "NA",
                      from = min(xmin_boots$xmin_clauset_raw)) +
  geom_point(shape = "|", size = 0.4) +
  scale_y_discrete(labels = rep(6:1, 4)) +
  scale_alpha_manual(values = c(0.3, 0.5)) +
  ggthemes::scale_fill_colorblind() +
  ggthemes::scale_color_colorblind() +
  geom_label(data = trt_labels, aes(x = 3.5, label = temp_nut_plus, alpha = temp_plus), 
             color = c("black", "white", "black", "white"),
             size = 3) +
  theme(legend.position = "none") +
  labs(y = "Replicate", 
       x = bquote(x[min] ~ "cutoff (mgDM)")) +
  # coord_cartesian(expand = c(1, 0)) +
  NULL

ggsave(xmin_cutoffs, file = "plots/xmin_cutoffs.jpg", width = 4, height = 5)



# variability of xmin -------------------------------------------------------------

summarize_xmin = xmin_boots %>% 
  left_join(tanks_treatments) %>%
  ungroup %>% 
  group_by(temp_nut_plus, tank) %>% 
  reframe(sd = sd(xmin_clauset_raw),
          median = median(xmin_clauset_raw),
          mad = mad(xmin_clauset_raw)) # median absolute deviation - for power law

summarize_xmin %>% 
  ggplot(aes(x = temp_nut_plus, y = mad)) + 
  geom_point(position = position_jitter(width = 0.02)) +
  geom_boxplot(outlier.shape = NA)

# plot histograms ---------------------------------------------------------


dat_unculled = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  # filter(correct_taxon != "Daphnia") %>% 
  left_join(dat_clauset_xmins %>% distinct(tank, xmin) %>% rename(xmin_clauset = xmin)) %>% 
  mutate(culled = case_when(dw_mg < xmin_clauset ~ "culled", TRUE ~ "not culled")) %>% 
  mutate(log_dw_mg = log10(dw_mg)) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

tank_rep = dat_unculled %>% ungroup %>% distinct(tank, treatment) %>% 
  arrange(tank) %>% 
  group_by(treatment) %>% 
  mutate(tank_rep = row_number()) 

dat_unculled %>% 
  left_join(tank_rep) %>% 
  ggplot(aes(x = dw_mg)) +
  geom_histogram(bins = 100) +
  # geom_density() +
  ggh4x::facet_grid2(tank_rep ~ temp_nut_plus) +
  scale_x_log10()

culled_labels = tibble(dw_mg = c(0.3e-03, 1e-00),
                       label = c("Undersampled for\na power law",
                                 "Not undersampled\nfor a power law"),
                       culled = c("culled", "not culled")) %>% 
  mutate(culled = fct_relevel(culled, "not culled"),
         y = c(0.6, 0.5),
         temp_nut_plus = "a) -heat | -nutrients")

culled_arrow_under =  tibble(x = 0.3e-03, 
                        y = 0.5, 
                        xend = 0.9e-03, 
                        yend = 0.3,
                        temp_nut_plus = "a) -heat | -nutrients")

culled_arrow_notunder =  tibble(x = 1e-00, 
                             y = 0.4, 
                             xend = 0.6, 
                             yend = 0.25,
                             temp_nut_plus = "a) -heat | -nutrients")

median_xmins = xmin_boots %>% 
  left_join(tanks_treatments) %>%
  ungroup %>% 
  group_by(temp_nut_plus) %>% 
  reframe(sd = sd(xmin_clauset_raw),
          median = median(xmin_clauset_raw),
          mad = mad(xmin_clauset_raw)) %>% 
  mutate(xmin_clauset_median = median,
         xmin_mad = mad)

dat_unculled_withxmin = dat_unculled %>% 
  group_by(temp_nut_plus) %>% 
  left_join(median_xmins) %>% 
  mutate(culled = case_when(dw_mg < xmin_clauset_median ~ "culled", TRUE ~ "not culled"),
         culled = fct_relevel(culled, "not culled")) %>%  
  mutate(temp_nut_plus = case_when(temp_nut_plus == "-heat | -nutrients" ~ "a) -heat | -nutrients",
                                   temp_nut_plus == "-heat | +nutrients" ~ "b) -heat | +nutrients",
                                   temp_nut_plus == "+heat | -nutrients" ~ "c) +heat | -nutrients",
                                   T ~ "d) +heat | +nutrients"))

xmin_labels = dat_unculled_withxmin %>% ungroup %>% distinct(temp_nut_plus, xmin_clauset_median, xmin_mad) %>% 
  mutate(xmin_clauset_median = janitor::signif_half_up(xmin_clauset_median, digits = 1),
         xmin_mad = janitor::signif_half_up(xmin_mad, digits = 1),
         dw_mg = xmin_clauset_median,
         label = paste0(xmin_clauset_median, " (", xmin_mad, ")"),
         y = 1)

bin_width = 0.08

test_df = dat_unculled %>% 
  group_by(temp_nut_plus) %>% 
  mutate(bin = floor(log_dw_mg/bin_width)*bin_width,
         bin_mid_log = bin + bin_width/2,
         bin_max = 10^(bin + bin_width),
         bin_min = 10^(bin - bin_width),
         bin_mid = 10^bin_mid_log) %>% 
  left_join(median_xmins) %>% 
  group_by(bin_mid, bin_mid_log, temp_plus, nut_plus, temp_nut_plus, culled, xmin_clauset_median, bin_max) %>% 
  tally() %>% 
  # left_join(tank_rep) %>% 
  group_by(temp_nut_plus) %>%  
  mutate(temp_nut_plus = case_when(temp_nut_plus == "-heat | -nutrients" ~ "a) -heat | -nutrients",
                                   temp_nut_plus == "-heat | +nutrients" ~ "b) -heat | +nutrients",
                                   temp_nut_plus == "+heat | -nutrients" ~ "c) +heat | -nutrients",
                                   T ~ "d) +heat | +nutrients")) %>% 
  mutate(culled = case_when(bin_max <= xmin_clauset_median ~ "culled", T ~ "not culled")) %>% 
  group_by(temp_nut_plus) %>% 
  mutate(n_1 = n/max(n)) 


xmin_histogram = test_df %>% 
  ggplot(aes(x = bin_mid, y = n_1)) +
  scale_x_log10() +
  geom_segment(aes(group = bin_mid, yend = 0, 
                   color = culled), 
               linewidth = 1) +
  ggh4x::facet_wrap2(~ temp_nut_plus, ncol = 1) +
  geom_vline(aes(xintercept = xmin_clauset_median), linetype = "dotted") +
  brms::theme_default() +
  scale_alpha_manual(values = c(0.2, 0.9)) +
  scale_fill_grey() +
  scale_color_manual(values = c("grey80", "black")) +
  theme(strip.text = element_text(hjust = 0)) +
  guides(fill = "none", 
         color = "none") +
  labs(y = "Counts",
       x = "Individual Dry Mass (mg)") +
  geom_text(data = culled_labels, aes(label = label,
                                      x = dw_mg,
                                      y = y),
            color = c("grey40", "black"),
            size = 2.5) +
  geom_curve(data = culled_arrow_under,
             aes(x = x, 
                 y = y, 
                 xend = xend, 
                 yend = yend),
             arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
             color = "grey70",
             curvature = 0.3
  ) + 
  geom_curve(data = culled_arrow_notunder,
             aes(x = x, 
                 y = y, 
                 xend = xend, 
                 yend = yend),
             arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
             color = "black",
             curvature = -0.3
  ) + 
  geom_label(data = xmin_labels, aes(y = y, 
                                     x = dw_mg,
                                     label = label),
             size = 2.5, fill = "white", linewidth = 0) +
  NULL


xmin_histogram

ggsave(xmin_histogram, file = "plots/xmin_histogram.jpg", width = 4, height = 8)


# old --------------------------------------------------------------------


bin_width = 0.15

tank_rep = dat_unculled %>% ungroup %>% distinct(tank, treatment) %>% 
  arrange(tank) %>% 
  group_by(treatment) %>% 
  mutate(tank_rep = row_number())

test_df = dat_unculled %>% 
  group_by(tank) %>% 
  mutate(bin = floor(log_dw_mg/bin_width)*bin_width,
         bin_mid_log = bin + bin_width/2,
         bin_mid = 10^bin_mid_log) %>% 
  group_by(bin_mid, bin_mid_log, tank, temp_plus, nut_plus, temp_nut_plus, culled, xmin_clauset) %>% 
  tally() %>% 
  left_join(tank_rep) %>% 
  group_by(tank) %>% 
  mutate(max_mid = max(bin_mid),
         bin_mid_norm = bin_mid/max_mid, # normalize size range to 0-1. Allows quantifying the proportion of the range that is cut off

         xmin_clauset_norm = xmin_clauset/max_mid)  %>% 
  mutate(culled_norm = case_when(bin_mid_norm < xmin_clauset_norm ~ "culled", TRUE ~ "not culled")) # normalize culled cut off to match normalized size range to 0-1. Allows quantifying the proportion of the range that is cut off


xmin_histograms = test_df %>% 
  ggplot(aes(x = bin_mid, y = n)) +
  scale_x_log10() +
  geom_bar(stat = "identity", aes(fill = treatment, 
                                  # alpha = culled
                                  ),
           width = 0.2) +
  ggh4x::facet_grid2(tank_rep ~ temp_nut_plus) +
  geom_vline(aes(xintercept = xmin_clauset, color = treatment), linetype = "dotted") +
  brms::theme_default() +
  scale_alpha_manual(values = c(0.2, 0.9)) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  guides(fill = "none", 
         alpha = "none",
         color = "none") +
  labs(y = "Count", x = "Dry Mass (mg)")

# proportion of individuals removed due to culling
proportion_culled_boots = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  # filter(correct_taxon != "Daphnia") %>% 
  left_join(xmin_boots %>% distinct(tank, xmin_clauset_raw, rep) %>% rename(xmin_clauset = xmin_clauset_raw), relationship = "many-to-many") %>% 
  group_by(tank, rep) %>% 
  mutate(culled = case_when(dw_mg < xmin_clauset ~ "culled", TRUE ~ "not culled")) %>% 
  mutate(log_dw_mg = log10(dw_mg)) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  group_by(tank, rep, temp_nut_plus, temp_plus, nut_plus, culled) %>% 
  tally() %>% 
  group_by(tank, rep, temp_nut_plus, temp_plus, nut_plus) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(culled == "culled") %>% 
  left_join(tank_rep)

proportion_culled_boots %>% 
  group_by(tank, temp_nut_plus) %>% 
  reframe(mean = median(prop), 
          sd = sd(prop),
          upper = mean + 1.96*sd,
          lower = mean - 1.96*sd) %>% 
  arrange(-mean) %>% 
  mutate(tank_ordered = 1:nrow(.)) %>% 
  ggplot(aes(x = temp_nut_plus, y = mean, color = temp_nut_plus)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_color_colorblind()
  
proportion_culled_boots %>% 
  group_by(tank, temp_nut_plus) %>% 
  reframe(mean = median(prop)) %>%
  group_by(temp_nut_plus) %>%
  median_qi(prop)

dat_unculled %>% 
  ungroup %>% 
  group_by(temp_nut_plus, temp_plus, nut_plus, culled, tank) %>% 
  tally() %>% 
  ungroup %>% 
  arrange(temp_nut_plus, culled) %>%
  mutate(tank_ordered = rep(1:6, 8)) %>%  
  mutate(temp_nut_plus = case_when(temp_nut_plus == "-heat | -nutrients" ~ "a) -heat | -nutrients",
                                   temp_nut_plus == "-heat | +nutrients" ~ "b) -heat | +nutrients",
                                   temp_nut_plus == "+heat | -nutrients" ~ "c) +heat | -nutrients",
                                   T ~ "d) +heat|+nutrients")) %>%
  mutate(culled = fct_relevel(culled, "not culled")) %>% 
  ggplot(aes(x = tank_ordered, y = n,
             fill = temp_plus,
             alpha = culled)) +
  # geom_bar(position = 'fill', stat = "identity") +
  geom_col() +
  facet_wrap(~temp_nut_plus, ncol = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  theme_default() +
  theme(strip.text = element_text(hjust = 0)) +
  scale_fill_colorblind() +
  scale_alpha_manual(values = c(0.8, 0.2))
  
dat_unculled %>% 
  ungroup %>% 
  group_by(temp_nut_plus, temp_plus, nut_plus, culled, tank) %>% 
  tally() %>% 
  ungroup %>% 
  arrange(temp_nut_plus, culled) %>%
  mutate(tank_ordered = rep(1:6, 8)) %>%  
  mutate(temp_nut_plus = case_when(temp_nut_plus == "-heat | -nutrients" ~ "a) -heat | -nutrients",
                                   temp_nut_plus == "-heat | +nutrients" ~ "b) -heat | +nutrients",
                                   temp_nut_plus == "+heat | -nutrients" ~ "c) +heat | -nutrients",
                                   T ~ "d) +heat|+nutrients")) %>%
  mutate(culled = fct_relevel(culled, "not culled")) %>% 
  group_by(temp_nut_plus, culled, tank) %>%
  reframe(total = sum(n)) %>% 
  group_by(temp_nut_plus, tank) %>% 
  mutate(tank_total = sum(total)) %>% 
  mutate(proportion = total/tank_total) %>% 
  group_by(temp_nut_plus) %>%
  filter(culled == "not culled") %>% 
  median_qi(proportion)

proportion_culled_boots %>%
  group_by(tank) %>% 
  mutate(median = median(prop)) %>% 
  ggplot(aes(x = temp_nut_plus, y = prop)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0), size = 0.2) +
  geom_point(data = . %>% distinct(tank, temp_nut_plus, median), aes(y = median), size = 5, color = "red", shape = 21)


proportion_culled_boots %>%
  group_by(tank) %>% 
  mutate(median = median(prop)) %>% 
  ggplot(aes(y = prop, x = tank_rep)) +
  facet_wrap(~temp_nut_plus, nrow = 1) +
  geom_point(size = 0.2, aes(color = temp_nut_plus), position = position_jitter(width = 0.2)) +
  geom_point(data = . %>% distinct(tank, temp_nut_plus, median, tank_rep), aes(y = median,
                                                                     color = temp_nut_plus), size = 5, shape = 21) 

# proportion of the size range culled
dat_unculled %>% 
  ungroup %>% 
  group_by(temp_nut_plus, temp_plus, nut_plus, culled, tank) %>% 
  tally() %>% 
  ungroup %>% 
  arrange(temp_nut_plus, culled) %>%
  mutate(tank_ordered = rep(1:6, 8)) %>%  
  mutate(temp_nut_plus = case_when(temp_nut_plus == "-heat | -nutrients" ~ "a) -heat | -nutrients",
                                   temp_nut_plus == "-heat | +nutrients" ~ "b) -heat | +nutrients",
                                   temp_nut_plus == "+heat | -nutrients" ~ "c) +heat | -nutrients",
                                   T ~ "d) +heat|+nutrients")) %>%
  mutate(culled = fct_relevel(culled, "not culled")) %>% 
  group_by(temp_nut_plus, culled, tank) %>%
  reframe(total = sum(n)) %>% 
  group_by(temp_nut_plus, tank) %>% 
  mutate(tank_total = sum(total)) %>% 
  mutate(proportion = total/tank_total) %>% 
  group_by(temp_nut_plus) %>%
  filter(culled == "not culled") %>% 
  median_qi(proportion)

# re-do with bin_mins -----------------------------------------------------

dat_unculled_bin = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  # filter(correct_taxon != "Daphnia") %>% 
  left_join(bin_mins %>% distinct(tank, bin_min) %>% rename(xmin_clauset = bin_min)) %>% 
  mutate(culled = case_when(dw_mg < xmin_clauset ~ "culled", TRUE ~ "not culled")) %>% 
  mutate(log_dw_mg = log10(dw_mg)) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))


tank_rep = dat_unculled_bin %>% ungroup %>% distinct(tank, treatment) %>% 
  arrange(tank) %>% 
  group_by(treatment) %>% 
  mutate(tank_rep = row_number())


bin_width = unique(bin_mins$bin_width)

test_df_bin = dat_unculled_bin %>% 
  group_by(tank) %>% 
  mutate(bin = floor(log_dw_mg/bin_width)*bin_width,
         bin_mid_log = bin + bin_width/2,
         bin_mid = 10^bin_mid_log) %>% 
  group_by(bin_mid, bin_mid_log, tank, temp_plus, nut_plus, temp_nut_plus, culled, xmin_clauset) %>% 
  tally() %>% 
  left_join(tank_rep) %>% 
  group_by(tank) %>% 
  mutate(max_mid = max(bin_mid),
         bin_mid_norm = bin_mid/max_mid, # normalize size range to 0-1. Allows quantifying the proportion of the range that is cut off
         
         xmin_clauset_norm = xmin_clauset/max_mid)  %>% 
  mutate(culled_norm = case_when(bin_mid_norm < xmin_clauset_norm ~ "culled", TRUE ~ "not culled")) # normalize culled cut off to match normalized size range to 0-1. Allows quantifying the proportion of the range that is cut off


xmin_histograms_bin = test_df_bin %>% 
  ggplot(aes(x = bin_mid, y = n)) +
  scale_x_log10() +
  geom_bar(stat = "identity", aes(fill = treatment, alpha = culled)) +
  ggh4x::facet_grid2(tank_rep ~ temp_nut_plus) +
  geom_vline(aes(xintercept = xmin_clauset, color = treatment), linetype = "dotted") +
  brms::theme_default() +
  scale_alpha_manual(values = c(0.2, 0.9)) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  guides(fill = "none", 
         alpha = "none",
         color = "none") +
  labs(y = "Count", x = "Dry Mass (mg)")


test_df_bin %>% 
  group_by(temp_nut_plus, culled_norm) %>% 
  tally() %>% 
  group_by(temp_nut_plus) %>% 
  mutate(total = sum(n)) %>% 
  mutate(n/total) %>% 
  filter(culled_norm == "culled")
