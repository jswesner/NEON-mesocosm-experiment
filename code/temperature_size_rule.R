library(tidyverse)
library(lubridate)
library(ggthemes)
library(janitor)
library(tidybayes)
library(brms)
library(ggrepel)
library(ggforce)
theme_set(theme_default())


# Temperature Size Rule 

# H1: Earlier emergence ---------------------------------------------------


# Test if insects emerge earlier and at smaller sizes when heated

# 1) load

emergence = readRDS(file = "data/emergence_wrangled.rds")

mean_g_day = mean(emergence$g_day, na.rm = T)
dragon_free_mean_g_day = mean(emergence$dragon_free_g_day, na.rm = T)
mean_day_s = mean(emergence$day)
sd_day_s = sd(emergence$day)
mean_ind_mgdw = mean(emergence$mean_individual_mgdw, na.rm = T)
mean_ind_perday = mean(emergence$no_ind_perday, na.rm = T)
day_1 = julian(as.Date("2022-05-23"))

# 2) fit model - when do most individuals emerge?
# emerge_smooth_ind = brm(no_ind_perday_s ~ s(day_s, by = treatment, k = 5) + (1|treatment), # response is mean centered and + 1 to account for zeros
#                         family = Gamma(link = "log"),
#                         data = emergence,
#                         prior = c(prior(normal(0, 1), class = "Intercept"), # prior is from Henry et al. 2019(?) of ~50-100 individuals per m2 per day in the same mesocosms
#                                   prior(normal(0, 1), class = "b"),
#                                   prior(exponential(2), class = "sd")),
#                         chains = 4, iter = 2000, cores = 4,
#                         data2 = list(mean_ind_perday = mean_ind_perday))
# 
# saveRDS(emerge_smooth_ind, file = "models/emerge_smooth_ind.rds")

emerge_smooth_ind = readRDS(file = "models/emerge_smooth_ind.rds")


#3) plot

emerge_dat = emerge_smooth_ind$data %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F)

emerge_posts_ind = tibble(day_s = seq(min(emerge_smooth_ind$data$day_s),
                                      max(emerge_smooth_ind$data$day_s),
                                      length.out = 30)) %>% 
  mutate(day = (day_s*sd_day_s) + mean_day_s,
         date = as.Date(day + day_1),
         mean_ind_perday = mean_ind_perday) %>% 
  expand_grid(treatment = unique(emerge_smooth_ind$data$treatment)) %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  add_epred_draws(emerge_smooth_ind) %>% 
  mutate(.epred = (.epred*mean_ind_perday)/0.36)

mod_dat = emerge_smooth_ind$data %>% 
  mutate(.epred = (no_ind_perday_s*mean_ind_perday)/0.36)  %>% 
  mutate(day = (day_s*sd_day_s) + mean_day_s,
         date = as.Date(day + day_1),
         mean_ind_perday = mean_ind_perday) %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

emerge_posts_summary = emerge_posts_ind %>% 
  group_by(day, heat, nut_plus) %>% 
  median_qi(.epred, .width = c(0.5, 0.95)) 

labels_plot_ind = emerge_posts_summary %>% 
  filter(.width == 0.5) %>% 
  filter(nut_plus == "-nutrients") %>% 
  filter(day == min(day) & heat == "heated" | .epred == max(.epred) & heat != "heated") %>% 
  mutate(label = case_when(heat == "heated" ~ "+heat",
                           T ~ "-heat"))

minor_log_breaks = seq(100, 2000, by = 100)

(
emerge_plot_ind = emerge_posts_summary %>% 
  ggplot(aes(x = day, y = .epred, color = heat, fill = heat)) +
  geom_pointinterval(data = . %>% filter(.width == 0.5), 
                     aes(ymin = .lower,
                         ymax = .upper), linewidth = 2,
                     size = 0.3) +
  geom_pointinterval(data = . %>% filter(.width != 0.5), 
                     aes(ymin = .lower, 
                         ymax = .upper), linewidth = 0.5,
                     size = 0.3, alpha = 0.5)  +
  facet_wrap(~nut_plus, nrow = 1) +
  geom_point(data = mod_dat, 
             shape = ".", size = 0.2) +
  scale_fill_colourblind() +
  scale_color_colorblind() +
  guides(color = "none", 
         fill = "none") +
  labs(y = expression(paste("Emergence (number/m"^2,"/d)")),
       x = "Day of Experiment",
       fill = "") +
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  geom_label(data = labels_plot_ind %>% filter(label == "+heat"), aes(label = label,
                                                                           y = .epred),
             color = "white",
                  nudge_x = 3, 
                  nudge_y = 230, 
                  size = 2.5,
             alpha = 0.8) +
  geom_label(data = labels_plot_ind %>% filter(label != "+heat"), aes(label = label),
             color = "white",
                  nudge_x = -3, 
                  nudge_y = 230, 
                  size = 2.5,
             alpha = 0.8) +
  NULL 
)


ggsave(emerge_plot_ind, file = "plots/emerge_plot_ind.jpg", width = 6.5, height = 3)


#4) heat effect derived quantity

heat_effect = emerge_posts_ind %>% 
  ungroup %>% 
  select(.draw, day, .epred, nut_plus, temp_plus) %>% 
  pivot_wider(names_from = temp_plus, values_from = .epred) %>% 
  mutate(heat_effect = `+heat` - `-heat`) %>% 
  group_by(day, nut_plus) %>% 
  median_qi(heat_effect, .width = c(0.5, 0.95)) %>% 
  mutate(color = case_when(heat_effect  > 0 ~ "Higher emergence in\nthe heated treatment",
                           T ~ "Higher emergence in\nthe unheated treatment"))

labels = heat_effect %>% 
  filter(.width == 0.5) %>% 
  filter(nut_plus == "-nutrients") %>% 
  filter(heat_effect == max(heat_effect) | heat_effect == min(heat_effect)) %>% 
  mutate()


heat_effect_plot = heat_effect %>%
  ggplot(aes(x = day, y = heat_effect, color = heat_effect)) +
  geom_pointinterval(data = . %>% filter(.width == 0.5), 
                     aes(ymin = .lower, ymax = .upper), linewidth = 2,
                     size = 0.1) +
  geom_pointinterval(data = . %>% filter(.width != 0.5), 
                     aes(ymin = .lower, ymax = .upper), linewidth = 0.5,
                     size = 0.1)  +
  facet_wrap(~nut_plus) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  guides(color = "none", fill = "none") +
  scale_colour_continuous_divergingx(palette = "cividis") +
  geom_text_repel(data = labels %>% filter(heat_effect == max(heat_effect)), aes(label = color),
                  nudge_x = 10, 
                  nudge_y = 150, 
                  size = 2.1) +
  geom_text_repel(data = labels %>% filter(heat_effect != max(heat_effect)), aes(label = color),
                  nudge_x = -13, 
                  nudge_y = -100, 
                  size = 2.1) +
  labs(x = "Day of Experiment",
       y = "Difference in emergence\n(ind. in heated - ind. in unheated)") +
  theme(text = element_text(size = 8)) +
  NULL


ggsave(heat_effect_plot, file = "plots/heat_effect_plot.jpg", width = 6, height = 3)


# make pub-ready plots

library(patchwork)
library(ggforce)

p1 = emerge_plot_ind + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 1) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p2 = emerge_plot_ind + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 2) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p3 = heat_effect_plot + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 1) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p4 = heat_effect_plot +
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 2) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p1p2 = (p1|p2) + plot_layout(axes = "collect") + plot_annotation(tag_levels = list(c("a)", "b)")),
                                                                 subtitle = "Emergence rates (abundance)")
p3p4 = (p3|p4) + plot_layout(axes = "collect") + plot_annotation(tag_levels = list(c("c)", "d)")),
                                                                 subtitle = "Difference in emerge rates")

ggsave(p1p2, file = "plots/emergence_abundance.jpg", width = 6, height = 3.5)
ggsave(p3p4 + plot_annotation(tag_levels = list(c("a)", "b)")),
                             subtitle = "Difference in emerge rates"),
       file = "plots/emergence_diff.jpg", width = 6, height = 3.5)

library(cowplot)
emerge_abundance_diff = plot_grid(p1p2, p3p4, ncol = 1)

ggsave(emerge_abundance_diff, file = "plots/emerge_abundance_diff.jpg", width = 6, height = 7)


# H2: Smaller adult size --------------------------------------------------

brm_mean_ind_mass_adults_gam = readRDS(file = "models/brm_mean_ind_mass_adults_gam.rds")


raw_mean_ind_mass_data = brm_mean_ind_mass_adults_gam$data2$data %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  mutate(.epred = 1000*mean_individual_mgdw)

mean_ind_mass = mean(raw_mean_ind_mass_data$mean_individual_mgdw, na.rm = T)

mean_ind_mass_posts = tibble(day_s = seq(min(brm_mean_ind_mass_adults_gam$data$day_s),
                                      max(brm_mean_ind_mass_adults_gam$data$day_s),
                                      length.out = 30)) %>% 
  mutate(day = (day_s*sd_day_s) + mean_day_s,
         date = as.Date(day + day_1),
         mean_ind_perday = mean_ind_perday) %>% 
  expand_grid(treatment = unique(brm_mean_ind_mass_adults_gam$data$treatment)) %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  add_epred_draws(brm_mean_ind_mass_adults_gam) %>% 
  mutate(.epred = 1000*(.epred*mean_ind_mass)) %>% 
  group_by(day, heat, nutrients, nut_plus, temp_plus) %>% 
  median_qi(.epred, .width = c(0.5, 0.95))


mean_ind_mass_plot = mean_ind_mass_posts %>% 
  ggplot(aes(x = day, y = .epred, color = heat, fill = heat)) +
  geom_pointinterval(data = . %>% filter(.width == 0.5), 
                     aes(ymin = .lower,
                         ymax = .upper), 
                     linewidth = 2,
                     size = 0.3) +
  geom_pointinterval(data = . %>% filter(.width != 0.5), 
                     aes(ymin = .lower, 
                         ymax = .upper), linewidth = 0.5,
                     size = 0.3, alpha = 0.5) +
  geom_ribbon(data = . %>% filter(.width == 0.95), 
              aes(ymin = .lower, 
                  ymax = .upper), alpha = 0.2, color = NA) +
  facet_wrap(~nut_plus, nrow = 1) +
  geom_point(data = raw_mean_ind_mass_data,
             shape = ".", size = 0.2) +
  scale_fill_colourblind() +
  scale_color_colorblind() +
  guides(color = "none", fill = "none") +
  labs(y = expression(paste("Individual mass (mg dry mass)")),
       x = "Day of Experiment",
       fill = "") +
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ylim(0, NA) +
  # scale_y_log10() +
  NULL 

p1 = emerge_plot_ind +
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 1) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA) 

p2 = emerge_plot_ind + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 2) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p3b = mean_ind_mass_plot + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 1) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), 
              aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p4b = mean_ind_mass_plot + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 2) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), 
              aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p1p2 = (p1|p2) + plot_layout(axes = "collect") + plot_annotation(tag_levels = list(c("a)", "b)")),
                                                                 subtitle = "Emergence rates (abundance)")
p3p4b = (p3b|p4b) + plot_layout(axes = "collect") + plot_annotation(tag_levels = list(c("c)", "d)")),
                                                                 subtitle = "Adult size")

ggsave(p1p2, file = "plots/emergence_abundance.jpg", width = 6, height = 3.5)
ggsave(p3p4b,
       file = "plots/emergence_adult_size.jpg", width = 6, height = 3.5)
saveRDS(p1p2, file = "plots/p1p2.rds")
saveRDS(p3p4b, file = "plots/p3p4b.rds")

library(cowplot)
emerge_abundance_size = plot_grid(p1p2, p3p4b, ncol = 1)

ggsave(emerge_abundance_size, file = "plots/emerge_abundance_size.jpg", width = 6, height = 7)

# H2: Without dragonflies --------------------------------------------------------
# Note, not plotting H1, because numerically dragonflies made up less than 0.01% of all individuals - 5 dragons + 30323 non-dragons:
# sum(emergence$dragons)/sum(emergence$no_ind, na.rm = T)
# brm_mean_ind_mass_adults_gam = readRDS(file = "models/brm_mean_ind_mass_adults_gam.rds")

brm_mean_ind_mass_adults_gam_nodragons = readRDS(file = "models/brm_mean_ind_mass_adults_gam_nodragons.rds")

raw_mean_ind_mass_data_nodragons = brm_mean_ind_mass_adults_gam_nodragons$data2$data %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  mutate(.epred = 1000*mean_individual_mgdw)

mean_ind_mass_nodragons = mean(raw_mean_ind_mass_data_nodragons$mean_individual_mgdw, na.rm = T)

mean_ind_mass_posts_nodragons = tibble(day_s = seq(min(brm_mean_ind_mass_adults_gam_nodragons$data$day_s),
                                         max(brm_mean_ind_mass_adults_gam_nodragons$data$day_s),
                                         length.out = 30)) %>% 
  mutate(day = (day_s*sd_day_s) + mean_day_s,
         date = as.Date(day + day_1),
         mean_ind_perday = mean_ind_perday) %>% 
  expand_grid(treatment = unique(brm_mean_ind_mass_adults_gam_nodragons$data$treatment)) %>% 
  separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
  mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  add_epred_draws(brm_mean_ind_mass_adults_gam_nodragons) %>% 
  mutate(.epred = 1000*(.epred*mean_ind_mass)) %>% 
  group_by(day, heat, nutrients, nut_plus, temp_plus) %>% 
  median_qi(.epred, .width = c(0.5, 0.95))


mean_ind_mass_plot_nodragons = mean_ind_mass_posts_nodragons %>% 
  ggplot(aes(x = day, y = .epred, color = heat, fill = heat)) +
  geom_pointinterval(data = . %>% filter(.width == 0.5), 
                     aes(ymin = .lower,
                         ymax = .upper), 
                     linewidth = 2,
                     size = 0.3) +
  geom_pointinterval(data = . %>% filter(.width != 0.5), 
                     aes(ymin = .lower, 
                         ymax = .upper), linewidth = 0.5,
                     size = 0.3, alpha = 0.5) +
  geom_ribbon(data = . %>% filter(.width == 0.95), 
              aes(ymin = .lower, 
                  ymax = .upper), alpha = 0.2, color = NA) +
  facet_wrap(~nut_plus, nrow = 1) +
  geom_point(data = raw_mean_ind_mass_data_nodragons,
             shape = ".", size = 0.2) +
  scale_fill_colourblind() +
  scale_color_colorblind() +
  guides(color = "none", fill = "none") +
  labs(y = expression(paste("Individual mass (mg dry mass)")),
       x = "Day of Experiment",
       fill = "") +
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ylim(0, NA) +
  # scale_y_log10() +
  NULL 

p3p4b_si = readRDS(file = "plots/p3p4b.rds") + plot_annotation(tag_levels = list(c("a)", "b)")),
                        subtitle = "Adult size - all taxa")

p3nd = mean_ind_mass_plot_nodragons + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 1) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), 
              aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)

p4nd = mean_ind_mass_plot_nodragons + 
  facet_wrap_paginate(~nut_plus, ncol = 1, nrow = 1, page = 2) + 
  geom_ribbon(data = . %>% filter(.width != 0.5), 
              aes(ymin = .lower, ymax = .upper), alpha = 0.1, color = NA)


p3p4nd = (p3nd|p4nd) + plot_layout(axes = "collect") + plot_annotation(tag_levels = list(c("c)", "d)")),
                                                                    subtitle = "Adult size - without dragonflies")
emerge_abundance_size_si_withwithoutdragons = plot_grid(p3p4b_si, p3p4nd, ncol = 1)

ggsave(emerge_abundance_size_si_withwithoutdragons, file = "plots/emerge_abundance_size_si_withwithoutdragons.jpg",
       width = 5, height = 6.5)

# Only dragonflies --------------------------------------------------------
# raw_masses larval
dw_raw = read_csv(file = "data/dw_raw.csv")

dw_raw %>% filter(taxon == "odonata") %>% 
  ggplot(aes(x = treatment, y = dw_mg)) +
  geom_point() 

emergence %>% 
  ggplot(aes(x = day, y = dragons)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0)) + 
  facet_wrap(~treatment)

# raw_masses larval
dw_raw = read_csv(file = "data/dw_raw.csv")

dw_raw %>% filter(taxon == "odonata") %>% 
  ggplot(aes(x = treatment, y = dw_mg)) +
  geom_point() 


