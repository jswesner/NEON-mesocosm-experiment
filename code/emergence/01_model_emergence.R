library(tidyverse)
library(lubridate)
library(ggthemes)
library(janitor)
library(tidybayes)
library(brms)
theme_set(theme_default())

dat_morin_xmins = readRDS(file = "data/dat_morin_xmins.rds") # use this for adding gpp

emergence = readRDS("data/emergence_wrangled.rds") |> 
  mutate(week = as.integer(week(date) - min(week(date)) + 1)) |> 
  left_join(dat_morin_xmins |> ungroup() |> select(tank, contains("gpp")) |> distinct())

dw_raw_adults = read_csv("data/dw_raw_adults.csv") |> 
  left_join(dat_morin_xmins |> ungroup() |> select(tank, contains("gpp")) |> distinct()) |> 
  ungroup() |> 
  mutate(taxa = case_when(taxa == "Odonota" ~ "Odonata", T ~ taxa),
         dw_mg_divmean = dw_mg/mean(dw_mg, na.rm = T))

mean_g_day = mean(emergence$g_day, na.rm = T)
dragon_free_mean_g_day = mean(emergence$dragon_free_g_day, na.rm = T)
mean_day_s = mean(emergence$day)
sd_day_s = sd(emergence$day)
mean_ind_mgdw = mean(emergence$mean_individual_mgdw, na.rm = T)

# emergence %>%
#   ggplot(aes(x = dry_bugs_only_g, y = no_ind)) +
#   geom_point() +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_smooth(method = "lm")
#
# emergence %>%
#   ggplot(aes(x = day_s, y = g_day_s, color = treatment)) +
#   geom_point() +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   geom_line(aes(group = tank)) +
#   facet_wrap(~treatment)
#
# emergence %>%
#   ggplot(aes(x = day_s, y = no_ind_s, color = treatment)) +
#   geom_point() +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   geom_line(aes(group = tank)) +
#   facet_wrap(~treatment)
#
# # model
emerge_smooth = brm(g_day_s ~ temp_treat +
                      t2(day_s, june_22_gpp_s, june_28_gpp_s, by = temp_treat, k = c(5, 5, 5)) +
                      (1 | temp_treat),
    family = Gamma(link = "log"),
    data = emergence,
    prior = c(prior(normal(0, 1), class = "Intercept"),
              prior(normal(0, 1), class = "b"),
              prior(exponential(2), class = "sd")),
    chains = 4, iter = 2000, cores = 4)

saveRDS(emerge_smooth, file = "models/emerge_smooth.rds")

emerge_smooth_ind = brm(no_ind_s ~ temp_treat +
                          t2(day_s, june_22_gpp_s, june_28_gpp_s, by = temp_treat, k = c(5, 5, 5)) +
                          (1 | temp_treat),
    family = Gamma(link = "log"),
    data = emergence,
    prior = c(prior(normal(0, 1), class = "Intercept"),
              prior(normal(0, 1), class = "b"),
              prior(exponential(2), class = "sd")),
    chains = 1, iter = 1000, cores = 4)

saveRDS(emerge_smooth_ind, file = "models/emerge_smooth_ind.rds")

# emerge_smooth = readRDS(file = "models/emerge_smooth.rds")
#
# # check model
# pp_check(emerge_smooth, type = "boxplot")
#
#
# # plot model --------------------------------------------------------------
#
# emerge_dat = emerge_smooth$data %>%
#   separate(treatment, into = c("heat", "nutrients"), remove = F)
#
# emerge_posts = tibble(day_s = seq(min(emerge_smooth$data$day_s),
#                    max(emerge_smooth$data$day_s),
#                    length.out = 30)) %>%
#   mutate(day = (day_s*sd_day_s) + mean_day_s,
#          date = as.Date(day + day_1)) %>%
#   expand_grid(treatment = unique(emerge_smooth$data$treatment)) %>%
#   separate(treatment, into = c("heat", "nutrients"), remove = F) %>%
#   mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
#          nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
#          temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>%
#   add_epred_draws(emerge_smooth) %>%
#   mutate(.epred = (.epred*mean_g_day/0.36))
#
# emerge_plot = emerge_posts %>%
#   ggplot(aes(x = day, y = .epred, fill = heat)) +
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~temp_nut_plus, nrow = 1) +
#   geom_point(data = emergence, aes(y = g_day/0.36, color = heat),
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment") +
#   theme(text = element_text(size = 8)) +
#   NULL
#
# ggsave(emerge_plot, file = "plots/emerge_plot.jpg", width = 6.5, height = 3)
#
#
# # number of emergers
#
# mean_ind = mean(emergence$no_ind, na.rm = T)
#
# emerge_dat = emerge_smooth$data %>%
#   separate(treatment, into = c("heat", "nutrients"), remove = F)
#
# emerge_posts_ind = tibble(day_s = seq(min(emerge_smooth$data$day_s),
#                                   max(emerge_smooth$data$day_s),
#                                   length.out = 30)) %>%
#   mutate(day = (day_s*sd_day_s) + mean_day_s,
#          date = as.Date(day + day_1)) %>%
#   expand_grid(treatment = unique(emerge_smooth$data$treatment)) %>%
#   separate(treatment, into = c("heat", "nutrients"), remove = F) %>%
#   mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
#          nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
#          temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>%
#   add_epred_draws(emerge_smooth_ind) %>%
#   mutate(.epred = (.epred*mean_ind/0.36))
#
# emerge_plot_ind = emerge_posts_ind %>%
#   ggplot(aes(x = day, y = .epred, fill = heat)) +
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~temp_nut_plus, nrow = 1) +
#   geom_point(data = emergence, aes(y = g_day/0.36, color = heat),
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment") +
#   theme(text = element_text(size = 8)) +
#   NULL
# 
# ggsave(emerge_plot, file = "plots/emerge_plot.jpg", width = 6.5, height = 3)
# #
# #
# #
# emerge_plot_heat = emerge_posts %>%
#   filter(temp_plus == "+heat") %>%
#   ggplot(aes(x = day, y = .epred, fill = nutrients)) +
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~nut_plus, nrow = 1) +
#   geom_point(data = emergence %>%
#                filter(temp_plus == "+heat"), aes(y = g_day/0.36, color = nutrients),
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment",
#        subtitle = "+heat",
#        title = "b)") +
#   coord_cartesian(ylim = c(0, 0.8)) +
#   theme(plot.subtitle = element_text(hjust = 0.5),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text = element_text(size = 7)) +
#   NULL
# #
# #
# emerge_plot_noheat = emerge_posts %>%
#   filter(temp_plus == "-heat") %>%
#   ggplot(aes(x = day, y = .epred, fill = nutrients)) +
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~nut_plus, nrow = 1) +
#   geom_point(data = emergence %>%
#                filter(temp_plus == "-heat"), aes(y = g_day/0.36, color = nutrients),
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment",
#        subtitle = "-heat",
#        title = "a)") +
#   coord_cartesian(ylim = c(0, 0.8)) +
#   theme(plot.subtitle = element_text(hjust = 0.5),
#         axis.text = element_text(size = 7)) +
#   NULL
# #
# library(patchwork)
# #
# emerge_plot = emerge_plot_noheat + emerge_plot_heat
# ggsave(emerge_plot, file = "plots/emerge_plot.jpg", width = 6.5, height = 3)
# # 
# 
# # cumulative emergence plot (not great)
# # emerge_posts %>% 
# #   ungroup %>% 
# #   select(-.row, -.chain, -.iteration) %>% 
# #   # filter(.draw == 1) %>% 
# #   # filter(treatment == "ambient_ambient") %>% 
# #   group_by(treatment, heat, nutrients, .draw) %>% 
# #   mutate(.epred = cumsum(.epred)) %>% 
# #   ggplot(aes(x = day_s, y = .epred, fill = treatment)) +
# #   stat_lineribbon() +
# #   facet_wrap(~treatment)
# 
# # total plot
# cumulative_emergence = emerge_posts %>% 
#   group_by(heat, nutrients, .draw, treatment, nut_plus, temp_plus, temp_nut_plus) %>% 
#   reframe(.epred = sum(.epred)) %>% 
#   mutate(blank_facet = "") %>% 
#   mutate(temp_plus = case_when(heat == "heated" ~ "+h", T ~ "-h"),
#          nut_plus = case_when(nutrients == "ambient" ~ "-n", T ~ "+n"),
#          temp_nut_plus_abbr = paste0(temp_plus, " | ", nut_plus)) %>% 
#   ggplot(aes(y = .epred, 
#              x = temp_nut_plus_abbr, 
#              fill = nut_plus)) + 
#   stat_halfeye(size = 0.2, alpha = 0.5) +
#   scale_fill_colorblind() +
#   guides(fill = "none") + 
#   labs(title = "c)",
#        subtitle = "",
#        x = "Treatment",
#        y = "Cumulative emergence (gDM)") +
#   facet_wrap(~blank_facet) +
#   theme(axis.text.x = element_text(size = 7))
# 
# 
# 
# emerge_and_cumulative = emerge_plot + cumulative_emergence + plot_layout(widths = c(0.4, 0.4, 0.4), nrow = 1)
# 
# ggsave(emerge_and_cumulative, file = "plots/emerge_and_cumulative.jpg", width = 6.5, height = 3.3, dpi = 400)
# 
# # date of peak emergence --------------------------------------------------
# 
# emerge_posts %>% 
#   group_by(temp_nut_plus, day) %>% 
#   median_qi(.epred) %>% 
#   group_by(temp_nut_plus) %>% 
#   filter(.epred == max(.epred))
# 
# (max_day = emerge_posts %>% 
#   group_by(heat, day) %>% 
#   median_qi(.epred) %>% 
#   group_by(heat) %>% 
#   filter(.epred == max(.epred))
# )
# 
# heat_effects = emerge_posts %>% 
#   group_by(heat, day, .draw) %>%
#   reframe(median = median(.epred))
# 
# heat_effects %>% 
#   pivot_wider(names_from = heat, values_from = median) %>% 
#   mutate(diff = heated - ambient) %>% 
#   group_by(day) %>%
#   median_qi(diff) %>% 
#   ggplot(aes(x = day, y = diff, ymin = .lower, ymax = .upper)) +
#   stat_pointinterval()
# 
# heat_effects %>% 
#   pivot_wider(names_from = heat, values_from = median) %>% 
#   mutate(diff = heated - ambient) %>% 
#   group_by(day) %>%
#   median_qi(diff) %>% 
#   print(n = Inf)
# 
# emerge_diffs = emerge_posts %>% 
#   group_by(temp_nut_plus, day) %>% 
#   mutate(median = median(.epred)) %>% 
#   group_by(temp_nut_plus) %>% 
#   filter(median == max(median)) %>% 
#   ungroup %>% 
#   select(.draw, treatment, .epred) %>% 
#   pivot_wider(names_from = treatment, values_from = .epred) %>% 
#   mutate(aaha = ambient_ambient - heated_ambient,
#          aaan = ambient_ambient - ambient_nutrient,
#          aahn = ambient_ambient - heated_nutrient,
#          haan = heated_ambient - ambient_nutrient,
#          hahn = heated_ambient - heated_nutrient,
#          anhn = ambient_nutrient - heated_nutrient) %>% 
#   select(-starts_with("ambient"), -starts_with("heated")) %>% 
#   pivot_longer(cols = -.draw) 
#   
# emerge_diffs %>% 
#   group_by(name) %>% 
#   median_qi(value)
# 
# emerge_diffs %>% 
#   group_by(name) %>% 
#   reframe(prob_diff = sum(value < 0)/max(.draw))
# 
# # summarize ---------------------------------------------------------------
# 
# total_emerge = emerge_posts %>% 
#   group_by(heat, nutrients, nut_plus, temp_plus, temp_nut_plus, .draw, treatment) %>% 
#   reframe(.epred = sum(.epred)) %>% 
#   group_by(temp_nut_plus) %>% 
#   median_qi(.epred)
# 
# write_csv(total_emerge, file = "tables/total_emerge.csv")
#           
# mean_emerge = emerge_posts %>% 
#   group_by(nut_plus, temp_plus, .draw, treatment) %>% 
#   reframe(.epred = mean(.epred)) %>% 
#   group_by(nut_plus, temp_plus) %>% 
#   median_qi(.epred)
# 
# write_csv(mean_emerge, file = "tables/mean_emerge.csv")
# 
# 
# emerge_posts %>% 
#   group_by(heat, nutrients, nut_plus, temp_plus, temp_nut_plus, .draw, treatment) %>%
#   reframe(.epred = sum(.epred)) %>% 
#   # group_by(temp_nut_plus) %>% 
#   median_qi(.epred)
# 
# # mean emergence daily increase over the last 10 days 
# emerge_posts %>% 
#   group_by(temp_nut_plus, day) %>% 
#   arrange(temp_nut_plus, day) %>% 
#   group_by(temp_nut_plus, .draw) %>% 
#   mutate(diff = .epred - lag(.epred)) %>% 
#   filter(day >= max(day) - 10) %>% 
#   group_by(temp_plus) %>% 
#   median_qi(diff*1000) # increase in milligrams
# 
# 
# 
# # individual mass ---------------------------------------------------------
# 
# emergence  %>% 
#   ggplot(aes(x = dry_bugs_only_g + 0.0001, y = no_ind + 1)) + 
#   geom_point() +
#   facet_wrap(~temp_nut_plus) + 
#   scale_x_log10() + 
#   scale_y_log10() +
#   geom_smooth(method = lm)
# 
# 
# emergence %>% 
#   ggplot(aes(x = temp_nut_plus, y = mean_individual_mgdw)) + 
#   geom_point() +
#   scale_y_log10() +
#   facet_wrap(~date)


# brm_mean_ind_mass_adults = brm(mean_individual_mgdw_s1 ~ temp_treat*june_22_gpp_s*june_28_gpp_s + 
#                                      (1 + temp_treat*june_22_gpp_s*june_28_gpp_s|week),
#                                    family = Gamma(link = "log"),
                                   # data = emergence %>% filter(mean_individual_mgdw_s1 > 0),
#                                    prior = c(prior(normal(0, 1), class = "Intercept"),
#                                              prior(normal(0, 1), class = "b"),
#                                              prior(exponential(2), class = "sd")),
#                                    chains = 4, iter = 2000, cores = 4)
# 
# saveRDS(brm_mean_ind_mass_adults , file = "models/brm_mean_ind_mass_adults.rds")
# 
# brm_mean_ind_mass_adults = update(readRDS("models/brm_mean_ind_mass_adults.rds"),
#                                                         newdata = emergence %>% filter(mean_individual_mgdw_s1 > 0))
# 
# saveRDS(brm_mean_ind_mass_adults , file = "models/brm_mean_ind_mass_adults.rds")


# brm_mean_ind_mass_adults_individually_measured = brm(dw_mg_divmean ~ temp_treat*june_22_gpp_s*june_28_gpp_s +
#                                                        (1 + temp_treat*june_22_gpp_s*june_28_gpp_s|taxa),
#                                                      family = Gamma(link = "log"),
#                                                      data = dw_raw_adults |> filter(!is.na(dw_mg_divmean)) |>
#                                                        filter(dw_mg_divmean > 0),
#                                                      prior = c(prior(normal(1, 0.5), class = "Intercept"),
#                                                                prior(normal(0, 1), class = "b"),
#                                                                prior(exponential(2), class = "sd")),
#                                                      chains = 4, iter = 2000, cores = 4)
# 
# saveRDS(brm_mean_ind_mass_adults_individually_measured , file = "models/brm_mean_ind_mass_adults_individually_measured.rds")
# 
# #
# brm_mean_ind_mass_adults_individually_measured = update(readRDS("models/brm_mean_ind_mass_adults_individually_measured.rds"),
#                                                         newdata = dw_raw_adults |> filter(!is.na(dw_mg_divmean)) |>
#                                                           filter(dw_mg_divmean > 0))
# 
# saveRDS(brm_mean_ind_mass_adults_individually_measured , file = "models/brm_mean_ind_mass_adults_individually_measured.rds")

# #check model
# pp_check(brm_mean_ind_mass_adults, type = "boxplot")
# 
# posts = brm_mean_ind_mass_adults_individually_measured$data %>%
#   # filter(!is.na(mean_individual_mgdw_s1)) %>%
#   distinct(temp_treat, taxa) |>
#   expand_grid(june_22_gpp_s = round(quantile(d$june_22_gpp_s,
#                                              probs = c(0.25, 0.5, 0.75)), 1)) |>
#   expand_grid(june_28_gpp_s = round(quantile(d$june_28_gpp_s,
#                                              probs = c(0.25, 0.5, 0.75)), 1)) |>
#   mutate(gpp_cat22 = case_when(june_22_gpp_s == min(june_22_gpp_s) ~ "Low GPP",
#                                june_22_gpp_s == max(june_22_gpp_s) ~ "High GPP",
#                                T ~ "Medium GPP")) |>
#   mutate(gpp_cat28 = case_when(june_28_gpp_s == min(june_28_gpp_s) ~ "Low GPP",
#                                june_28_gpp_s == max(june_28_gpp_s) ~ "High GPP",
#                                T ~ "Medium GPP")) |>
#   mutate(gpp_cat = case_when(gpp_cat22 == "Low GPP" & gpp_cat28 == "Low GPP" ~ "a) Low GPP",
#                              gpp_cat22 == "High GPP" & gpp_cat28 == "High GPP" ~ "c) High GPP",
#                              gpp_cat22 == "Medium GPP" & gpp_cat22 == "Medium GPP" ~ "b) Medium GPP")) |>
#   filter(!is.na(gpp_cat)) |>
#   add_epred_draws(brm_mean_ind_mass_adults_individually_measured, re_formula = NULL)
# 
# posts |>
#   ggplot(aes(x = interaction(temp_treat, gpp_cat),
#              y = .epred)) +
#   stat_pointinterval(aes(color = temp_treat)) +
#   facet_wrap(~taxa) +
#   scale_y_log10() +
#   NULL
# 
# 
# # Horne et al. 2015 estimate that each degree C generates a 2.9% decline in mean freshwater invertebrate mass.
# # Let's check that against the emergence data
# emerge_individual_diffs = posts %>% 
#   ungroup %>% 
#   mutate(treatment = paste(heat, nutrients, sep = "_")) %>% 
#   select(treatment, date, .epred, .draw) %>% 
#   pivot_wider(names_from = treatment, values_from = .epred) %>% 
#   mutate(aaha = (ambient_ambient - heated_ambient)/ambient_ambient,
#          aaan = (ambient_ambient - ambient_nutrient)/ambient_ambient,
#          aahn = (ambient_ambient - heated_nutrient)/ambient_ambient,
#          haan = (heated_ambient - ambient_nutrient)/heated_ambient,
#          hahn = (heated_ambient - heated_nutrient)/heated_ambient,
#          anhn = (ambient_nutrient - heated_nutrient)/ambient_nutrient) %>% 
#   select(-starts_with("ambient"), -starts_with("heated")) %>% 
#   pivot_longer(cols = c(-.draw, -date))  
# 
# emerge_individual_diffs_summarize_across = posts %>% 
#   ungroup %>%  
#   group_by(heat, date, .draw) %>% 
#   reframe(.epred = median(.epred)) %>% 
#   select(heat, date, .epred, .draw) %>% 
#   pivot_wider(names_from = heat, values_from = .epred) %>% 
#   mutate(diff = (heated - ambient)/heated) %>% 
#   select(-starts_with("ambient"), -starts_with("heated")) %>% 
#   pivot_longer(cols = c(-.draw, -date))  
# 
# emerge_individual_diffs_summarize_across %>% 
#   group_by(name, date) %>% 
#   ggplot(aes(x = date, y = value*100)) + 
#   # geom_pointrange(aes(color = name)) +
#   stat_halfeye() +
#   geom_hline(aes(yintercept = 0))
# 
# posts %>% 
#   ungroup %>%  
#   group_by(temp_plus, date, .draw) %>% 
#   reframe(.epred = median(.epred)) %>% 
#   ggplot(aes(x = date, y = .epred)) + 
#   stat_halfeye(aes(fill = temp_plus)) +
#   scale_fill_colorblind()
# 
# 
# # refit without dragons ---------------------------------------------------
# 
# emerge_smooth = readRDS(file = "models/emerge_smooth.rds")
# 
# emerge_smooth_nodragons = update(emerge_smooth, newdata = emergence,
#                                  formula = dragon_free_g_day_s ~ .)
# 
# saveRDS(emerge_smooth_nodragons, file = "models/emerge_smooth_nodragons.rds")
# 
# 
# brm_mean_ind_mass_adults_gam_nodragons = update(readRDS(file = "models/brm_mean_ind_mass_adults_gam.rds"),
#                                                 newdata = emergence %>% filter(mean_individual_mgdw_nodragons_s1>0),
#                                                 formula = mean_individual_mgdw_nodragons_s1 ~ .)
# saveRDS(brm_mean_ind_mass_adults_gam_nodragons, file = "models/brm_mean_ind_mass_adults_gam_nodragons.rds")
# 
# 
# # plot dragon-free model --------------------------------------------------------------
# 
# emerge_nodragons_dat = emerge_smooth_nodragons$data %>% 
#   separate(treatment, into = c("heat", "nutrients"), remove = F)
# 
# emerge_nodragons_posts = tibble(day_s = seq(min(emerge_smooth_nodragons$data$day_s),
#                                   max(emerge_smooth_nodragons$data$day_s),
#                                   length.out = 30)) %>% 
#   mutate(day = (day_s*sd_day_s) + mean_day_s,
#          date = as.Date(day + day_1)) %>% 
#   expand_grid(treatment = unique(emerge_smooth_nodragons$data$treatment)) %>% 
#   separate(treatment, into = c("heat", "nutrients"), remove = F) %>% 
#   mutate(temp_plus = case_when(heat == "heated" ~ "+heat", T ~ "-heat"),
#          nut_plus = case_when(nutrients == "ambient" ~ "-nutrients", T ~ "+nutrients"),
#          temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
#   add_epred_draws(emerge_smooth_nodragons) %>% 
#   mutate(.epred = (.epred*mean_g_day/0.36))
# 
# emerge_nodragons_plot = emerge_nodragons_posts %>% 
#   ggplot(aes(x = day, y = .epred, fill = heat)) + 
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~temp_nut_plus, nrow = 1) +
#   geom_point(data = emergence, aes(y = dragon_free_g_day/0.36, color = heat), 
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment") +
#   theme(text = element_text(size = 8)) +
#   NULL 
# 
# ggsave(emerge_nodragons_plot, file = "plots/emerge_nodragons_plot.jpg", width = 6.5, height = 3)
# 
# 
# emerge_plot_heat = emerge_posts %>%
#   filter(temp_plus == "+heat") %>% 
#   ggplot(aes(x = day, y = .epred, fill = nutrients)) + 
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~nut_plus, nrow = 1) +
#   geom_point(data = emergence %>%
#                filter(temp_plus == "+heat"), aes(y = g_day/0.36, color = nutrients), 
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment",
#        subtitle = "+heat",
#        title = "b)") +
#   coord_cartesian(ylim = c(0, 0.8)) +
#   theme(plot.subtitle = element_text(hjust = 0.5),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text = element_text(size = 7)) +
#   NULL 
# 
# 
# emerge_plot_noheat = emerge_posts %>%
#   filter(temp_plus == "-heat") %>% 
#   ggplot(aes(x = day, y = .epred, fill = nutrients)) + 
#   stat_lineribbon(.width = 0.95, alpha = 0.4, linewidth = 0.5) +
#   facet_wrap(~nut_plus, nrow = 1) +
#   geom_point(data = emergence %>%
#                filter(temp_plus == "-heat"), aes(y = g_day/0.36, color = nutrients), 
#              shape = ".", size = 0.2) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   guides(fill = "none", color = "none") +
#   labs(y = expression(paste("Emergence (gDM/m"^2,"/d)")),
#        x = "Day of Experiment",
#        subtitle = "-heat",
#        title = "a)") +
#   coord_cartesian(ylim = c(0, 0.8)) +
#   theme(plot.subtitle = element_text(hjust = 0.5),
#         axis.text = element_text(size = 7)) +
#   NULL 
# 
# library(patchwork)
# 
# emerge_plot = emerge_plot_noheat + emerge_plot_heat  
# ggsave(emerge_plot, file = "plots/emerge_plot.jpg", width = 6.5, height = 3)
# 
# 
# # cumulative emergence plot (not great)
# # emerge_posts %>% 
# #   ungroup %>% 
# #   select(-.row, -.chain, -.iteration) %>% 
# #   # filter(.draw == 1) %>% 
# #   # filter(treatment == "ambient_ambient") %>% 
# #   group_by(treatment, heat, nutrients, .draw) %>% 
# #   mutate(.epred = cumsum(.epred)) %>% 
# #   ggplot(aes(x = day_s, y = .epred, fill = treatment)) +
# #   stat_lineribbon() +
# #   facet_wrap(~treatment)
# 
# # total plot
# cumulative_emergence = emerge_posts %>% 
#   group_by(heat, nutrients, .draw, treatment, nut_plus, temp_plus, temp_nut_plus) %>% 
#   reframe(.epred = sum(.epred)) %>% 
#   mutate(blank_facet = "") %>% 
#   mutate(temp_plus = case_when(heat == "heated" ~ "+h", T ~ "-h"),
#          nut_plus = case_when(nutrients == "ambient" ~ "-n", T ~ "+n"),
#          temp_nut_plus_abbr = paste0(temp_plus, " | ", nut_plus)) %>% 
#   ggplot(aes(y = .epred, 
#              x = temp_nut_plus_abbr, 
#              fill = nut_plus)) + 
#   stat_halfeye(size = 0.2, alpha = 0.5) +
#   scale_fill_colorblind() +
#   guides(fill = "none") + 
#   labs(title = "c)",
#        subtitle = "",
#        x = "Treatment",
#        y = "Cumulative emergence (gDM)") +
#   facet_wrap(~blank_facet) +
#   theme(axis.text.x = element_text(size = 7))
# 
# 
# 
# emerge_and_cumulative = emerge_plot + cumulative_emergence + plot_layout(widths = c(0.4, 0.4, 0.4), nrow = 1)
# 
# ggsave(emerge_and_cumulative, file = "plots/emerge_and_cumulative.jpg", width = 6.5, height = 3.3, dpi = 400)
# 
