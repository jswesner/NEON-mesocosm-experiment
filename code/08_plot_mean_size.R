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
         mean_dw_mg = mean(dw_mg, na.rm = T),
         dw_mg_divmean = dw_mg/mean_dw_mg)

mean_g_day = mean(emergence$g_day, na.rm = T)
dragon_free_mean_g_day = mean(emergence$dragon_free_g_day, na.rm = T)
mean_day_s = mean(emergence$day)
sd_day_s = sd(emergence$day)
mean_ind_mgdw = mean(emergence$mean_individual_mgdw, na.rm = T)

# mod = readRDS("models/brm_mean_ind_mass_adults.rds")
mod = readRDS("models/brm_mean_ind_mass_adults_individually_measured.rds")

posts = mod$data %>%
  # filter(!is.na(mean_individual_mgdw_s1)) %>%
  distinct(temp_treat, taxa, june_22_gpp_s, june_28_gpp_s) |>
  # expand_grid(june_22_gpp_s = c(-1, 0, 1)) |>
  # mutate(june_28_gpp_s = june_22_gpp_s) |> 
  # mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
  # mutate(gpp_cat = case_when(gpp_sum < 0 ~ "a) Low GPP",
  #                            gpp_sum > 0 ~ "c) High GPP",
  #                            T ~ "b) Medium GPP")) |>
  add_epred_draws(mod, re_formula = NULL) |> 
  group_by(.draw, temp_treat, taxa) |> 
  reframe(.epred = mean(.epred)) |> 
  mutate(.epred = .epred*unique(dw_raw_adults$mean_dw_mg))

posts |>
  ggplot(aes(x = temp_treat,
             y = .epred)) +
  stat_pointinterval(aes(color = temp_treat)) +
  geom_line(data = . %>% filter(.draw <= 300),
            aes(group = .draw),
            alpha = 0.2,
            linewidth = 0.05) +
  # stat_halfeye(aes(fill = temp_treat)) +
  facet_wrap(~taxa, scales = "free_y") +
  scale_y_log10() +
  scale_color_colorblind() +
  guides(color = "none") +
  NULL




# adult size over time ----------------------------------------------------

mod = readRDS("models/brm_mean_ind_mass_adults.rds")

posts = mod$data %>%
  # filter(!is.na(mean_individual_mgdw_s1)) %>%
  distinct(temp_treat, week, june_22_gpp_s, june_28_gpp_s) |>
  # expand_grid(june_22_gpp_s = c(-1, 0, 1)) |>
  # mutate(june_28_gpp_s = june_22_gpp_s) |> 
  # mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
  # mutate(gpp_cat = case_when(gpp_sum < 0 ~ "a) Low GPP",
  #                            gpp_sum > 0 ~ "c) High GPP",
  #                            T ~ "b) Medium GPP")) |>
  add_epred_draws(mod, re_formula = NULL) |> 
  mutate(.epred = .epred*unique(dw_raw_adults$mean_dw_mg)) |> 
  group_by(.draw, temp_treat, week) |> 
  reframe(.epred = mean(.epred))

posts |>
  ggplot(aes(x = week,
             y = .epred)) +
  stat_pointinterval(aes(color = temp_treat)) +
  # facet_wrap(~gpp_cat, scales = "free_y") +
  scale_y_log10() +
  scale_color_colorblind() +
  guides(color = "none") +
  NULL

