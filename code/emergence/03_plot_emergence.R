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

mod = readRDS(file = "models/emerge_smooth.rds")

posts_emerge = emerge_smooth$data |> 
  distinct(temp_treat)|> 
  expand_grid(june_22_gpp_s = c(-1, 0, 1)) |>
  mutate(june_28_gpp_s = june_22_gpp_s) |> 
  mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
  mutate(gpp_cat = case_when(gpp_sum < 0 ~ "a) Low GPP",
                             gpp_sum > 0 ~ "c) High GPP",
                             T ~ "b) Medium GPP")) |> 
  expand_grid(day_s = seq(-2, 2, length.out = 30)) |> 
  add_epred_draws(mod, re_formula = NULL) 


posts_emerge |> 
  ggplot(aes(x = day_s, y = .epred)) +
  stat_lineribbon(aes(fill = temp_treat), 
                  color = "white") +
  scale_y_log10() +
  facet_wrap(temp_treat~gpp_cat)

# individuals -------------------------------------------------------------

mod = readRDS(file = "models/emerge_smooth_ind.rds")

posts_emerge_ind = emerge_smooth_ind$data |> 
  distinct(temp_treat)|> 
  expand_grid(june_22_gpp_s = c(-1, 0, 1)) |>
  mutate(june_28_gpp_s = june_22_gpp_s) |> 
  mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
  mutate(gpp_cat = case_when(gpp_sum < 0 ~ "a) Low GPP",
                             gpp_sum > 0 ~ "c) High GPP",
                             T ~ "b) Medium GPP")) |> 
  expand_grid(day_s = seq(-2, 2, length.out = 30)) |> 
  add_epred_draws(mod, re_formula = NULL) 


posts_emerge_ind |> 
  ggplot(aes(x = day_s, y = .epred)) +
  stat_lineribbon(aes(fill = temp_treat), 
                  color = "white") +
  scale_y_log10() +
  facet_wrap(temp_treat~gpp_cat)



