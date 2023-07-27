# paretocounts <- custom_family(
#   "paretocounts", dpars = c("mu"),
#   links = c("identity"),
#   lb = -Inf, ub = Inf,
#   type = "real", vars = c("vreal1[n]", 
#                           "vreal2[n]",
#                           "vreal3[n]"))
# 
# stan_funs <- "
# real paretocounts_lpdf(real Y, real mu, real vreal1, real vreal2, real vreal3){
#     if(mu != -1)
#     return(vreal1*(log((mu+1) / ( vreal3^(mu+1) - vreal2^(mu+1))) + mu*log(Y)));
#     else
#     return(vreal1*(log(log(vreal2) - log(vreal3)) + mu*log(Y)));
# }
# "
# stanvars <- stanvar(scode = stan_funs, block = "functions")

source("code/brm_distributions.R")


library(readxl)
library(rstan)
library(tidyverse)
library(brms)
library(tidybayes)

macro_lw_coeffs <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/neon_size_spectra/data/raw_data/inverts/macro_lw_coeffs.csv") %>% 
  pivot_longer(cols = c(subphylum, class, order, family, genus, taxon),
               names_to = "group",
               values_to = "taxon") %>% 
  group_by(taxon, formula) %>% 
  summarize(a = mean(a),
            b = mean(b)) %>% 
  filter(formula == "M = aL^b") %>% 
  bind_rows(tibble(taxon = "Amphipod",
                   a = 0.0058,
                   b = 3.015))

o2 = read_csv("data/o2_temp.csv")

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(temp_treat, "_",nutrient_treat))


gpp_dates = o2 %>% 
  mutate(date = as.Date(date_time),
         time = hour(date_time), 
         week = week(date_time)) %>% 
  group_by(week) %>% 
  filter(date == min(date)) %>% 
  distinct(date, week) %>% 
  ungroup %>% 
  mutate(time_collected = as.factor(c(1,2))) %>% 
  select(-week)

gpp_list = bind_rows(readRDS("data/gppposteriors.rds"))

gpp = gpp_list$summary %>% distinct() %>% 
  separate(tank, into = c("tank", "time_collected")) %>% 
  mutate(tank = parse_number(tank)) %>% 
  as_tibble() %>% 
  left_join(gpp_dates) %>% 
  mutate(time_collected = paste("gpp", time_collected, sep = "_")) %>% 
  select(tank, time_collected, GPP) %>% 
  pivot_wider(names_from = time_collected, values_from = GPP)

temperature <- read_csv("data/temperature.csv") %>% 
  mutate(temp_deg_c = (temp_deg_f - 32)*(5/9),
         date_time = parse_date_time(paste(date, time), orders = "Ymd HMS")) %>% 
  select(-heater) %>% 
  left_join(treatments) %>% 
  mutate(temp_deg_ysi = case_when(is.na(temp_from_ysi_deg_c) ~ temp_deg_c, TRUE ~ temp_from_ysi_deg_c)) 


mean_temp = temperature %>% 
  group_by(tank, treatment) %>% 
  summarize(mean_temp = median(temp_deg_c, na.rm = T),
            sd_temp = sd(temp_deg_c, na.rm = T)) %>% 
  left_join(gpp) %>% 
  separate(treatment, into = c("heated", "nutrients"))


taxon_fixes = read_csv("data/taxon_fixes.csv")

body_size_data <- rio::import_list("data/body size data.xlsx", setclass = "tbl") %>% 
  bind_rows() %>% 
  left_join(mean_temp) %>% 
  left_join(taxon_fixes) %>% 
  mutate(original_taxon = taxon) %>% 
  mutate(taxon_stage = paste(correct_taxon, correct_stage, sep = "_")) %>% 
  select(-taxon) %>% 
  mutate(taxon = correct_taxon) %>% 
  select(-correct_taxon) %>% 
  replace_na(list(correct_stage = "unknown"))

dw = body_size_data %>% 
  filter(correct_stage != "adult") %>% 
  mutate(taxon = str_to_sentence(taxon)) %>% 
  left_join(macro_lw_coeffs) %>% 
  mutate(dw = a*length_mm^b) %>% 
  filter(dw >= 0.0003) %>% 
  filter(length_mm < 200) %>% # filters some typos
  filter(length_mm > 1) %>% # clear undersampling below 1 mm
  filter(dw < 200) %>% 
  group_by(tank) %>%
  mutate(xmin = min(dw, na.rm = T),
         xmax = max(dw, na.rm = T),
         counts = 1) %>% 
  group_by(tank) %>% 
  mutate(sample = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(grandmean_temp = mean(mean_temp),
         grandsd_temp = sd(mean_temp)) %>% 
  mutate(mat_s = (mean_temp - grandmean_temp)/grandsd_temp,
         gpp_1_s = (gpp_1 - mean(gpp_1))/sd(gpp_1),
         gpp_2_s = (gpp_2 - mean(gpp_2))/sd(gpp_2))


dw_short = dw %>% filter(sample <= 1) %>% 
  mutate(counts = as.integer(counts))


fit2 <- brm(
  dw | vreal(counts, xmin, xmax) ~ 1, 
  data = dw_short,
  family = paretocounts, 
  stanvars = stanvars,
  chains = 1, iter = 100,
  prior = c(prior(normal(-1.3, 0.1), class = "Intercept"))
)

predict(fit2)
fitted(fit2)
conditional_effects(fit2, effects = "mat_s")

fit2$data %>% 
  add_epred_draws(fit2) 
