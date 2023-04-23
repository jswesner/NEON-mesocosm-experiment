library(readxl)
library(rstan)
library(tidyverse)

# lw_functions
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

temperature <- read_csv("data/temperature.csv") %>% 
  mutate(temp_deg_c = (temp_deg_f - 32)*(5/9),
         date_time = parse_date_time(paste(date, time), orders = "Ymd HMS")) %>% 
  select(-heater) %>% 
  left_join(treatments) %>% 
  mutate(temp_deg_ysi = case_when(is.na(temp_from_ysi_deg_c) ~ temp_deg_c, TRUE ~ temp_from_ysi_deg_c)) 


mean_temp = temperature %>% 
  group_by(tank) %>% 
  summarize(mean_temp = median(temp_deg_c, na.rm = T),
            sd_temp = sd(temp_deg_c, na.rm = T))

body_size_data <- import_list("data/body size data.xlsx", setclass = "tbl") %>% 
  bind_rows() %>% 
  mutate(taxon = case_when(taxon == "Chironomidae (pupa)" ~ "Chironomidae",
                           taxon == "Coleptera" ~ "Coleoptera",
                           TRUE ~ taxon)) %>% 
  left_join(mean_temp)

body_size_data %>% 
  group_by(tank, mean_temp, name) %>% 
  filter(length_mm <= 1000) %>% 
  summarize(gm_size = exp(mean(log(length_mm))),
            sd_gmsize = exp(sd(log(length_mm))))  %>% 
  ggplot(aes(x = mean_temp, y = gm_size, color = name)) + 
  # geom_point() + 
  geom_pointrange(aes(ymin = gm_size - sd_gmsize, ymax = gm_size + sd_gmsize))

dw = body_size_data %>% 
  mutate(taxon = str_to_sentence(taxon)) %>% 
  left_join(macro_lw_coeffs) %>% 
  mutate(dw = a*length_mm^b) %>% 
  filter(dw >= 0.0003) %>% 
  group_by(tank) %>%
  mutate(xmin = min(dw, na.rm = T),
         xmax = max(dw, na.rm = T),
         counts = 1)


# model
stan_mod = stan_model("models/b_paretocounts_singlesample.stan")

sim_data = dw %>% 
  filter(tank == 4) %>% 
  filter(!is.na(dw))

stan_dat <- list(x = sim_data$dw,
                 N = nrow(sim_data),
                 counts = sim_data$counts,
                 xmax = sim_data$xmax,
                 xmin = sim_data$xmin)

fit <- sampling(object = stan_mod,
                       data = stan_dat,
                       iter = 1000,
                       cores = 2,
                       chains = 2,
                       open_progress = F,
                       verbose = F)

fit

library(sizeSpectra)

MLE.plot(x = unique(sim_data$dw),
         b = -1.04,
         confVals = c(-1.09, -.98))

